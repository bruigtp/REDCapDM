#' Read REDCap data
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Import REDCap data into R either from REDCap's exported R file or directly via the REDCap API. The function returns a list with the dataset, the project dictionary (metadata) and, for longitudinal projects, the instrument–event mapping (`event_form`) when available.
#'
#' @details
#' Two import modes are supported:
#' * **Exported files** — use `data_path` (REDCap R export) and `dic_path` (dictionary CSV/XLSX).
#' * **API** — use `uri` and `token` to pull data and metadata directly from REDCap.
#'
#' If the project is longitudinal, provide `event_path` (instrument–event mapping)
#' or the function will attempt to fetch it from the API when using API mode.
#'
#' With the API, the values and their labels are read with two requests. The record identifiers are read as text in
#' both of them (long identifiers such as `9007199254740993`, or `100000000000000`, are not changed), and the
#' identifier is a number only when its text is that number, as with the exported files. The labels of the events
#' (`redcap_event_name.factor`), repeated instruments and data access groups are given to each row by its code (the
#' unique event name, instrument or data access group), and they are checked with the metadata of the project (its
#' events, arms, instruments and data access groups): if the records changed between the two requests, the labels are
#' read again, and the function stops if they keep changing. When that metadata can't be read, those columns have
#' the codes instead of the labels, with a warning.
#'
#' **Steps for using exported data in REDCap:**
#' 1. Use the REDCap *Export Data* function and choose *R Statistical Software* format.
#' 2. REDCap generates:
#'    - A CSV file with observations.
#'    - An R script to format variables for import.
#' 3. Ensure the exported files, dictionary, and event mapping (if any) are in the same directory.
#'
#'
#' @note To use other package functions effectively, include the `dic_path` argument to load the project dictionary.
#'
#' @param data_path Path to exported R file (use with `dic_path`).
#' @param dic_path Path to the dictionary file (CSV or XLSX; use with `data_path`)..
#' @param event_path Path to the event-form mapping file (CSV or XLSX) for longitudinal projects (downloadable via the `Designate Instruments for My Events` tab within the `Project Setup` section of REDCap).
#' @param uri REDCap API base URI (use with `token`).
#' @param token REDCap API token (use with `uri`).
#' @param sep Character string specifying the field separator for the exported dictionary CSV file. Default `","`.
#' @param filter_field Optional character vector of field names to request from the API. It must include the record
#'   identifier (the first field of the project, e.g. `record_id`).
#' @param survey_fields Logical; include survey-related fields when pulling via API. Default `FALSE`.
#' @param rm_mdc Logical; Removes the Missing Data Codes from the data. Default `TRUE`. With `FALSE`, the codes are kept: in multiple-choice fields as an additional category (in the `.factor` version), and in validated text fields (numbers, dates, ...) in a new `<field>_mdc` column, while the field itself keeps its type and has `NA` in those rows.
#' @param mdc Optional Missing Data Codes of the project, besides the default ones of REDCap (`NI`, `UNK`, `NASK`, ...), which are always
#'   used: a vector of codes, e.g. `c("-999", "-888")`. A code can have a label, written as `code = label`
#'   (e.g. `c("-999" = "Not applicable", "-888")`): with `rm_mdc = FALSE`, the `.factor` version of the multiple-choice fields shows the
#'   label instead of the code. Numeric codes are also found in numeric fields, also when written in another way (e.g. `-999.0`
#'   for `-999`). With the API, the codes defined in the project
#'   (Project Setup > Additional customizations > Missing Data Codes) are added automatically. As with the default codes, they are
#'   searched in the validated text fields (numbers, dates, ...), the multiple-choice fields and the checkboxes, so they must not be values
#'   that the fields can take (e.g. the code of an option).
#'
#' @return A list with:
#'   - `data`: Imported dataset.
#'   - `dictionary`: Variable dictionary (project meta data).
#'   - `event_form`: Event-form mapping for longitudinal projects (if applicable).
#'
#' @note
#' * Use either exported-files mode (`data_path` + `dic_path`) **or** API mode (`uri` + `token`) — not both.
#' * For exported files, REDCap's R export is required for `data_path`. Dictionary and event files must be CSV or XLSX.
#'
#' @examples
#' \dontrun{
#' # From exported files
#' out <- redcap_data(
#'   data_path = "project_export.r",
#'   dic_path  = "project_dictionary.csv",
#'   event_path = "instrument_event_map.csv"
#' )
#'
#' # From API
#' out_api <- redcap_data(
#'   uri   = "https://redcap.example.org/api/",
#'   token = "REPLACE_WITH_TOKEN"
#' )
#' }
#'
#' @export
#' @importFrom stats setNames
#' @importFrom utils head

redcap_data <- function(data_path = NA, dic_path = NA, event_path = NA, uri = NA, token = NA, sep = ",", filter_field = NULL, survey_fields = FALSE, rm_mdc = TRUE, mdc = NULL) {

  event_form <- NULL
  api_mdc <- character(0)
  
  # Error: invalid arguments
  for (arg in c("data_path", "dic_path", "event_path", "uri", "token")) {
    check_string(get(arg), arg, na_ok = TRUE)
  }
  if (!is.character(sep) || length(sep) != 1 || is.na(sep) || nchar(sep) != 1) {
    rd_abort("{.arg sep} must be a single character, e.g. {.val ,} or {.val ;}.", class = "REDCapDM_error_argument")
  }
  check_strings(filter_field, "filter_field")

  # A path copied with "Copy as path" in Windows comes between quotes (e.g. '"C:\\Users\\...\\project.r"'): the quotes are not part
  # of the path (Windows doesn't allow quotes in file names)
  unquote <- function(path) if (is.na(path)) path else sub("^\\s*([\"'])(.*)\\1\\s*$", "\\2", path)
  data_path <- unquote(data_path)
  dic_path <- unquote(dic_path)
  event_path <- unquote(event_path)

  # Save the current working directory and ensure it is restored on exit
  oldwd <- getwd()
  on.exit(setwd(oldwd))

  # Warning: data_path, dic_path and another argument are specified.
  if (all(!c(data_path, dic_path) %in% NA) && any(!c(token, uri) %in% NA)) {
    rd_abort(c("Too many arguments.", "i" = "Use {.arg data_path} and {.arg dic_path} for exported data or {.arg uri} and {.arg token} for API connection."), class = "REDCapDM_error_argument")
  }

  # Warning: token, uri and another argument are specified.
  if (all(!c(token, uri) %in% NA) && any(!c(data_path, dic_path) %in% NA)) {
    rd_abort(c("Too many arguments.", "i" = "Use {.arg uri} and {.arg token} for API connection or {.arg data_path} and {.arg dic_path} for exported data."), class = "REDCapDM_error_argument")
  }

  # Warning: either data_path or dic_path is specified alone
  if ((!is.na(data_path) && is.na(dic_path)) || (is.na(data_path) && !is.na(dic_path))) {
    rd_abort("Both {.arg data_path} and {.arg dic_path} are required for exported data.", class = "REDCapDM_error_argument")
  }

  # Warning: either uri or token is specified alone
  if ((!is.na(uri) && is.na(token)) || (is.na(uri) && !is.na(token))) {
    rd_abort("Both {.arg uri} and {.arg token} are required for API connection.", class = "REDCapDM_error_argument")
  }
  
  # Error: no data source
  if (all(c(data_path, dic_path, uri, token) %in% NA)) {
    rd_abort(c("No data to read.", "i" = "Use {.arg data_path} and {.arg dic_path} for exported data or {.arg uri} and {.arg token} for API connection."), class = "REDCapDM_error_argument")
  }

  # Error: logical arguments
  for (arg in c("survey_fields", "rm_mdc")) {
    check_flag(get(arg), arg)
  }

  # Error: Missing Data Codes given by the user (codes and their labels)
  mdc <- mdc_table(mdc)

  # Process data from exported files
  if (all(!c(data_path, dic_path) %in% NA) && all(c(token, uri) %in% NA)) {
    # Check the extension of the R data file
    if (!grepl("\\.R$", data_path) && !grepl("\\.r$", data_path)) {
      rd_abort(c("Unsupported file format. {.arg data_path} must be an R file exported from REDCap.", "x" = "It is {.file {data_path}}."), class = "REDCapDM_error_file")
    }
    
    # Error: the files do not exist
    paths <- c(data_path = data_path, dic_path = dic_path, event_path = event_path)
    not_found <- paths[!is.na(paths) & !file.exists(paths)]
    if (length(not_found) > 0) {
      not_found_args <- names(not_found)
      rd_abort(
        c(
          "File not found: {.arg {not_found_args}}.",
          stats::setNames(sprintf("{.arg {not_found_args[%d]}} = {.file {not_found[%d]}}", seq_along(not_found), seq_along(not_found)), rep("x", length(not_found))),
          "i" = "Relative paths start at the working directory: {.path {getwd()}}."
        ),
        class = "REDCapDM_error_file"
      )
    }

    # Import the data using the R file
    tmp_env <- new.env()
    file.lines <- scan(data_path, what = character(), skip = 2, sep = "\n", quiet = TRUE)

    # The lines of the script that would change the user's session are not run. The `label()` and `units()` of the script are defined here instead, with the same result.
    file.lines <- file.lines[!grepl("^\\s*(graphics\\.off\\(\\)|library\\(Hmisc\\)|rm\\(list\\s*=\\s*ls\\(\\)\\))\\s*;?\\s*$", file.lines)]
    tmp_env$`label<-` <- function(x, value) {
      attr(x, "label") <- value
      x
    }
    tmp_env$`units<-` <- function(x, value) {
      attr(x, "units") <- value
      x
    }
    # REDCap writes the options of a variable without options (e.g. the repeated instrument of a project that only repeats
    # events) as `c("", "")`, without names: `factor(x, levels = names(...), labels = ...)` fails in R because the levels
    # are NULL. That factor is created without options; the other calls are run by base::factor() as they are.
    tmp_env$factor <- function(x = character(), levels, labels = levels, ...) {
      if (!missing(levels) && is.null(levels)) {
        return(base::factor(x))
      }
      call <- sys.call()
      call[[1]] <- quote(base::factor)
      eval(call, parent.frame())
    }

    # Error: the csv file that the R script reads must be in the same folder
    csv_file <- stringr::str_match(file.lines, "read\\.csv\\(['\"]([^'\"]+)['\"]")[, 2]
    csv_file <- csv_file[!is.na(csv_file)]
    if (length(csv_file) > 0 && !file.exists(file.path(dirname(data_path), csv_file[1]))) {
      rd_abort(
        c("The R script reads the data from {.file {csv_file[1]}}, which must be in the same folder as {.arg data_path}.", "i" = "Folder of {.arg data_path}: {.path {dirname(data_path)}}."),
        class = "REDCapDM_error_file"
      )
    }
    file.lines.collapsed <- paste(file.lines, collapse = "\n")
    command <- paste0("dirname(parent.frame(2)$", "data_path", ")")
    setwd(eval(parse(text = command)))

    # Error: the R script can't be run (e.g. it was edited)
    tryCatch(
      source(textConnection(file.lines.collapsed), local = tmp_env, encoding = "UTF-8"),
      error = function(e) {
        rd_abort(
          c("The R script in {.arg data_path} could not be run.", "i" = "Use the R script exported by REDCap without editing it."),
          class = "REDCapDM_error_file", parent = e, call = NULL
        )
      }
    )
    data <- get("data", envir = tmp_env)

    # Saving labels before changes
    labels <- purrr::map_chr(data, function(x) {
      lab <- attr(x, "label")
      if (!is.null(lab)) {
        lab
      } else {
        ""
      }
    })

    # Ensure the primary identifier column is correctly named (and keep its label)
    if (names(data)[1] != "record_id") {
      names(data)[1] <- "record_id"
      names(labels)[1] <- "record_id"
    }

    # Load the dictionary and validate its format
    setwd(oldwd)
    extension_dic <- tools::file_ext(dic_path)
    if (extension_dic == "xlsx") {
      # Read XLSX file
      rlang::check_installed("openxlsx", reason = "to read .xlsx files.")
      dic <- openxlsx::read.xlsx(dic_path, colNames = FALSE, detectDates = TRUE, sheet = 1)
    } else if (extension_dic == "csv") {
      # Read CSV file
      dic <- utils::read.csv(dic_path, encoding = "UTF-8", header = FALSE, sep = sep)
      # Error: the dictionary is not UTF-8 (e.g. it was saved again with Excel as "CSV", which uses the encoding of Windows)
      if (!all(validUTF8(as.character(unlist(dic))))) {
        rd_abort(
          c("The dictionary {.file {dic_path}} is not encoded in UTF-8.", "i" = "Use the dictionary exported by REDCap or, if you edited it with Excel, save it as {.val CSV UTF-8}."),
          class = "REDCapDM_error_file"
        )
      }
    } else {
      rd_abort(c("Unsupported dictionary format: {.file {dic_path}}.", "i" = "Only CSV and XLSX are supported."), class = "REDCapDM_error_file")
    }

    # Process the dictionary: clean names
    names(dic) <- dic[1, ]
    dic <- dic[-1, ]
    names(dic) <- janitor::make_clean_names(names(dic))
    names(dic)[1] <- "field_name"

    # Text without spaces or line breaks at the beginning or at the end, as with the API
    dic[] <- lapply(dic, function(x) if (is.character(x)) trimws(x) else x)

    # Rows without a field name are not fields (e.g. empty rows of a dictionary saved with Excel)
    dic <- dic[!is.na(dic$field_name) & trimws(dic$field_name) != "", ]

    # Warning: fields that appear more than once in the dictionary (only the first one is kept)
    repeated_fields <- unique(dic$field_name[duplicated(dic$field_name)])
    if (length(repeated_fields) > 0) {
      rd_warn("The dictionary has more than one row for {?the field/the fields} {.var {repeated_fields}}: only the first one is kept.")
      dic <- dic[!duplicated(dic$field_name), ]
    }

    if (dic[1, 1] != "record_id") {
      dic[1, 1] <- "record_id"
    }

    # Remove descriptive variables from dictionary
    if ("descriptive" %in% dic$field_type) {
      dic <- dic |>
        dplyr::filter(!.data$field_type %in% "descriptive")
    }


    # Text fields that look like numbers (e.g. phones, postal codes or identifiers with leading zeros): read.csv() turns them into numbers and changes them ("0034" is read as 34). They keep the text exported by REDCap.
    data <- keep_text_fields(data, dic, if (length(csv_file) > 0) file.path(dirname(data_path), csv_file[1]) else NA)

    # The labels of the options that the R script wrote without their spaces take the label of the dictionary, as with the API
    data <- dictionary_option_labels(data, dic)

    # Indicator of longitudinal projects
    longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

    # Read event file
    if (!is.na(event_path)) {
      setwd(oldwd)

      # Evaluate the extension
      extension <- tools::file_ext(event_path)

      if (extension == "xlsx") {
        # Read XLSX file
        rlang::check_installed("openxlsx", reason = "to read .xlsx files.")
        event_form <- openxlsx::read.xlsx(event_path, detectDates = TRUE, sheet = 1)
      } else if (extension == "csv") {
        # Read CSV file -> REDCap exports this file without a final newline: readLines(warn = FALSE) avoids the "incomplete final line" warning of read.csv()
        event_form <- utils::read.csv(text = readLines(event_path, warn = FALSE, encoding = "UTF-8"))
      } else {
        rd_abort(c("Unsupported event mapping format: {.file {event_path}}.", "i" = "Only CSV and XLSX are supported."), class = "REDCapDM_error_file")
      }

      # Error if the exported event_form file is not the correct one! (using events instead of instrument-event)
      if ("event_name" %in% names(event_form)) {
        rd_abort(
          c("Invalid file provided in {.arg event_path}.", "i" = "Please download the instrument-event mapping file from the {.emph Designate Instruments for My Events} tab of your REDCap project and try again."),
          class = "REDCapDM_error_file"
        )
      }

      # Output
      data_def <- list(data = data, dictionary = dic, event_form = event_form)
    } else {
      # If no event is specified and the project is longitudinal
      if (longitudinal) {
        rd_warn("The project is longitudinal. Consider providing {.arg event_path} for event-form correspondence.")
      }

      data_def <- list(data = data, dictionary = dic)
    }
  }

  # Process data from API connection
  if (all(!c(token, uri) %in% NA) && all(c(data_path, dic_path) %in% NA)) {
    # REDCapR 1.2.0 or later: older versions only return the events of the first arm (redcap_event_instruments())
    rlang::check_installed("REDCapR", version = "1.2.0", reason = "to read data from the REDCap API.")

    # Message: Begin data import process
    rd_inform("Importing data from REDCap API...")

    # Read labels from REDCap

    ## Error SSL peer certificate (GitHub issue #6)

    # Errors of the requests of the records
    api_error <- function(e) {
      # (REDCapR gives this error when some records of its first request (their identifiers) are not in the data, e.g.
      # without their identifier field: it is the cause only when filter_field doesn't have it)
      if (grepl("REDCap's PHP code is likely trying to process too much text in one bite", conditionMessage(e)) && !is.null(filter_field) &&
        !api_id_requested(tryCatch(REDCapR::redcap_metadata_read(redcap_uri = uri, token = token, verbose = FALSE)$data, error = function(e) NULL), filter_field)) {
        rd_abort("The {.var record_id} or equivalent variable is missing on the {.arg filter_field} argument.", class = "REDCapDM_error_argument", call = NULL)
      } else if (grepl("SSL peer certificate", conditionMessage(e))) {
        rd_abort(
          c("Unable to establish a secure connection.", "i" = "Consider bypassing SSL verification: {.code httr::with_config(httr::config(ssl_verifypeer = FALSE), redcap_data(...))}."),
          class = "REDCapDM_error_api", call = NULL
        )
      } else {
        rd_abort("The data couldn't be read from the REDCap API.", class = "REDCapDM_error_api", parent = e, call = NULL)
      }
    }

    # The values are read as text (guess_type = FALSE), also in the first request of REDCapR, which reads the identifiers
    # of the records: as numbers, two long identifiers (e.g. 9007199254740992 and 9007199254740993) would be the same one,
    # and 100000000000000 would be requested as 1e+14
    # (a function, as the labels are read again if the data changed between the two requests, see below)
    read_labels <- function() tryCatch(
      suppressMessages(REDCapR::redcap_read(redcap_uri = uri, token = token, verbose = FALSE, raw_or_label = "label", raw_or_label_headers = "label", export_data_access_groups = TRUE, export_survey_fields = survey_fields, fields = filter_field, guess_type = FALSE)$data),
      error = api_error
    )
    labels <- read_labels()

    # Ensure labels were retrieved and assign a default column name
    if (nrow(labels) > 0) {
      record_id_label <- names(labels)[1]
      names(labels)[1] <- "record_id"
    } else {
      rd_abort(
        c("Data retrieval is currently unavailable.", "i" = "Please check the status of the REDCap server, confirm the existence of records within the project and verify that you have the necessary data export and API permissions to perform this operation."),
        class = "REDCapDM_error_api"
      )
    }

    # The labels of the events, repeated instruments and data access groups (their factor versions) are added to the data
    # once it is read, after checking that both requests returned the same rows
    labels_data <- labels

    # Clean up label names by removing suffixes
    labels <- gsub("\\.{3}\\d+$", "", c(record_id_label, names(labels)[-1]))

    # Message: Intermediate status update
    rd_inform("Almost done...")

    # Fetch metadata dictionary using the REDCap API (before the data: it gives the text fields)
    dic_api <- REDCapR::redcap_metadata_read(
      redcap_uri = uri,
      token = token,
      verbose = FALSE
    )$data

    # Missing Data Codes defined in the project (if they can't be read, the default codes are used)
    api_mdc <- tryCatch(
      parse_mdc_codes(REDCapR::redcap_project_info_read(redcap_uri = uri, token = token, verbose = FALSE)$data$missing_data_codes),
      error = function(e) character(0)
    )

    # Text fields without a numeric or date validation are read as text: as with the exported files, they are numbers
    # only when that doesn't change them (e.g. a phone "+34600111222" or a code "1.50" keep their text)
    api_validation <- if ("text_validation_type_or_show_slider_number" %in% names(dic_api)) dic_api$text_validation_type_or_show_slider_number else rep(NA_character_, nrow(dic_api))
    api_text_fields <- dic_api[[1]][-1][dic_api$field_type[-1] %in% c("text", "notes") & !grepl("^(number|integer|date|time)", api_validation[-1])]
    if (!all(filter_field %in% NA)) {
      api_text_fields <- intersect(api_text_fields, filter_field)
    }
    # Fetch main data using the REDCap API
    # (the data are read as text and their types are guessed afterwards over all the records, as REDCapR does, except
    # for the identifier and the text fields)
    data_api <- tryCatch(
      REDCapR::redcap_read(redcap_uri = uri, token = token, verbose = FALSE, raw_or_label = "raw", export_data_access_groups = TRUE, export_survey_fields = survey_fields, fields = filter_field, guess_type = FALSE)$data,
      error = api_error
    )
    if (any(vapply(data_api, is.character, logical(1)))) {
      text_cols <- unique(c(names(data_api)[1], intersect(api_text_fields, names(data_api))))
      data_api <- readr::type_convert(
        data_api,
        col_types = do.call(readr::cols, c(stats::setNames(rep(list(readr::col_character()), length(text_cols)), text_cols), list(.default = readr::col_guess())))
      )
      # The identifier is a number only when its text is that number (not with "007", 100000000000000, written as
      # 1e+14, or 9007199254740993, beyond the precision of the numbers of R), as with the exported files
      ids <- data_api[[1]]
      as_number <- if (is.character(ids)) readr::parse_guess(ids, guess_integer = FALSE) else ids
      if (is.character(ids) && is.numeric(as_number) && all(is.na(ids) | as.character(as_number) == ids)) {
        data_api[[1]] <- as_number
      }
      for (field in setdiff(text_cols, names(data_api)[1])) {
        as_number <- utils::type.convert(data_api[[field]], as.is = TRUE)
        if (is.character(data_api[[field]]) && !is.character(as_number) && all(is.na(data_api[[field]]) | as.character(as_number) == data_api[[field]])) {
          data_api[[field]] <- as_number
        }
      }
    }

    # Ensure data retrieval was successful
    if (nrow(data_api) > 0) {
      names(data_api)[1] <- "record_id"
    } else {
      rd_abort(
        c("Observational data retrieval is currently unavailable.", "i" = "Please verify the status of the REDCap server or confirm the existence of records within the project."),
        class = "REDCapDM_error_api"
      )
    }

    # Ensure dictionary has appropriate names
    names(dic_api)[1] <- "field_name"
    if (dic_api[1, 1] != "record_id") {
      dic_api[1, 1] <- "record_id"
    }

    # Making sure the names of both dictionaries(exported data and API connection) match
    names(dic_api)[names(dic_api) %in% c("select_choices_or_calculations", "branching_logic", "question_number")] <- c("choices_calculations_or_slider_labels", "branching_logic_show_field_only_if", "question_number_surveys_only")

    # Apply labels to the main dataset (named by column, so that they are applied again at the end)
    if (length(labels) == ncol(data_api)) {
      names(labels) <- names(data_api)
    }
    data_api <- purrr::map2(data_api, as.list(labels), ~ {
      if (!is.null(.y)) {
        attr(.x, "label") <- .y
      }
      .x
    }) |>
      as.data.frame()

    # Empty cells of the dictionary are empty texts, as in the dictionary exported from REDCap
    # (a column without any value is read as logical by REDCapR)
    # (and without spaces or line breaks at the beginning or at the end, as with the exported files: REDCapR only removes
    # them from the text without quotes)
    dic_api[] <- lapply(dic_api, function(x) if (is.character(x) || (is.logical(x) && all(is.na(x)))) replace(trimws(as.character(x)), is.na(x), "") else x)

    # Filter descriptive fields from dictionary
    if ("descriptive" %in% dic_api$field_type) {
      dic_api <- dic_api |>
        dplyr::filter(!.data$field_type %in% "descriptive")
    }

    # Filter dictionary fields based on filter_field if provided
    if (!all(filter_field %in% NA)) {
      dic_api <- dic_api |>
        dplyr::filter(.data$field_name %in% filter_field)
    }

    # Process checkbox fields into factors

    if (sum(dic_api$field_type %in% "checkbox") > 0) {
      var_check <- names(data_api)[grep("___", names(data_api))]

      data_api <- data_api |>
        dplyr::mutate(dplyr::across(dplyr::all_of(var_check), ~ factor(., levels = c("0", "1"), labels = c("Unchecked", "Checked")), .names = "{col}.factor"))
    }

    # Process radio and dropdown fields into factors

    if (sum(dic_api$field_type %in% c("radio", "dropdown")) > 0) {
      var_radio <- dic_api |>
        dplyr::filter(.data$field_type %in% c("radio", "dropdown")) |>
        dplyr::select("field_name", "field_type", "choices_calculations_or_slider_labels") |>
        dplyr::mutate(
          factor = purrr::map(.data$choices_calculations_or_slider_labels, ~ stringr::str_split(.x, "\\|") |>
            unlist() |>
            trimws()),
          levels = purrr::map(factor, ~ gsub(",.*", "", .x)),
          labels = purrr::map(factor, ~ gsub("^[^,]*,\\s*", "", .x))
        )

      for (i in var_radio$field_name) {
        tryCatch(
          {
            data_api[[stringr::str_glue("{i}.factor")]] <- factor(data_api[[i]],
              levels = c(var_radio$levels[[which(var_radio$field_name %in% i)]]),
              labels = c(var_radio$labels[[which(var_radio$field_name %in% i)]])
            )
          },
          error = function(e) {
            rd_warn("The following variable could not be replicated in its factor version: {.var {i}}. Please manually create a factor version of this variable named {.var {paste0(i, '.factor')}} to properly execute the {.fn rd_transform} function.")
          }
        )
      }
    }

    # Process yes/no, true/false and form status fields into factors, as the R script exported by REDCap does
    two_options <- list(yesno = c("1" = "Yes", "0" = "No"), truefalse = c("1" = "True", "0" = "False"))
    for (type in names(two_options)) {
      for (i in intersect(dic_api$field_name[dic_api$field_type %in% type], names(data_api))) {
        data_api[[paste0(i, ".factor")]] <- factor(data_api[[i]], levels = names(two_options[[type]]), labels = unname(two_options[[type]]))
      }
    }
    for (i in intersect(paste0(unique(dic_api$form_name), "_complete"), names(data_api))) {
      data_api[[paste0(i, ".factor")]] <- factor(data_api[[i]], levels = c("0", "1", "2"), labels = c("Incomplete", "Unverified", "Complete"))
    }

    # Labels of the events, repeated instruments and data access groups
    # The values and their labels are read with two requests: if the records changed in between (e.g. someone entered,
    # deleted or imported data), the rows of both requests are not the same ones. So the labels are not taken by their
    # position: each label is the one of its code (unique event name, instrument or data access group), and the label
    # of each code is checked with the metadata of the project (its events and arms, instruments and data access groups).
    # If a label isn't the one of its code, the labels are read again (twice at most).
    api_meta <- list()
    read_meta <- function(result) {
      if (isTRUE(result$success) && is.data.frame(result$data)) as.data.frame(result$data) else NULL
    }
    # (metadata that can't be read is NULL)
    if ("redcap_event_name" %in% names(data_api)) {
      api_meta$event <- tryCatch(
        api_event_metadata(
          tryCatch(read_meta(REDCapR::redcap_event_read(redcap_uri = uri, token = token, verbose = FALSE)), error = function(e) NULL),
          tryCatch(read_meta(REDCapR::redcap_arm_export(redcap_uri = uri, token = token, verbose = FALSE)), error = function(e) NULL)
        ),
        error = function(e) NULL
      )
    }
    if ("redcap_repeat_instrument" %in% names(data_api)) {
      api_meta$instrument <- tryCatch(
        api_code_metadata(
          tryCatch(read_meta(REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)), error = function(e) NULL),
          "instrument_name", "instrument_label"
        ),
        error = function(e) NULL
      )
    }
    if ("redcap_data_access_group" %in% names(data_api)) {
      api_meta$dag <- tryCatch(
        api_code_metadata(
          tryCatch(read_meta(REDCapR::redcap_dag_read(redcap_uri = uri, token = token, verbose = FALSE)), error = function(e) NULL),
          "unique_group_name", "data_access_group_name"
        ),
        error = function(e) NULL
      )
    }

    for (attempt in 1:3) {
      label_columns <- api_label_columns(labels_data, data_api, api_meta)
      if (!is.null(label_columns)) break
      if (attempt == 3) {
        rd_abort(
          c("The records of the project changed while they were read from the REDCap API: the two requests of {.fn redcap_data} (the values and their labels) didn't return the same rows.",
            "i" = "Try again when no one is entering, deleting or importing data in the project."),
          class = "REDCapDM_error_api"
        )
      }
      labels_data <- read_labels()
      if (nrow(labels_data) > 0) names(labels_data)[1] <- "record_id"
    }

    unchecked <- attr(label_columns, "unchecked")
    if (length(unchecked) > 0) {
      rd_warn(c("The labels of {.var {unchecked}} couldn't be checked with the metadata of the project (its events, instruments or data access groups): {?this column has/these columns have} the codes of REDCap instead of their labels.",
        "i" = "Check that the API token can export the events, arms, instruments and data access groups of the project."))
    }
    attr(label_columns, "unchecked") <- NULL

    data_api <- data_api |>
      dplyr::bind_cols(label_columns)

    # Determine if project is longitudinal
    longitudinal <- ifelse("redcap_event_name" %in% names(data_api), TRUE, FALSE)

    # Handle event path or fetch event-form correspondence via API
    if (!is.na(event_path)) {
      # Warning: event_path not necessary while using API connection
      rd_warn("The {.arg event_path} argument is not required when using an API connection.")

      setwd(oldwd)

      # Evaluate the extension
      extension <- tools::file_ext(event_path)

      if (extension == "xlsx") {
        # Read XLSX file
        rlang::check_installed("openxlsx", reason = "to read .xlsx files.")
        event_form <- openxlsx::read.xlsx(event_path, detectDates = TRUE, sheet = 1)
      } else if (extension == "csv") {
        # Read CSV file -> REDCap exports this file without a final newline: readLines(warn = FALSE) avoids the "incomplete final line" warning of read.csv()
        event_form <- utils::read.csv(text = readLines(event_path, warn = FALSE, encoding = "UTF-8"))
      } else {
        rd_abort(c("Unsupported event mapping format: {.file {event_path}}.", "i" = "Only CSV and XLSX are supported."), class = "REDCapDM_error_file")
      }

      # Error if the exported event_form file is not the correct one! (using events instead of instrument-event)
      if ("event_name" %in% names(event_form)) {
        rd_abort(
          c("Invalid file provided in {.arg event_path}.", "i" = "Please download the instrument-event mapping file from the {.emph Designate Instruments for My Events} tab of your REDCap project and try again."),
          class = "REDCapDM_error_file"
        )
      }

      # Output
      data_def <- list(
        data = data_api,
        dictionary = dic_api,
        event_form = event_form
      )
    } else {
      # If the event file is not specified, the function reads it using the API connection (in case of longitudinal projects)
      if (longitudinal) {
        event_form <- as.data.frame(REDCapR::redcap_event_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data)

        data_def <- list(
          data = data_api,
          dictionary = dic_api,
          event_form = event_form
        )
      } else {
        data_def <- list(
          data = data_api,
          dictionary = dic_api
        )
      }
    }

    # Message:  Completion
    rd_inform(c("v" = "API data import completed."))
  }

  # Apply UTF-8 encoding to character columns
  for (i in seq_along(data_def$data)) {
    if (is.character(data_def$data[, i])) {
      suppressWarnings(data_def$data[, i] <- stringr::str_conv(data_def$data[, i], "UTF-8"))
    }
  }

  # Transform empty values into missing values (NA)
  data_def$data <- data_def$data |>
    # Fix characters:
    dplyr::mutate_if(is.character, ~ gsub("^$", NA, .x)) |>
    # Fix factors:
    dplyr::mutate_if(is.factor, function(x) {
      levels(x)[levels(x) == ""] <- NA
      x
    })

  # Remove variables with no corresponding event
  if (!is.null(event_form)) {
    # Error: the event file is not the instrument-event mapping of this project
    if (!"form" %in% names(event_form) || !any(event_form$form %in% data_def$dictionary$form_name)) {
      rd_abort(
        c("The file in {.arg event_path} is not the instrument-event mapping of this project: none of its forms is in the dictionary.", "i" = "Download it from the {.emph Designate Instruments for My Events} tab of your REDCap project."),
        class = "REDCapDM_error_file"
      )
    }

    var_noevent <- data_def$dictionary |>
      dplyr::filter(!.data$form_name %in% event_form$form) |>
      dplyr::pull(.data$field_name)

    if (length(var_noevent) > 0) {

      rd_warn("The following variable{?s} {?was/were} removed since {?it is/they are} not linked to any event: {.var {var_noevent}}.")

      var_noevent <- intersect(var_noevent, names(data_def$data))

      var_noevent <- var_noevent[var_noevent != "record_id"]

      data_def$data <- data_def$data |>
        dplyr::select(-dplyr::any_of(var_noevent))

      data_def$dictionary <- data_def$dictionary |>
        dplyr::filter(!.data$field_name %in% var_noevent)
    }
  }

  # Define the Missing Data Codes (MDCs)
  missing_codes <- c(
    "NI", "INV", "UNK", "NASK", "ASKU", "NAV", "MSK",
    "NA", "NAVU", "NP", "QS", "QI", "TRC", "UNC",
    "DER", "PINF", "NINF", "OTH"
  )

  # Codes of the project: read from the API and given in `mdc`
  missing_codes <- unique(c(missing_codes, api_mdc, mdc$code))
  mdc_labels <- stats::setNames(mdc$label[!is.na(mdc$label)], mdc$code[!is.na(mdc$label)])

  # Numeric codes (e.g. -999): a numeric column can contain them
  mdc_numbers <- suppressWarnings(as.numeric(missing_codes))
  numeric_mdc <- mdc_numbers[is.finite(mdc_numbers)]
  numeric_mdc_codes <- missing_codes[is.finite(mdc_numbers)]

  # Values that are a Missing Data Code (numbers are compared as numbers)
  is_mdc_value <- function(x, ignore_case = FALSE) {
    if (is.numeric(x)) {
      return(!is.na(x) & x %in% numeric_mdc)
    }
    value <- stringr::str_trim(as.character(x))
    if (ignore_case) toupper(value) %in% toupper(missing_codes) else value %in% missing_codes
  }

  # Text of the code of each value (NA when it isn't a code), and the label given in `mdc` instead of the code
  mdc_text <- function(x) {
    if (is.numeric(x)) {
      return(numeric_mdc_codes[match(x, numeric_mdc)])
    }
    as.character(ifelse(is_mdc_value(x, ignore_case = TRUE), as.character(x), NA_character_))
  }
  mdc_shown <- function(x) {
    code <- mdc_text(x)
    label <- unname(mdc_labels[match(toupper(stringr::str_trim(code)), toupper(names(mdc_labels)))])
    # (as.character: a project without records gives an empty vector)
    as.character(ifelse(is.na(label), code, label))
  }

  # Create the column pattern for checkboxes with Missing Data Codes (MDCs)
  # (in the name of a checkbox column, REDCap writes the code in lower case and with `_` instead of `-` or `.`)
  checkbox_mdc_codes <- unique(c(missing_codes, gsub("[^A-Za-z0-9_]", "_", missing_codes)))
  checkbox_mdc_pattern <- stringr::regex(
    paste0("___(", paste(escape_regex(checkbox_mdc_codes), collapse = "|"), ")(\\.factor)?$"),
    ignore_case = TRUE
  )

  # Identify which fields are of type 'text' & have non-null validation
  validated_text_fields <- if ("text_validation_type_or_show_slider_number" %in% names(data_def$dictionary)) {
    data_def$dictionary |>
      dplyr::filter(
        .data$field_type == "text",
        !is.na(.data$text_validation_type_or_show_slider_number) &
          .data$text_validation_type_or_show_slider_number != ""
      ) |>
      dplyr::pull(.data$field_name)
  } else {
    character(0)
  }

  # Identify categorical text field types
  categorical_fields <- data_def$dictionary |>
    dplyr::filter(.data$field_type %in% c("radio", "yesno", "dropdown", "truefalse")) |>
    dplyr::pull(.data$field_name)

  
  # A Missing Data Code turns the whole column into text when the CSV is read. Once the codes are removed, numeric fields and multiple-choice codes recover their type (as if the column had no codes); dates, phones, etc. stay as text
  numeric_fields <- if ("text_validation_type_or_show_slider_number" %in% names(data_def$dictionary)) {
    data_def$dictionary |>
      dplyr::filter(
        .data$field_type == "text",
        .data$text_validation_type_or_show_slider_number %in% c("number", "integer") |
          grepl("^number_", .data$text_validation_type_or_show_slider_number)
      ) |>
      dplyr::pull(.data$field_name)
  } else {
    character(0)
  }
  # (a number written as text in these fields is a numeric code when its value is, e.g. `-999.0` or `-9.99e2` with -999)
  number_fields <- numeric_fields
  is_mdc_number <- function(value, field) {
    field %in% number_fields & !is.na(value) & suppressWarnings(as.numeric(value)) %in% numeric_mdc
  }
  numeric_fields <- c(numeric_fields, categorical_fields)

  restore_type <- function(x, field) {
    if (field %in% numeric_fields && is.character(x)) utils::type.convert(x, as.is = TRUE) else x
  }

  # Validated fields whose Missing Data Codes are kept in a `<field>_mdc` column (rm_mdc = FALSE)
  mdc_fields <- character(0)

  if (rm_mdc) {
    # Include .factor versions in "radio" & "yesno" field types
    categorical_factor_fields <- paste0(categorical_fields, ".factor")
    # Filter only the text validated columns
    all_target_fields <- c(validated_text_fields, categorical_fields, categorical_factor_fields)
    target_columns <- intersect(all_target_fields, colnames(data_def$data))

    removed_checkbox_cols <- colnames(data_def$data)[stringr::str_detect(colnames(data_def$data), checkbox_mdc_pattern)]
    # (a checkbox option of a code is in the data when a record has it checked: REDCap exports these columns empty too)
    checked_checkbox_mdc <- any(vapply(removed_checkbox_cols[!grepl("\\.factor$", removed_checkbox_cols)], function(col) {
      any(as.character(data_def$data[[col]]) %in% c("1", "Checked"))
    }, logical(1)))
    n_mdc_values <- sum(vapply(
      intersect(target_columns, colnames(data_def$data)),
      function(col) {
        x <- data_def$data[[col]]
        sum(is_mdc_value(x) | (!is.numeric(x) & is_mdc_number(stringr::str_trim(as.character(x)), col)), na.rm = TRUE)
      },
      numeric(1)
    ))

    # Replace Missing Data Codes with NA in the selected columns
    data_def$data <- data_def$data |>
      dplyr::select(
        -dplyr::matches(checkbox_mdc_pattern)
      ) |>
            dplyr::mutate(
        dplyr::across(
          dplyr::all_of(target_columns),
          ~ {
            # Numeric columns can only contain numeric codes
            if (is.numeric(.x)) {
              return(replace(.x, is_mdc_value(.x), NA))
            }
            if (!is.character(.x) && !is.factor(.x)) {
              return(.x)
            }
            was_factor <- is.factor(.x)
            val_char <- stringr::str_trim(as.character(.x))
            # (.env: a field of the project can be called `missing_codes`)
            val_clean <- ifelse(
              val_char %in% .env$missing_codes | .env$is_mdc_number(val_char, dplyr::cur_column()),
              NA_character_,
              val_char
            )
            if (was_factor) factor(val_clean, levels = levels(.x)) else restore_type(val_clean, dplyr::cur_column())
          }
        )
      )
    # Warning: only when the data has Missing Data Codes and the user didn't give the codes of the project in `mdc`
    if ((checked_checkbox_mdc || n_mdc_values > 0) && nrow(mdc) == 0) {
      rd_warn("All the Missing Data Codes have been removed.")
    }
  } else {

    # Validated text fields (numbers, dates, ...): the codes are set to NA, so that the field keeps its type, and kept in a new `<field>_mdc` column next to it
    text_targets <- intersect(validated_text_fields, colnames(data_def$data))
    for (var in text_targets) {
      x <- data_def$data[[var]]

      # Numeric columns can only contain numeric codes
      if (!is.character(x) && !is.factor(x) && !is.numeric(x)) {
        next
      }

      if (is.numeric(x)) {
        is_mdc <- is_mdc_value(x)
        val_char <- mdc_text(x)
      } else {
        val_char <- stringr::str_trim(as.character(x))
        is_mdc <- is_mdc_value(val_char, ignore_case = TRUE)
        # (a number written as text, e.g. `-999.0`, is kept with the text of its code)
        by_number <- !is_mdc & is_mdc_number(val_char, var)
        is_mdc <- is_mdc | by_number
      }

      if (any(is_mdc)) {
        mdc_col <- paste0(var, "_mdc")

        if (mdc_col %in% colnames(data_def$data)) {
          rd_warn("The Missing Data Codes of {.var {var}} can't be kept because the dataset already has a {.var {mdc_col}} column. They are set to NA.")
        } else {
          mdc_values <- ifelse(is_mdc, val_char, NA_character_)
          if (!is.numeric(x)) {
            mdc_values[by_number] <- numeric_mdc_codes[match(suppressWarnings(as.numeric(val_char[by_number])), numeric_mdc)]
          }
          data_def$data[[mdc_col]] <- mdc_values
          data_def$data <- data_def$data |>
            dplyr::relocate(dplyr::all_of(mdc_col), .after = dplyr::all_of(var))
          mdc_fields <- c(mdc_fields, var)
        }
      }

      data_def$data[[var]] <- if (is.numeric(x)) replace(x, is_mdc, NA) else restore_type(ifelse(is_mdc, NA_character_, val_char), var)
    }

    if (length(mdc_fields) > 0) {
      mdc_new_cols <- paste0(mdc_fields, "_mdc")
      rd_warn("Missing Data Codes found in validated fields ({.var {mdc_fields}}). They are set to NA and kept in the new column{?s} {.var {mdc_new_cols}}.")
    }

    # Process categorical variables
    cat_targets <- intersect(categorical_fields, colnames(data_def$data))
    for (var in cat_targets) {
      factor_var <- paste0(var, ".factor")

      if (factor_var %in% colnames(data_def$data)) {

        # Make sure that MDC labels are in the .factor version (the label given in `mdc`, or else the code)
        data_def$data[[factor_var]] <- dplyr::coalesce(
          as.character(data_def$data[[factor_var]]),
          mdc_shown(data_def$data[[var]])
        )

        # Assign a numerical sequential code to MDC in the original variable
        is_mdc_mask <- is_mdc_value(data_def$data[[var]], ignore_case = TRUE)
        orig_vals <- as.character(data_def$data[[var]])

        if (any(is_mdc_mask, na.rm = TRUE)) {

          suppressWarnings({
            numeric_codes <- as.numeric(orig_vals[!is_mdc_mask & !is.na(orig_vals)])
          })
          # (also the codes of the options that nobody chose: the code given to the Missing Data Codes can't be one of them)
          numeric_codes <- c(numeric_codes, option_codes(data_def$dictionary, var))
          numeric_codes <- numeric_codes[!is.na(numeric_codes)]
          next_code <- if (length(numeric_codes) > 0) max(numeric_codes) + 1 else 1

          orig_vals[is_mdc_mask] <- as.character(next_code)
          data_def$data[[var]] <- restore_type(orig_vals, var)
        }
      }
      }

    # Checkboxes: Remove only empty MDC columns
    cb_mdc_cols <- colnames(data_def$data)[stringr::str_detect(colnames(data_def$data), checkbox_mdc_pattern)]
    
    cols_to_remove <- c()

    base_cb_cols <- unique(stringr::str_remove(cb_mdc_cols, "\\.factor$"))

    for (col in base_cb_cols) {
      if (col %in% colnames(data_def$data)) {

        col_data <- data_def$data[[col]]

        has_at_least_one_1 <- any(col_data == 1 | col_data == "1", na.rm = TRUE)

        if (!has_at_least_one_1) {
          cols_to_remove <- c(cols_to_remove, col)

          factor_col <- paste0(col, ".factor")
          if (factor_col %in% colnames(data_def$data)) {
            cols_to_remove <- c(cols_to_remove, factor_col)
            }
          }
        }
      }

    if (length(cols_to_remove) > 0) {
      data_def$data <- data_def$data |>
        dplyr::select(-dplyr::all_of(cols_to_remove))
    }
  }

  # Clean HTML labels
  if ("field_label" %in% names(data_def$dictionary)) {
    data_def$dictionary <- data_def$dictionary |>
      dplyr::mutate(
        field_label = dplyr::case_when(
          is.na(.data$field_label) | !nzchar(.data$field_label) ~ .data$field_label,

          TRUE ~ clean_label(.data$field_label)
        )
      )
  }

  # Reapply labels to the modified dataset (without HTML, as the labels of the dictionary)
  data_def$data <- data_def$data |>
    labelled::set_variable_labels(.labels = stats::setNames(clean_label(labels), names(labels)) |> as.list(), .strict = FALSE)

  
  # Labels of the columns with the Missing Data Codes (rm_mdc = FALSE)
  if (length(mdc_fields) > 0) {
    mdc_labels <- if ("field_label" %in% names(data_def$dictionary)) {
      data_def$dictionary$field_label[match(mdc_fields, data_def$dictionary$field_name)]
    } else {
      rep(NA_character_, length(mdc_fields))
    }
    mdc_labels <- paste0(ifelse(is.na(mdc_labels) | !nzchar(mdc_labels), mdc_fields, mdc_labels), " (Missing Data Code)")

    data_def$data <- data_def$data |>
      labelled::set_variable_labels(.labels = stats::setNames(as.list(mdc_labels), paste0(mdc_fields, "_mdc")))
  }

  # Change the labelled class of each column but don't remove the label:
  data_def$data <- data_def$data |>
    dplyr::mutate_all(function(x) {
      class(x) <- setdiff(class(x), "labelled")
      x
    })

  # Warning: variables of the data that are not in the dictionary (the dictionary of another project or an old one)
  data_fields <- names(data_def$data)
  data_fields <- unique(sub("___.*$", "", data_fields[!grepl("^redcap_|\\.factor$|_complete$|_timestamp$|_mdc$", data_fields)]))
  not_in_dic <- setdiff(data_fields, c("record_id", data_def$dictionary$field_name))
  if (length(not_in_dic) > 0) {
    shown_vars <- utils::head(not_in_dic, 10)
    rd_warn(c("{length(not_in_dic)} variable{?s} of the data {?is/are} not in the dictionary: {.var {shown_vars}}.", "i" = "Check that the dictionary is the current one of this project."))
  }

  # Warning: project without records
  if (nrow(data_def$data) == 0) {
    rd_warn("The project has no records yet: the data has 0 rows.")
  }

  # Return the result
  return(data_def)
}

#' Labels of the options of the radio and dropdown fields as in the dictionary
#'
#' The R script exported by REDCap removes the line breaks of the labels of the
#' options without leaving a space (e.g. "oral<line break>corticosteroids" is
#' written "oralcorticosteroids"), while the dictionary and the API keep a space.
#' The labels of the `.factor` versions that only differ from the label of the
#' dictionary in their spaces take the label of the dictionary, which is the one
#' that the API gives. Any other label is kept as it is.
#'
#' @param data Data read by the R script.
#' @param dic Dictionary of the project.
#' @return `data`, with the labels of the dictionary.
#' @noRd
dictionary_option_labels <- function(data, dic) {
  if (!all(c("field_name", "field_type", "choices_calculations_or_slider_labels") %in% names(dic))) {
    return(data)
  }
  fields <- dic[dic$field_type %in% c("radio", "dropdown") & paste0(dic$field_name, ".factor") %in% names(data), , drop = FALSE]
  # (the non-breaking space, U+00A0, that the labels of REDCap can have, is also a space)
  squash <- function(x) gsub("\\s+", "", gsub("\u00a0", " ", x, fixed = TRUE))

  for (k in seq_len(nrow(fields))) {
    column <- paste0(fields$field_name[k], ".factor")
    x <- data[[column]]
    if (!is.factor(x) || is.na(fields$choices_calculations_or_slider_labels[k])) next

    # Labels of the options as the API reads them from the dictionary
    options <- trimws(unlist(strsplit(fields$choices_calculations_or_slider_labels[k], "|", fixed = TRUE)))
    labels <- gsub("^[^,]*,\\s*", "", options)

    new_levels <- levels(x)
    for (j in seq_along(new_levels)) {
      same <- unique(labels[squash(labels) == squash(new_levels[j]) & labels != new_levels[j]])
      if (length(same) == 1 && !same %in% new_levels) {
        new_levels[j] <- same
      }
    }
    if (!identical(new_levels, levels(x))) {
      levels(x) <- new_levels
      data[[column]] <- x
    }
  }

  data
}

#' Keep the text of the text fields that read.csv() turned into numbers
#'
#' The R script exported by REDCap reads the data with `read.csv()`, which turns
#' the text fields that look like numbers into numbers and changes their value
#' (e.g. `"0034"` is read as `34`, `"1.50"` as `1.5` and `"T"` as `TRUE`). The
#' text fields (and the record identifier) whose value changed take the text of
#' the csv file again. The fields that kept their value are not changed.
#'
#' @param data Data read by the R script, with the record identifier named `record_id`.
#' @param dic Dictionary of the project.
#' @param csv_path Path of the csv file read by the R script.
#' @return `data`, with the text of the changed text fields.
#' @noRd
keep_text_fields <- function(data, dic, csv_path) {
  if (is.na(csv_path) || !file.exists(csv_path) || !all(c("field_name", "field_type") %in% names(dic))) {
    return(data)
  }

  validation <- if ("text_validation_type_or_show_slider_number" %in% names(dic)) dic$text_validation_type_or_show_slider_number else rep(NA_character_, nrow(dic))
  text_fields <- dic$field_name[dic$field_type %in% c("text", "notes") & !grepl("^(number|integer|date|time)", validation)]
  text_fields <- intersect(c("record_id", text_fields), names(data))
  text_fields <- text_fields[vapply(data[text_fields], function(x) is.numeric(x) || is.logical(x), logical(1))]
  if (length(text_fields) == 0) {
    return(data)
  }

  csv <- utils::read.csv(csv_path, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8")
  names(csv)[1] <- "record_id"
  if (nrow(csv) != nrow(data)) {
    return(data)
  }

  for (field in intersect(text_fields, names(csv))) {
    text <- csv[[field]]
    changed <- !is.na(text) & (is.na(data[[field]]) | as.character(data[[field]]) != text)
    if (any(changed)) {
      label <- attr(data[[field]], "label")
      data[[field]] <- text
      attr(data[[field]], "label") <- label
    }
  }

  data
}


#' Missing Data Codes given in the `mdc` argument of `redcap_data()`
#'
#' @param mdc Vector of codes, optionally with their labels written as `code = label`.
#' @param call The function shown in the error.
#' @return A data frame with the codes (`code`) and their labels (`label`, `NA` when there isn't one).
#' @noRd
mdc_table <- function(mdc, call = rlang::caller_env()) {
  if (is.null(mdc)) {
    return(data.frame(code = character(0), label = character(0), stringsAsFactors = FALSE))
  }

  values <- if (is.numeric(mdc)) vapply(mdc, format, character(1), scientific = FALSE, trim = TRUE, digits = 15) else mdc
  if (!(is.character(mdc) || is.numeric(mdc)) || is.list(mdc) || length(mdc) == 0 || anyNA(values) || any(!nzchar(trimws(values)))) {
    rd_abort(
      c("{.arg mdc} must be a vector of Missing Data Codes, e.g. {.code c(\"-999\", \"-888\" = \"Not applicable\")}.", "x" = "It is {.obj_type_friendly {mdc}}, or it has missing or empty values."),
      class = "REDCapDM_error_argument", call = call
    )
  }

  values <- trimws(unname(values))
  codes <- names(mdc)
  codes <- if (is.null(codes)) rep("", length(values)) else trimws(ifelse(is.na(codes), "", codes))
  out <- data.frame(
    code = ifelse(nzchar(codes), codes, values),
    label = ifelse(nzchar(codes), values, NA_character_),
    stringsAsFactors = FALSE
  )
  out[!duplicated(out$code), , drop = FALSE]
}

#' Codes of the Missing Data Codes of a project, as the API returns them
#'
#' REDCap returns each code with its label (`code, label`), separated by `|` or line breaks.
#'
#' @param x Text with the Missing Data Codes of the project (`NULL`, `NA` or empty when it has none).
#' @return The codes.
#' @noRd
parse_mdc_codes <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) {
    return(character(0))
  }

  items <- trimws(unlist(strsplit(x, "[|\r\n]+")))
  codes <- trimws(sub(",.*$", "", items))
  unique(codes[nzchar(codes)])
}

#' Numeric codes of the options of a multiple-choice field
#'
#' @param dic Dictionary of the project.
#' @param field Name of the field.
#' @return The numeric codes of its options (`0` and `1` in yes/no and true/false fields).
#' @noRd
option_codes <- function(dic, field) {
  row <- dic[dic$field_name %in% field, , drop = FALSE]
  if (nrow(row) == 0) {
    return(numeric(0))
  }
  if (row$field_type[1] %in% c("yesno", "truefalse")) {
    return(c(0, 1))
  }
  if (!"choices_calculations_or_slider_labels" %in% names(row) || is.na(row$choices_calculations_or_slider_labels[1])) {
    return(numeric(0))
  }
  options <- strsplit(as.character(row$choices_calculations_or_slider_labels[1]), "|", fixed = TRUE)[[1]]
  codes <- suppressWarnings(as.numeric(trimws(sub(",.*$", "", options))))
  codes[!is.na(codes)]
}


#' Whether the identifier of the project is among the fields requested from the REDCap API
#'
#' @param dic Dictionary read from the API (its first field is the identifier), or `NULL` if it couldn't be read.
#' @param filter_field Fields requested (`filter_field` of [redcap_data()]).
#' @return `FALSE` only when the identifier is known and it isn't in `filter_field`.
#' @noRd
api_id_requested <- function(dic, filter_field) {
  if (!is.data.frame(dic) || nrow(dic) == 0 || ncol(dic) == 0) {
    return(TRUE)
  }
  id <- as.character(dic[[1]][1])
  is.na(id) || id %in% filter_field
}


#' Metadata of the events of a project read from the REDCap API
#'
#' @param events Events read with `REDCapR::redcap_event_read()` (`NULL` if they couldn't be read).
#' @param arms Arms read with `REDCapR::redcap_arm_export()` (`NULL` if they couldn't be read).
#' @return A data frame with the `code` (unique event name), `name`, `arm` (number) and `arm_name` of each event, or
#'   `NULL`.
#' @noRd
api_event_metadata <- function(events, arms) {
  if (!is.data.frame(events) || !all(c("unique_event_name", "event_name", "arm_num") %in% names(events))) {
    return(NULL)
  }
  arm_number <- intersect(c("arm_number", "arm_num"), names(arms))[1]
  arm_name <- intersect(c("arm_name", "name"), names(arms))[1]
  data.frame(
    code = as.character(events$unique_event_name),
    name = as.character(events$event_name),
    arm = as.character(events$arm_num),
    arm_name = if (is.data.frame(arms) && !is.na(arm_number) && !is.na(arm_name)) {
      as.character(arms[[arm_name]])[match(as.character(events$arm_num), as.character(arms[[arm_number]]))]
    } else {
      rep(NA_character_, nrow(events))
    },
    stringsAsFactors = FALSE
  )
}


#' Metadata of the instruments or data access groups of a project read from the REDCap API
#'
#' @param x Instruments (`REDCapR::redcap_instruments()`) or data access groups (`REDCapR::redcap_dag_read()`), `NULL` if
#'   they couldn't be read.
#' @param code,name Columns of `x` with the code and the label.
#' @return A data frame with `code` and `name`, or `NULL`.
#' @noRd
api_code_metadata <- function(x, code, name) {
  if (!is.data.frame(x) || !all(c(code, name) %in% names(x))) {
    return(NULL)
  }
  data.frame(code = as.character(x[[code]]), name = as.character(x[[name]]), arm = rep(NA_character_, nrow(x)), arm_name = rep(NA_character_, nrow(x)), stringsAsFactors = FALSE)
}


#' Whether a label read from the REDCap API is the label of a code, according to the metadata of the project
#'
#' The labels are compared without HTML tags or entities and with single spaces. The label of an event can have its
#' arm, as REDCap writes it in projects with several arms: `Name (Arm 1: Name of the arm)` (the word "Arm" depends on
#' the language of the server).
#'
#' @param label Label read with the records.
#' @param name,arm,arm_name Name of the code in the metadata, and number and name of the arm of an event (`NA` for the
#'   instruments and data access groups).
#' @return `TRUE` or `FALSE`.
#' @noRd
api_label_matches <- function(label, name, arm = NA, arm_name = NA) {
  clean <- function(x) {
    x <- gsub("&#039;", "&#39;", as.character(x), fixed = TRUE)
    x <- gsub("\u00a0", " ", x, fixed = TRUE)
    clean_label(x)
  }
  if (is.na(label) || is.na(name)) {
    return(FALSE)
  }
  label <- clean(label)
  name <- clean(name)
  if (identical(label, name)) {
    return(TRUE)
  }
  if (is.na(arm) || !startsWith(label, paste0(name, " ("))) {
    return(FALSE)
  }
  # (the rest of the label: "Arm 1: Name of the arm)")
  rest <- substring(label, nchar(name) + 3)
  arm_end <- paste0(" ", arm, ": ", if (is.na(arm_name)) "" else clean(arm_name), ")")
  if (is.na(arm_name)) {
    grepl(paste0("^[^()]+ ", arm, ": .*\\)$"), rest)
  } else {
    endsWith(rest, arm_end) && nchar(rest) > nchar(arm_end) && !grepl("[()]", substr(rest, 1, nchar(rest) - nchar(arm_end)))
  }
}


#' Factor versions of the event, repeated instrument and data access group read from the REDCap API
#'
#' The labels are read with another request than the values, so its rows can be other ones (the records can change in
#' between). They are not taken by their position: each row has the label of its code (unique event name, instrument or
#' data access group). The label of each code is the one of the rows with that code in the request with labels, and it
#' is checked with the metadata of the project.
#'
#' @param labels Data read with labels (`raw_or_label = "label"`), with `record_id` as its first column.
#' @param data Data read with the raw values, with `record_id` as its first column.
#' @param meta List with the metadata of the `event`, `instrument` and `dag` codes (see `api_event_metadata()` and
#'   `api_code_metadata()`); `NULL` when it couldn't be read.
#' @return `NULL` if the rows of both requests are not the same ones or a label isn't the one of its code. Otherwise, a
#'   data frame with the factor versions present (`redcap_event_name.factor`, `redcap_repeat_instrument.factor` and
#'   `redcap_data_access_group.factor`), whose `unchecked` attribute has the columns with codes instead of labels (their
#'   metadata couldn't be read, or doesn't have some code). An empty column (e.g. the data access groups of a project
#'   without them in its records) is an empty factor, as with the exported files.
#' @noRd
api_label_columns <- function(labels, data, meta = list()) {
  as_text <- function(x) {
    x <- as.character(x)
    x[x %in% ""] <- NA
    x
  }
  # (the number of a repeated instance can be read as text or as a number)
  as_number <- function(x) suppressWarnings(as.numeric(as_text(x)))
  same <- function(a, b) identical(is.na(a), is.na(b)) && all(a[!is.na(a)] == b[!is.na(b)])

  # The rows of both requests: the same records and repetitions
  if (nrow(labels) != nrow(data) || !same(as_text(labels[[1]]), as_text(data[[1]]))) {
    return(NULL)
  }
  if ("Repeat Instance" %in% names(labels) && "redcap_repeat_instance" %in% names(data) &&
    !same(as_number(labels[["Repeat Instance"]]), as_number(data$redcap_repeat_instance))) {
    return(NULL)
  }

  columns <- list(
    event = c("Event Name", "redcap_event_name", "redcap_event_name.factor"),
    instrument = c("Repeat Instrument", "redcap_repeat_instrument", "redcap_repeat_instrument.factor"),
    dag = c("Data Access Group", "redcap_data_access_group", "redcap_data_access_group.factor")
  )
  out <- list()
  unchecked <- character(0)
  for (kind in names(columns)) {
    column <- columns[[kind]]
    if (!column[1] %in% names(labels)) next
    if (!column[2] %in% names(data)) {
      return(NULL)
    }

    label <- as_text(labels[[column[1]]])
    code <- as_text(data[[column[2]]])
    if (!identical(is.na(label), is.na(code))) {
      return(NULL)
    }

    # Label of each code
    codes <- unique(code[!is.na(code)])
    code_labels <- vapply(codes, function(x) {
      y <- unique(label[code %in% x])
      if (length(y) == 1) y else NA_character_
    }, character(1), USE.NAMES = FALSE)
    if (anyNA(code_labels)) {
      return(NULL)
    }

    # Checked with the metadata of the project
    m <- meta[[kind]]
    known <- if (is.null(m)) rep(FALSE, length(codes)) else codes %in% m$code
    for (i in which(known)) {
      j <- match(codes[i], m$code)
      if (!api_label_matches(code_labels[i], m$name[j], m$arm[j], m$arm_name[j])) {
        return(NULL)
      }
    }
    
    # Without metadata for a code, keep the code and report that its label is unverified.
    if (!all(known)) {
      code_labels[!known] <- codes[!known]
      unchecked <- c(unchecked, column[3])
    }

    value <- code_labels[match(code, codes)]
    out[[column[3]]] <- if (all(is.na(value))) {
      factor(as.character(value))
    } else if (kind == "instrument") {
      value
    } else {
      forcats::fct_inorder(value)
    }
  }

  out <- if (length(out) > 0) as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE) else data.frame(row.names = seq_len(nrow(data)))
  attr(out, "unchecked") <- unchecked
  out
}
