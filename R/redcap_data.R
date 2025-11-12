#' Read REDCap data
#'
#' @description
#' `r lifecycle::badge('stable')`
#' This function reads datasets from a REDCap project into R for analysis. Data can be imported from REDCap exported files or via an API connection.
#'
#' **Options for data import:**
#'
#' - **Exported Data**: REDCap's *Export Data* function generates files suitable for R import.
#' - **API Connection**: Use the REDCap API to directly pull data into R.
#'
#' **Steps for using exported data:**
#' 1. Use the REDCap *Export Data* function and choose *R Statistical Software* format.
#' 2. REDCap generates:
#'    - A CSV file with observations.
#'    - An R script to format variables for import.
#' 3. Ensure the exported files, dictionary, and event mapping (if any) are in the same directory.
#'
#' @note To use other package functions effectively, include the `dic_path` argument to load the project dictionary.
#'
#' @param data_path Path to the exported R file for data import (if using exported files).
#' @param dic_path Path to the dictionary file (CSV or XLSX).
#' @param event_path Path to the event-form mapping file (CSV or XLSX) for longitudinal projects (downloadable via the `Designate Instruments for My Events` tab within the `Project Setup` section of REDCap).
#' @param uri The URI of the REDCap project (for API connection).
#' @param token API token for REDCap project access.
#' @param filter_field Fields to include in the import (API connection only).
#' @param survey_fields Logical indicating whether to include survey-related fields (API connection only).
#'
#' @return A list containing:
#'   - `data`: Imported dataset.
#'   - `dictionary`: Variable dictionary.
#'   - `event_form` (if applicable): Event-form mapping for longitudinal projects.
#'
#'
#' @examples
#' \dontrun{
#' # Import using exported files
#'
#' dataset <- redcap_data(
#'   data_path = "C:/Users/username/example.r",
#'   dic_path = "C:/Users/username/example_dictionary.csv",
#'   event_path = "C:/Users/username/events.csv"
#' )
#'
#' # Import using API
#'
#' dataset_api <- redcap_data(
#'   uri = "https://redcap.idibell.cat/api/",
#'   token = "55E5C3D1E83213ADA2182A4BFDEA"
#' ) # This token is fictitious
#' }
#' @export
#' @importFrom stats setNames
#'

redcap_data <- function(data_path = NA, dic_path = NA, event_path = NA, uri = NA, token = NA, filter_field = NULL, survey_fields = FALSE) {

  event_form <- NULL

  # Save the current working directory and ensure it is restored on exit
  oldwd <- getwd()
  on.exit(setwd(oldwd))

  # Warning: data_path, dic_path and another argument are specified.
  if (all(!c(data_path, dic_path) %in% NA) & any(!c(token, uri) %in% NA)) {
    stop("Too many arguments. Use `data_path` and `dic_path` for exported data or `uri` and `token` for API connection.", call. = FALSE)
  }

  # Warning: token, uri and another argument are specified.
  if (all(!c(token, uri) %in% NA) & any(!c(data_path, dic_path) %in% NA)) {
    stop("Too many arguments. Use `uri` and `token` for API connection or `data_path` and `dic_path` for exported data.", call. = FALSE)
  }

  # Warning: either uri or token is specified alone
  if ((!is.na(uri) & is.na(token)) | (is.na(uri) & !is.na(token))) {
    stop("Both `uri` and `token` are required for API connection.", call. = FALSE)
  }

  # Process data from exported files
  if (all(!c(data_path, dic_path) %in% NA) & all(c(token, uri) %in% NA)) {
    # Check the extension of the R data file
    if (!grepl("\\.R$", data_path) & !grepl("\\.r$", data_path)) {
      stop("Unsupported file format. `data_path` must be an R file exported from REDCap.", call. = FALSE)
    }

    # Import the data using the R file
    tmp_env <- new.env()
    file.lines <- scan(data_path, what = character(), skip = 2, sep = "\n", quiet = TRUE)
    file.lines.collapsed <- paste(file.lines, collapse = "\n")
    command <- paste0("dirname(parent.frame(2)$", "data_path", ")")
    setwd(eval(parse(text = command)))
    source(textConnection(file.lines.collapsed), local = tmp_env, encoding = "UTF-8")
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

    # Ensure the primary identifier column is correctly named
    if (names(data)[1] != "record_id") {
      names(data)[1] <- "record_id"
    }

    # Load the dictionary and validate its format
    setwd(oldwd)
    extension_dic <- tools::file_ext(dic_path)
    if (extension_dic == "xlsx") {
      # Read XLSX file
      dic <- openxlsx::read.xlsx(dic_path, colNames = FALSE, detectDates = TRUE, sheet = 1)
    } else if (extension_dic == "csv") {
      # Read CSV file
      dic <- utils::read.csv(dic_path, encoding = "UTF-8", header = FALSE)
    } else {
      stop("Unsupported dictionary format. Only CSV and XLSX are supported.", call. = FALSE)
    }

    # Process the dictionary: clean names
    names(dic) <- dic[1, ]
    dic <- dic[-1, ]
    names(dic) <- janitor::make_clean_names(names(dic))
    names(dic)[1] <- "field_name"
    if (dic[1, 1] != "record_id") {
      dic[1, 1] <- "record_id"
    }

    # Remove descriptive variables from dictionary
    if ("descriptive" %in% dic$field_type) {
      dic <- dic |>
        dplyr::filter(!.data$field_type %in% "descriptive")
    }


    # Indicator of longitudinal projects
    longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

    # Read event file
    if (!is.na(event_path)) {
      setwd(oldwd)

      # Evaluate the extension
      extension <- tools::file_ext(event_path)

      if (extension == "xlsx") {
        # Read XLSX file
        event_form <- openxlsx::read.xlsx(event_path, detectDates = TRUE, sheet = 1)
      } else if (extension == "csv") {
        # Read CSV file
        event_form <- utils::read.csv(event_path, encoding = "UTF-8")
      } else {
        stop("Unsupported event mapping format. Only CSV and XLSX are supported.", call. = FALSE)
      }

      # Error if the exported event_form file is not the correct one! (using events instead of instrument-event)
      if ("event_name" %in% names(event_form)) {
        stop("Invalid file provided in `event_path`.\n\nPlease download the instrument-event mapping file from the 'Designate Instruments for My Events' tab of your REDCap project and try again.",
          call. = FALSE
        )
      }


      # Output
      data_def <- list(data = data, dictionary = dic, event_form = event_form)
    } else {
      # If no event is specified and the project is longitudinal
      if (longitudinal) {
        warning("The project is longitudinal. Consider providing `event_path` for event-form correspondence.", call. = FALSE)
      }

      data_def <- list(data = data, dictionary = dic)
    }
  }

  # Process data from API connection
  if (all(!c(token, uri) %in% NA) & all(c(data_path, dic_path) %in% NA)) {
    # Message: Begin data import process
    message("Importing data from REDCap API...")

    # Read labels from REDCap

    ## Error SSL peer certificate (Github issue #6)

    tryCatch(
      {
        labels <- suppressMessages(REDCapR::redcap_read(redcap_uri = uri, token = token, verbose = FALSE, raw_or_label = "label", raw_or_label_headers = "label", export_data_access_groups = TRUE, export_survey_fields = survey_fields, fields = filter_field)$data)
      },
      error = function(e) {
        if (grepl("REDCap's PHP code is likely trying to process too much text in one bite", e$message) & !is.null(filter_field)) {
          stop("The `record_id` or equivalent variable is missing on the `filter_field` argument.", call. = FALSE)
        } else if (grepl("SSL peer certificate", e$message)) {
          stop("Unable to establish a secure connection.\nConsider bypassing SSL verification:\nhttr::with_config(httr::config(ssl_verifypeer = FALSE), ... <- readcap_data(...)).", call. = FALSE)
        } else {
          stop(e)
        }
      }
    )

    # Ensure labels were retrieved and assign a default column name
    if (nrow(labels) > 0) {
      names(labels)[1] <- "record_id"
    } else {
      stop("Data retrieval is currently unavailable. Please check the status of the REDCap server, confirm the existence of records within the project and verify that you have the necessary data export and API permissions to perform this operation.", call. = FALSE)
    }

    # Save the factor version of the default variables of redcap
    redcap_names <- names(labels |>
      dplyr::select(dplyr::any_of(c("Event Name", "Repeat Instrument", "Data Access Group"))))

    default_names <- data.frame(fac = redcap_names) |>
      dplyr::mutate(corres = dplyr::case_when(
        fac %in% "Event Name" ~ "redcap_event_name.factor",
        fac %in% "Repeat Instrument" ~ "redcap_repeat_instrument.factor",
        fac %in% "Data Access Group" ~ "redcap_data_access_group.factor"
      ))

    rename_redcap <- default_names$fac
    names(rename_redcap) <- default_names$corres

    main_vars <- labels |>
      dplyr::mutate_at(
        redcap_names[!redcap_names %in% "Repeat Instrument"],
        ~ if (all(is.na(.))) . else forcats::fct_inorder(.)
      ) |>
      dplyr::rename(dplyr::all_of(rename_redcap)) |>
      dplyr::select("record_id", default_names$corres)

    # Clean up label names by removing suffixes
    labels <- gsub("\\.{3}\\d+$", "", names(labels))

    # Message: Intermediate status update
    message("Almost done...")

    # Fetch main data using the REDCap API
    data_api <- REDCapR::redcap_read(
      redcap_uri = uri,
      token = token,
      verbose = FALSE,
      raw_or_label = "raw",
      export_data_access_groups = TRUE,
      export_survey_fields = survey_fields,
      fields = filter_field
    )$data

    # Ensure data retrieval was successful
    if (nrow(data_api) > 0) {
      names(data_api)[1] <- "record_id"
    } else {
      stop("Observational data retrieval is currently unavailable. Please verify the status of the REDCap server or confirm the existence of records within the project.", call. = FALSE)
    }

    # Fetch metadata dictionary using the REDCap API
    dic_api <- REDCapR::redcap_metadata_read(
      redcap_uri = uri,
      token = token,
      verbose = FALSE
    )$data

    # Ensure dictionary has appropriate names
    names(dic_api)[1] <- "field_name"
    if (dic_api[1, 1] != "record_id") {
      dic_api[1, 1] <- "record_id"
    }

    # Making sure the names of both dictionaries(exported data and API connection) match
    names(dic_api)[names(dic_api) %in% c("select_choices_or_calculations", "branching_logic", "question_number")] <- c("choices_calculations_or_slider_labels", "branching_logic_show_field_only_if", "question_number_surveys_only")

    # Apply labels to the main dataset
    data_api <- purrr::map2(data_api, as.list(labels), ~ {
      if (!is.null(.y)) {
        attr(.x, "label") <- .y
      }
      .x
    }) |>
      as.data.frame()

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
            warning(stringr::str_glue("The following variable could not be replicated in its factor version: {i}. Please manually create a factor version of this variable named '{i}.factor' to properly execute the rd_transform() function."), call. = FALSE)
          }
        )
      }
    }

    # Merge main_vars with imported data

    data_api <- data_api |>
      dplyr::bind_cols(main_vars |> dplyr::select(-"record_id"))

    # Determine if project is longitudinal
    longitudinal <- ifelse("redcap_event_name" %in% names(data_api), TRUE, FALSE)

    # Handle event path or fetch event-form correspondence via API
    if (!is.na(event_path)) {
      # Warning: event_path not necessary while using API connection
      warning("The event_path argument is not required when using an API connection.")

      setwd(oldwd)

      # Evaluate the extension
      extension <- tools::file_ext(event_path)

      if (extension == "xlsx") {
        # Read XLSX file
        event_form <- openxlsx::read.xlsx(event_path, detectDates = TRUE, sheet = 1)
      } else if (extension == "csv") {
        # Read CSV file
        event_form <- utils::read.csv(event_path, encoding = "UTF-8")
      } else {
        stop("Unsupported file format. Only XLSX and CSV formats are supported.")
      }

      # Error if the exported event_form file is not the correct one! (using events instead of instrument-event)
      if ("event_name" %in% names(event_form)) {
        stop("Invalid file provided in `event_path`.\n\nPlease download the instrument-event mapping file from the 'Designate Instruments for My Events' tab of your REDCap project and try again.",
          call. = FALSE
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
    message("API data import completed.")
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
    var_noevent <- data_def$dictionary |>
      dplyr::filter(!.data$form_name %in% event_form$form) |>
      dplyr::pull(.data$field_name)

    if (length(var_noevent) > 0) {
      warning(stringr::str_glue("The following variables were removed since they are not linked to any event: {var_noevent}"), call. = FALSE)

      var_noevent <- intersect(var_noevent, names(data))

      data_def$data <- data_def$data |>
        dplyr::select(-dplyr::any_of(var_noevent))

      data_def$dictionary <- data_def$dictionary |>
        dplyr::filter(!.data$field_name %in% var_noevent)
    }
  }

  # Reapply labels to the modified dataset
  data_def$data <- data_def$data |>
    labelled::set_variable_labels(.labels = labels |> as.list(), .strict = FALSE)

  # Return the result
  return(data_def)
}
