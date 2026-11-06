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
#' @param filter_field Optional character vector of field names to request from the API.
#' @param survey_fields Logical; include survey-related fields when pulling via API. Default `FALSE`.
#' @param rm_mdc Logical; Removes the Missing Data Codes from de data. Default `TRUE`.
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

redcap_data <- function(data_path = NA, dic_path = NA, event_path = NA, uri = NA, token = NA, sep = ",", filter_field = NULL, survey_fields = FALSE, rm_mdc = TRUE) {

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

  # Warning: either data_path or dic_path is specified alone
  if ((!is.na(data_path) & is.na(dic_path)) | (is.na(data_path) & !is.na(dic_path))) {
    stop("Both `data_path` and `dic_path` are required for exported data.", call. = FALSE)
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
      dic <- utils::read.csv(dic_path, encoding = "UTF-8", header = FALSE, sep = sep)
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

      vars_str <- paste(var_noevent, collapse = ", ")

      warning(
        sprintf("The following variable%s were removed since they are not linked to any event: %s",
                ifelse(length(var_noevent) > 1, "s", ""), vars_str),
        call. = FALSE
      )

      var_noevent <- intersect(var_noevent, names(data))

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

  # Create the column pattern for checkboxes with Missing Data Codes (MDCs)
  checkbox_mdc_pattern <- stringr::regex(
    paste0("___(", paste(missing_codes, collapse = "|"), ")(\\.factor)?$"),
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

  #Identify categorical text field types
  categorical_fields <- data_def$dictionary |>
    dplyr::filter(.data$field_type %in% c("radio", "yesno", "dropdown", "truefalse")) |>
    dplyr::pull(.data$field_name)

  if (rm_mdc) {
    # Include .factor versions in "radio" & "yesno" field types
    categorical_factor_fields <- paste0(categorical_fields, ".factor")
    # Filter only the text validated columns
    all_target_fields <- c(validated_text_fields, categorical_fields, categorical_factor_fields)
    target_columns <- intersect(all_target_fields, colnames(data_def$data))

    removed_checkbox_cols <- colnames(data_def$data)[stringr::str_detect(colnames(data_def$data), checkbox_mdc_pattern)]
    n_mdc_values <- sum(vapply(
      data_def$data[intersect(target_columns, colnames(data_def$data))],
      function(x) sum(stringr::str_trim(as.character(x)) %in% missing_codes, na.rm = TRUE),
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
            val_char <- stringr::str_trim(as.character(.x))
            val_clean <- ifelse(
              val_char %in% missing_codes,
              NA_character_,
              val_char
            )
            return(val_clean)
          }
        )
      )
    if (length(removed_checkbox_cols) > 0 || n_mdc_values > 0) {
      warning("All the Missing Data Codes have been removed.", call. = FALSE)
    }
  } else {

    # Convert into NA all validated text fields.
    text_targets <- intersect(validated_text_fields, colnames(data_def$data))
    if (length(text_targets) > 0) {
      data_def$data <- data_def$data |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(text_targets),
            ~ {
              val_char <- stringr::str_trim(as.character(.x))
              ifelse(toupper(val_char) %in% missing_codes, NA_character_, val_char)
              }
            )
        )
      }
    # Process categorical variables
    cat_targets <- intersect(categorical_fields, colnames(data_def$data))
    for (var in cat_targets) {
      factor_var <- paste0(var, ".factor")

      if (factor_var %in% colnames(data_def$data)) {

        # Make sure that MDC labels are in the .factor version
        data_def$data[[factor_var]] <- dplyr::coalesce(
          as.character(data_def$data[[factor_var]]),
          ifelse(toupper(as.character(data_def$data[[var]])) %in% missing_codes,
                 as.character(data_def$data[[var]]), NA_character_)
        )

        # Assign a numerical sequential code to MDC in the original variable
        orig_vals <- as.character(data_def$data[[var]])
        is_mdc_mask <- toupper(orig_vals) %in% missing_codes

        if (any(is_mdc_mask, na.rm = TRUE)) {

          suppressWarnings({
            numeric_codes <- as.numeric(orig_vals[!is_mdc_mask & !is.na(orig_vals)])
          })
          next_code <- if (length(numeric_codes) > 0) max(numeric_codes, na.rm = TRUE) + 1 else 1

          orig_vals[is_mdc_mask] <- as.character(next_code)
          data_def$data[[var]] <- orig_vals
        }
      }
      }

    # Checkboxes: Remove only empy MDC columns
    cb_mdc_cols <- colnames(data_def$data)[stringr::str_detect(colnames(data_def$data), checkbox_mdc_pattern)]
    cb_mdc_cols <- grep(tolower(checkbox_mdc_pattern), colnames(data_def$data), value = TRUE)
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
  data_def$dictionary <- data_def$dictionary |>
    dplyr::mutate(
      field_label = dplyr::case_when(
        is.na(field_label) | !nzchar(field_label) ~ field_label,

        TRUE ~ {
          lbl_clean <- stringr::str_replace_all(field_label, "<[^>]+>", " ")

          lbl_clean <- stringr::str_replace_all(lbl_clean, "&nbsp;", " ")
          lbl_clean <- stringr::str_replace_all(lbl_clean, "&amp;", "&")
          lbl_clean <- stringr::str_replace_all(lbl_clean, "&lt;", "<")
          lbl_clean <- stringr::str_replace_all(lbl_clean, "&gt;", ">")
          lbl_clean <- stringr::str_replace_all(lbl_clean, "&quot;", '"')
          lbl_clean <- stringr::str_replace_all(lbl_clean, "&#39;", "'")

          lbl_clean <- stringr::str_replace_all(lbl_clean, "[\\r\\n\\t\\u200B\\u200C\\u200D\\uFEFF]", " ")

          stringr::str_squish(lbl_clean)
        }
      )
    )

  # Reapply labels to the modified dataset
  data_def$data <- data_def$data |>
    labelled::set_variable_labels(.labels = labels |> as.list(), .strict = FALSE)

  # Change the labelled class of each column but don't remove the label:
  data_def$data <- data_def$data |>
    dplyr::mutate_all(function(x) {
      class(x) <- setdiff(class(x), "labelled")
      x
    })

  # Return the result
  return(data_def)
}
