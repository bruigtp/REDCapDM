#' Identification of Queries
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Detects and summarizes queries in a REDCap dataset based on specified expressions, filters, or a defined branching logic. Useful for identifying missing values, out-of-range values, or values that do not meet predefined criteria.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param variables Character vector of variable names to check for queries.
#' @param expression Character vector of R expressions to evaluate for each variable.
#' @param negate Logical, if `TRUE`, identifies values that **do not** meet the condition. Default is `FALSE`.
#' @param variables_names Optional character vector of descriptions for each variable. Defaults to the variables labels in the dictionary.
#' @param query_name Optional character vector describing each query. Defaults to a standard format: `The value is [value] and it should not be [expression]`.
#' @param event Required for longitudinal projects to avoid overestimation. REDCap event(s) to analyze.
#' @param instrument Optional REDCap instrument(s) for each variable. Defaults to the instrument reported in the dictionary.
#' @param filter Optional string of filters to apply to the dataset, such as the branching logic of a variable.
#' @param addTo Optional data frame from a previous query report to which the new results can be appended.
#' @param report_title Optional string specifying the title of the final report. Defaults to `"Report of queries"`.
#' @param report_zeros Logical, include variables with zero queries in the report. Default is `FALSE`.
#' @param by_dag Logical, split results by Data Access Group (DAG). Default is `FALSE`.
#' @param link Optional list containing project information (`domain`, `redcap_version`, `proj_id`, `event_id`) to generate clickable links for each query.
#'
#' @details
#' The function performs the following steps:
#' * Applies user-specified expressions to the selected variables to detect queries.
#' * Optionally negates the expressions to find values that **do not** satisfy the condition.
#' * Handles REDCap branching logic, converting it into R-compatible expressions for evaluation.
#' * Applies additional user-specified filters before identifying queries.
#' * Generates structured query results with metadata including:
#'   - Identifier (record_id)
#'   - DAG (if present)
#'   - Event and Instrument
#'   - Field, Repetition, Description, Query statement
#'   - Optional link to REDCap entry
#' * Optionally combines results with previous query outputs using `addTo`.
#' * Produces a summarized report, optionally including variables with zero queries.
#' * Provides warnings for variables with branching logic that could not be automatically evaluated.
#'
#'
#' @return A list containing:
#' \describe{
#'   \item{queries}{A data frame or a list of data frames (if `by_dag = TRUE`) with detailed query information for each record.}
#'   \item{results}{A formatted report (HTML table) summarizing total queries per variable, event, and DAG if applicable.}
#' }
#'
#' @examples
#' \dontrun{
#' # Identify missing values for multiple variables
#' result <- rd_query(covican,
#'   variables = c("copd", "age"),
#'   expression = c("is.na(x)", "x %in% NA"),
#'   event = "baseline_visit_arm_1"
#' )
#' result$results
#'
#' # Identify values exceeding a threshold
#' result <- rd_query(covican,
#'   variables = "age",
#'   expression = "x > 20",
#'   event = "baseline_visit_arm_1"
#' )
#'
#' # Apply a filter to select subset of data
#' result <- rd_query(covican,
#'   variables = "potassium",
#'   expression = "is.na(x)",
#'   event = "baseline_visit_arm_1",
#'   filter = "available_analytics == '1'"
#' )
#' }
#'
#' @export
#' @importFrom rlang .data

rd_query <- function(project = NULL, variables = NA, expression = NA, negate = FALSE, event = NA, filter = NA, addTo = NA, variables_names = NA, query_name = NA, instrument = NA, report_title = NA, report_zeros = FALSE, by_dag = FALSE, link = list(), data = NULL, dic = NULL, event_form = NULL) {

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    env_vars <- check_proj(project, data, dic, event_form)
    # browser()
    list2env(env_vars, envir = environment())
  }

  # Ensure both `data` and `dic` are provided; stop if either is missing
  if (is.null(data) | is.null(dic)) {
    stop("Both `data` and `dic` (data and dictionary) arguments must be provided.")
  }

  # Ensure data and dictionary are data frames
  data <- as.data.frame(data)
  dic <- as.data.frame(dic)

  # Ensure variables is a character string
  variables <- as.character(variables)

  # Initialize the query structure
  queries <- as.data.frame(matrix(ncol = 10, nrow = 0))
  colnames(queries) <- c("Identifier", "DAG", "Event", "Instrument", "Field", "Repetition", "Description", "Query", "Code", "Link")

  # Initialize the zero-query structure
  excel_zero <- as.data.frame(matrix(ncol = 4, nrow = 0))
  colnames(excel_zero) <- c("DAG", "Variables", "Description", "Query")

  # Rename the first column of the dataset to 'record_id' if necessary
  if (all(!names(data) == "record_id")) {
    names(data)[1] <- "record_id"
  }

  # Error: specified variables do not exist in the dataset
  if (any(!variables %in% names(data))) {
    stop("Specified variables do not exist in the dataset. Please review the `variables` argument.")
  }

  # Warning: event not specified
  if (all(is.na(event)) & any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data)) & is.null(event_form)) {
    warning("No event or event-form has been specified. Therefore, the function will automatically consider observations from all events in the dataset. Ensure that the selected variable(s) is(are) collected in all specified events. This will avoid overestimating the number of queries.", call. = FALSE)
  }

  # Handle events automatically based on event_form and dictionary
  if (all(is.na(event)) & !is.null(event_form)) {
    var_form <- dic |>
      dplyr::filter(.data$field_name %in% variables) |>
      dplyr::pull(.data$form_name)

    event <- event_form |>
      dplyr::filter(.data$form %in% var_form) |>
      dplyr::pull(.data$unique_event_name)
  }

  # Apply event filtering if events are specified
  if (all(!is.na(event))) {
    # Saving initial dataset
    data0 <- data

    # Error: event variable is missing in dataset
    if (all(!c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
      stop("Event specified, but no event variable found in the dataset. Please review the `event` argument.")
    }

    # Error: specified events are not in the dataset
    if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
      if (any(!event %in% data$redcap_event_name) & any(!event %in% data$redcap_event_name.factor)) {
        stop("One or more specified events are not found in the dataset. Please review the `event` argument.")
      }
    }

    # Warning: multiple events are specified
    if (length(event) > 1 & is.null(event_form)) {
      warning("Multiple events specified. Ensure variables are collected across all events to avoid overestimation.", .call = FALSE)
    }

    # Filter data by events - factor version
    if ("redcap_event_name.factor" %in% names(data) & all(event %in% data$redcap_event_name.factor)) {
      data <- dplyr::filter(data, data$redcap_event_name.factor %in% event)
    }

    # Filter data by events - raw version
    if (all(event %in% data$redcap_event_name)) {
      data <- dplyr::filter(data, data$redcap_event_name %in% event)
    }

    # Warning: identifiers are missing observations in any event (it only appears when checking for missings - %in%NA)
    if (length(unique(data0$record_id)) != length(unique(data$record_id)) & any(grepl("%in%NA", gsub(" ", "", expression)))) {
      warning("Some identifiers are missing observations in specified events.\nUse 'rd_event' to check which identifiers are missing.", call. = FALSE)
    }
  }

  # Error: by_dag is TRUE but no DAG variable exists
  if (by_dag %in% TRUE & all(!c("redcap_data_access_group", "redcap_data_access_group.factor") %in% names(data))) {
    stop("DAG-based reporting requested, but no DAG variable found in the dataset. Use `by_dag = FALSE` to continue.")
  }

  # Warning: Validate 'link' argument and ensure completeness
  if (!is.null(names(link))) {
    if (!all(c("domain", "redcap_version", "proj_id", "event_id") %in% names(link)) & any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
      stop("Incomplete 'link' argument. To properly use it, please provide domain, redcap_version, project ID, and event ID.", call. = FALSE)
    }

    if (!all(c("domain", "redcap_version", "proj_id") %in% names(link)) & !any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
      stop("Incomplete 'link' argument. To properly use it, please provide domain, redcap_version and project ID.", call. = FALSE)
    }
  }

  # Handle event_id addition based on the link argument
  if (!is.null(link[["event_id"]])) {
    if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
      # The event argument is specified and the event_id is specified using the events in the raw version
      if (all(event %in% names(link[["event_id"]])) & all(event %in% data[, "redcap_event_name"])) {
        event_link <- data.frame(
          "name" = names(link[["event_id"]]),
          "event_id" = as.numeric(link[["event_id"]])
        )
        data <- merge(data, event_link, by.x = "redcap_event_name", by.y = "name")
      }

      # The event argument is specified and the event_id is specified using the events in the factor version
      if (all(event %in% names(link[["event_id"]])) & all(event %in% data[, "redcap_event_name.factor"])) {
        event_link <- data.frame(
          "name" = names(link[["event_id"]]),
          "event_id" = as.numeric(link[["event_id"]])
        )
        data <- merge(data, event_link, by.x = "redcap_event_name.factor", by.y = "name")
      }

      # The event argument is not specified and the event_id is specified using the events in the raw version
      if (all(is.na(event) & all(names(data[, "redcap_event_name"] %in% names(link[["event_id"]]))))) {
        event_link <- data.frame(
          "name" = names(link[["event_id"]]),
          "event_id" = as.numeric(link[["event_id"]])
        )
        data <- merge(data, event_link, by.x = "redcap_event_name", by.y = "name")
      }

      # The event argument is not specified and the event_id is specified using the events in the factor version
      if (all(is.na(event) & all(names(data[, "redcap_event_name.factor"] %in% names(link[["event_id"]]))))) {
        event_link <- data.frame(
          "name" = names(link[["event_id"]]),
          "event_id" = as.numeric(link[["event_id"]])
        )
        data <- merge(data, event_link, by.x = "redcap_event_name.factor", by.y = "name")
      }
    } else {
      # There is not an event name variable in the dataset
      if (length(link[["event_id"]]) == 1) {
        data[, "event_id"] <- as.numeric(link[["event_id"]])
      } else {
        # Error: More than one event_id is specified and the project is non-longitudinal
        stop("Non-longitudinal project. Please provide only one event ID.", .call = FALSE)
      }
    }
  }

  # Ensure expressions and variables match in length
  if (length(variables) != length(expression)) {
    if (length(variables) > length(expression)) {
      warning(
        paste0(
          "Number of variables (", length(variables),
          ") is greater than the number of expressions (", length(expression), "). ",
          "The first expression will be applied to all variables."
        ),
        call. = FALSE
      )

      # Repeat the first expression for remaining variables
      expression <- rep(expression[1], length(variables))
    } else {
      warning(
        paste0(
          "Number of expressions (", length(expression),
          ") is greater than the number of variables (", length(variables), "). ",
          "The first variable will be used for all expressions."
        ),
        call. = FALSE
      )

      # Repeat the first variable for remaining expressions
      variables <- rep(variables[1], length(expression))
    }
  }

  # Apply filter logic if specified
  if (all(!is.na(filter)) & length(filter) == 1) {
    # Error: logic used in the filter is incorrect
    command <- paste0("data", "<-dplyr::filter(data,", filter, ")")

    evaluation <- try(eval(parse(text = command)), silent = TRUE)

    if (inherits(evaluation, "try-error")) {
      stop("Invalid filter logic. Please review the `filter` argument.")
    } else {
      eval(parse(text = command))
    }

    # Error: filter results in zero observations
    if (nrow(data) == 0) {
      warning("Filter applied successfully, but no matching observations found. Please review the `filter` argument.")
    }
  }

  # Handle branching logic for variables
  var_logic <- variables[which(variables %in% dic[!dic$branching_logic_show_field_only_if %in% c(NA, ""), "field_name"])]

  # Objects to track compatible logic, unconverted logic, and branching evaluations
  compatible <- NULL
  logics <- NULL
  br_eval <- NULL

  if (length(var_logic) > 0) {
    # Create a data frame of variables, labels, and their branching logic
    branch <- data.frame(
      var = dic$field_name[dic$field_name %in% gsub("___\\d*$", "", var_logic)],
      label = gsub("\\s+", "", gsub("<.*?>", "", dic$field_label[dic$field_name %in% gsub("___\\d*$", "", var_logic)])),
      branch = dic$branching_logic_show_field_only_if[dic$field_name %in% gsub("___\\d*$", "", var_logic)]
    )

    # Save the original branching logic data for reference
    branch0 <- branch

    # Convert REDCap logic into R-compatible logic
    if ((!is.null(event_form) | all(!c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) & all(stringr::str_detect(branch$branch, paste(c("\\[", "\\]"), collapse = "|")))) {
      for (j in seq_len(nrow(branch))) {
        evaluation <- try(rd_rlogic(data = data, dic = dic, event_form = event_form, logic = branch$branch[j], var = branch$var[j])$rlogic, silent = TRUE)

        if (!inherits(evaluation, "try-error")) {
          branch$branch[j] <- rd_rlogic(data = data, dic = dic, event_form = event_form, logic = branch$branch[j], var = branch$var[j])$rlogic
        } else {
          logics <- rbind(logics, branch$var[j])
        }
      }

      # Issue warning for logic that could not be converted
      if (!is.null(logics) & nrow(data) > 0) {
        warning(stringr::str_glue("The branching logic of the following variables could not be converted into R logic: {paste0('\n - ', unique(logics))}\nCheck the results element of the output(...$results) for details."), call. = FALSE)
      }
    }

    # Reorganize data frames for reporting
    rownames(branch0) <- NULL
    names(branch0) <- c("Variable", "Label", "Branching logic")
    rownames(branch) <- NULL
    names(branch) <- c("Variable", "Label", "Branching logic")

    ## Warning branching logic in multi-event REDCap projects
    if (is.null(event_form) & any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
      warning("One or more variables checked for missing values have branching logic.\nSee results (...$results) for details.", call. = FALSE)
    }
  }

  # Apply expression filters to variables
  for (i in seq_along(expression)) {
    # If the chosen variable has a branching logic
    if (length(var_logic) > 0 & (!is.null(event_form) | all(!c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) & any(!variables[i] %in% logics)) {
      if (any(variables[i] == var_logic)) {
        # Retrieve branching logic for the current variable
        logic <- branch |>
          dplyr::filter(.data$Variable %in% variables[i]) |>
          dplyr::pull(.data$`Branching logic`)

        branching <- NULL

        # Evaluate branching logic
        command <- paste0("branching", "<-dplyr::filter(data,", gsub(pattern = "data\\$", replacement = "data$", x = logic), ")")

        eval(parse(text = command))

        if (nrow(branching) > 0) {
          compatible <- rbind(compatible, variables[i])
          raw0 <- branching
        } else {
          br_eval <- rbind(br_eval, variables[i])
          raw0 <- data
        }
      } else {
        raw0 <- data
      }
    } else {
      raw0 <- data
    }

    # Apply custom expression filters
    x <- variables[i]
    command <- paste0("raw", "<-dplyr::filter(raw0,", gsub("\\bx\\b", x, expression[i]), ")")
    eval(parse(text = command))

    # Transform the expression so it does not contain "x"
    expression[i] <- gsub("\\bx\\b", "", expression[i])

    # Handle negation if specified
    if (negate == TRUE) {
      raw <- suppressMessages(dplyr::anti_join(raw0, raw))
    }

    # Handle multiple filters
    if (all(!filter %in% NA) & length(filter) > 1) {
      # If the number of filters is equal to the number of variables
      if (length(filter) == length(variables)) {
        # Apply each filter individually
        command <- paste0("definitive", "<-dplyr::filter(raw,", filter[i], ")")
        eval(parse(text = command))

        # Filtering results in zero observations
        if (nrow(data) == 0) {
          warning(
            paste0(
              "The filter for variable '", variables[i], "' resulted in no matching observations. Verify the filter logic."
            ),
            call. = FALSE
          )
        }
      } else {
        # Error: number of filters is different from the number of variables
        stop("Mismatch in the number of filters and variables. Ensure each variable has a corresponding filter.", call. = FALSE)
      }
    } else {
      definitive <- raw
    }


    # Generate query structure and metadata
    if (nrow(definitive) > 0) {
      # Construct query data frame
      x <- definitive[, c("record_id", grep("redcap", names(definitive), value = TRUE), variables[i])]

      # Add event_id when the link argument is specified
      if (all(c("domain", "redcap_version", "proj_id", "event_id") %in% names(link))) {
        x[, "event_id"] <- definitive[, "event_id"]
      }

      # Filling the structure data frame with data
      excel <- data.frame(
        Identifier = x[, "record_id"],
        DAG = if (any(c("redcap_data_access_group", "redcap_data_access_group.factor") %in% names(x))) {
          if ("redcap_data_access_group.factor" %in% names(x)) {
            as.character(x[, "redcap_data_access_group.factor"])
          } else {
            as.character(x[, "redcap_data_access_group"])
          }
        } else {
          "-"
        },
        Event = if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(x))) {
          if ("redcap_event_name.factor" %in% names(x)) {
            as.character(x[, "redcap_event_name.factor"])
          } else {
            as.character(x[, "redcap_event_name"])
          }
        } else {
          "-"
        },
        Instrument = if (all(is.na(instrument))) {
          if (gsub("___.*$", "", variables[i]) %in% dic$field_name) {
            gsub("_", " ", stringr::str_to_sentence(dic[dic[, "field_name"] %in% gsub("___.*$", "", variables[i]), "form_name"]))
          } else {
            "-"
          }
        } else {
          if (length(instrument) == 1) {
            instrument
          } else {
            if (length(instrument) == length(variables) & length(instrument) > 1) {
              instrument[i]
            } else {
              if (length(instrument) != length(variables) & length(instrument) > 1) {
                stop("Multiple instruments specified, but the number of instruments is different from the number of variables. Please match each variable to each instrument.", call. = FALSE)
              }
            }
          }
        },
        Field = variables[i],
        Repetition = if (any(c("redcap_repeat_instrument", "redcap_repeat_instance") %in% names(x))) {
          if ("redcap_repeat_instrument" %in% names(x) & all(is.na(x[, "redcap_repeat_instrument"]))) {
            if ("redcap_repeat_instance" %in% names(x) & all(is.na(x[, "redcap_repeat_instance"]))) {
              "-"
            } else {
              paste0(x[, "redcap_repeat_instance"])
            }
          } else {
            paste0(x[, "redcap_repeat_instrument"], "-", x[, "redcap_repeat_instance"])
          }
        } else {
          "-"
        },
        Description = if (all(is.na(variables_names))) {
          if (gsub("___.*$", "", variables[i]) %in% dic$field_name) {
            trimws(gsub("<.*?>", "", as.character(dic[dic[, "field_name"] %in% gsub("___.*$", "", variables[i]), "field_label"])))
          } else {
            "-"
          }
        } else {
          if (length(variables_names) == 1) {
            variables_names
          } else {
            if (length(variables_names) == length(variables) & length(variables_names) > 1) {
              variables_names[i]
            } else {
              if (length(variables_names) != length(variables) & length(variables_names) > 1) {
                stop("Multiple variables names specified, but the number of names is different from the number of variables. Please match each variable to each name.", call. = FALSE)
              }
            }
          }
        },
        Query = if (all(!is.na(query_name))) {
          if (length(query_name) %in% 1) {
            query_name
          } else {
            if (length(query_name) %in% length(variables) & length(query_name) > 1) {
              query_name[i]
            } else {
              if (!length(query_name) %in% length(variables) & length(query_name) > 1) {
                stop("Multiple query names specified, but the number of query names is different from the number of variables. Please match each variable to each query name.", call. = FALSE)
              }
            }
          }
        } else {
          if (negate == TRUE) {
            trimws(gsub("  ", " ", paste("The value is", x[, variables[i]], "and it should be", stringi::stri_replace_all_regex(gsub(" ", "", expression[i]), pattern = c("<", ">", "<=", ">=", "&", "\\|", "==", "!=", "%in%NA", "%in%", "%nin%", "is.na\\(\\)"), replacement = c(" less than ", " greater than ", " less than or equal to ", " greater than or equal to ", " and ", " or ", " equal to ", " not equal to ", " missing ", " equal to ", " not equal to ", " missing "), vectorize = FALSE))))
          } else {
            trimws(gsub("  ", " ", paste("The value is", x[, variables[i]], "and it should not be", stringi::stri_replace_all_regex(gsub(" ", "", expression[i]), pattern = c("<", ">", "<=", ">=", "&", "\\|", "==", "!=", "%in%NA", "%in%", "%nin%", "is.na\\(\\)"), replacement = c(" less than ", "greater than ", " less than or equal to ", " greater than or equal to ", " and ", " or ", " equal to ", " not equal to ", " missing ", " equal to ", " not equal to ", " missing "), vectorize = FALSE))))
          }
        },
        Code = "",
        stringsAsFactors = FALSE
      )

      # Add link to each query
      if (all(c("domain", "redcap_version", "proj_id", "event_id") %in% names(link)) & any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
        excel[, "Link"] <- paste0("https://", link[["domain"]], "/redcap_v", link[["redcap_version"]], "/DataEntry/index.php?pid=", link[["proj_id"]], "&event_id=", x[, "event_id"], "&page=", dic[dic[, "field_name"] %in% gsub("___.*$", "", variables[i]), "form_name"], "&id=", x[, "record_id"])
      }

      if (all(c("domain", "redcap_version", "proj_id") %in% names(link)) & !any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
        excel[, "Link"] <- paste0("https://", link[["domain"]], "/redcap_v", link[["redcap_version"]], "/DataEntry/index.php?pid=", link[["proj_id"]], "&page=", dic[dic[, "field_name"] %in% gsub("___.*$", "", variables[i]), "form_name"], "&id=", x[, "record_id"])
      }

      # Adding each identified query to the queries data frame
      queries <- rbind(queries, excel)
    } else {

      # Handle cases with zero queries
      excel <- tibble::tibble(
        DAG = if (any(c("redcap_data_access_group", "redcap_data_access_group.factor") %in% names(data)) & nrow(data) > 0) {
          if ("redcap_data_access_group.factor" %in% names(data)) {
            if (all(is.na(event))) {
              unique(as.character(data[, "redcap_data_access_group.factor"]))
            } else {
              unique(as.character(data[data$redcap_event_name %in% event | data$redcap_event_name.factor %in% event, "redcap_data_access_group.factor"]))
            }
          } else {
            if (all(is.na(event))) {
              unique(as.character(data[, "redcap_data_access_group"]))
            } else {
              unique(as.character(data[data$redcap_event_name %in% event | data$redcap_event_name.factor %in% event, "redcap_data_access_group"]))
            }
          }
        } else {
          "-"
        },
        Variables = variables[i],
        Description = if (all(is.na(variables_names))) {
          if (gsub("___.*$", "", variables[i]) %in% dic$field_name) {
            trimws(gsub("<.*?>", "", dic[dic[, "field_name"] %in% gsub("___.*$", "", variables[i]), "field_label"]))
          } else {
            "-"
          }
        } else {
          if (length(variables_names) == 1) {
            variables_names
          } else {
            if (length(variables_names) == length(variables) & length(variables_names) > 1) {
              variables_names[i]
            } else {
              if (length(variables_names) != length(variables) & length(variables_names) > 1) {
                stop("Multiple variables names specified, but the number of names is different from the number of variables. Please match each variable to each name.", call. = FALSE)
              }
            }
          }
        },
        Event = if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data)) & nrow(data) > 0) {
          if ("redcap_event_name.factor" %in% names(data)) {
            unique(as.character(data[, "redcap_event_name.factor"]))
          } else {
            unique(as.character(data[, "redcap_event_name"]))
          }
        } else {
          "-"
        },
        Query = if (all(!is.na(query_name))) {
          if (length(query_name) == 1) {
            query_name
          } else {
            if (length(query_name) == length(variables) & length(query_name) > 1) {
              query_name[i]
            } else {
              if (length(query_name) != length(variables) & length(query_name) > 1) {
                stop("Multiple query names specified, but the number of query names is different from the number of variables. Please match each variable to each query name.", call. = FALSE)
              }
            }
          }
        } else {
          if (negate == TRUE) {
            gsub("  ", " ", paste("The value should be", stringi::stri_replace_all_regex(gsub(" ", "", expression[i]), pattern = c("<", ">", ">=", "<=", "&", "\\|", "==", "!=", "%in%NA", "%in%", "%nin%", "is.na\\(\\)"), replacement = c(" less than ", " greater than ", " greater than or equal to ", " less than or equal to ", " and ", " or ", " equal to ", " not equal to ", " missing ", " equal to ", " not equal to ", " missing "), vectorize = FALSE)))
          } else {
            gsub("  ", " ", paste("The value should not be", stringi::stri_replace_all_regex(gsub(" ", "", expression[i]), pattern = c("<", ">", ">=", "<=", "&", "\\|", "==", "!=", "%in%NA", "%in%", "%nin%", "is.na\\(\\)"), replacement = c(" less than ", " greater than ", " greater than or equal to ", " less than or equal to ", " and ", " or ", " equal to ", " not equal to ", " missing ", " equal to ", " not equal to ", " missing "), vectorize = FALSE)))
          }
        },
        stringsAsFactors = FALSE
      ) |>
        as.data.frame()

      # Adding each variable with zero queries to the data frame
      excel_zero <- rbind(excel_zero, excel)
    }
  }

  # Warnings: variables where branching logic could not be applied automatically
  if (!is.null(br_eval) & nrow(data) > 0) {
    warning(stringr::str_glue("The branching logic for the following variables could not be applied automatically: {paste0('\n- ', unique(br_eval))}\nCheck the results element of the output(...$results) for details."), call. = FALSE)
  }

  # Report variables where branching logic was applied automatically
  if (!is.null(compatible)) {
    if (length(compatible) > 1) {
      sentence <- stringr::str_glue("The branching logic of the following variables were applied automatically: {paste0('\n- ', unique(compatible), collapse = '')}")
    } else {
      sentence <- stringr::str_glue("The branching logic of the following variables were applied automatically: {unique(compatible)}")
    }
    warning(sentence, call. = FALSE)
  }


  # If 'addTo' argument is specified, merge previous queries
  if (all(!is.na(addTo))) {
    # Add 'Link' column if it exists in the addTo$queries data frame
    if ("Link" %in% names(addTo$queries)) {
      col_names <- c(names(queries), "Link")
    } else {
      col_names <- names(queries)
    }

    # Merge queries and add 'Link' if present
    queries <- merge(queries,
      addTo$queries,
      by = intersect(names(addTo$queries), names(queries)),
      all = TRUE
    )
    queries <- queries |>
      dplyr::select(dplyr::all_of(col_names))
  }

  # Classify each query with its own code
  if (nrow(queries) > 0) {
    # Sort data by record ID
    if (all(grepl("-", queries$Identifier))) {
      queries <- queries |>
        tidyr::separate("Identifier", c("center", "id"), sep = "([-])", remove = FALSE)
      queries[, "center"] <- as.numeric(queries[, "center"])
      queries[, "id"] <- as.numeric(queries[, "id"])
      queries <- queries[order(queries[, "center"], queries[, "id"]), ]
      rownames(queries) <- NULL
      queries <- queries |> dplyr::select(-"center", -"id")
    } else {
      queries$Identifier <- as.numeric(queries$Identifier)
      queries <- queries[order(queries$Identifier), ]
    }

    # Remove duplicates and add code for each query
    queries <- unique(queries |>
      dplyr::select(-"Code"))
    queries <- data.frame(queries |>
      dplyr::group_by(.data$Identifier) |>
      dplyr::mutate(cod = 1:dplyr::n()))
    queries$Code <- paste0(as.character(queries$Identifier), "-", queries$cod)
    queries <- queries |> dplyr::select(-"cod")

    # Reorder columns if 'Link' was included
    if ("Link" %in% names(queries)) {
      queries <- queries |>
        dplyr::select("Identifier":"Query", "Code", "Link")
    }

    # Build the report
    report <- data.frame(
      "dag" = queries$DAG,
      "var" = queries$Field,
      "descr" = queries$Description,
      "event" = queries$Event,
      "query_descr" = gsub(" is .* it", "", queries$Query)
    )

    # Convert variables and descriptions to factors if necessary
    if (all(addTo %in% NA & variables_names %in% NA)) {
      report$var <- factor(report$var,
        levels = c(unique(variables))
      )
      report$descr <- factor(report$descr,
        levels = c(unique(trimws(gsub("<.*?>", "", dic$field_label[dic$field_name %in% gsub("___.*$", "", variables)]))))
      )

      # If the project has events, convert them to factors
      if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
        report$event <- factor(report$event,
          levels = if ("redcap_event_name.factor" %in% names(data)) {
            unique(as.character(data$redcap_event_name.factor))
          } else {
            unique(as.character(data$redcap_event_name))
          }
        )
      }
    }

    # Handle cases where only variable names are specified
    if (all(addTo %in% NA & !is.na(variables_names))) {
      report$var <- factor(report$var,
        levels = c(unique(variables))
      )
      report$descr <- factor(report$descr,
        levels = c(unique(variables_names))
      )

      # If our project has events, we also convert them to factors
      if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
        report$event <- factor(report$event,
          levels = if ("redcap_event_name.factor" %in% names(data)) {
            unique(as.character(data$redcap_event_name.factor))
          } else {
            unique(as.character(data$redcap_event_name))
          }
        )
      }
    }


    # Handle variables with zero queries
    if (report_zeros == TRUE) {
      # Total number of queries by variable, event and DAG
      report <- report |>
        dplyr::group_by(.data$dag, .data$var, .data$event, .data$query_descr, .drop = FALSE) |>
        dplyr::summarise("total" = dplyr::n(), .groups = "keep")

      # Add query description for variables with zero queries
      if (any(report$total %in% 0)) {
        # Add the query description
        report$var_event <- paste0(report$var, "_", report$event)
        complete_vars <- unique(as.character(report$var_event[report$total != 0]))
        zero_vars <- unique(as.character(report$var_event[!report$var_event %in% complete_vars]))
        report <- report |>
          dplyr::filter((.data$var_event %in% complete_vars & .data$total != 0) | .data$var_event %in% zero_vars)

        for (i in zero_vars) {
          report$query_descr[report$var_event %in% i] <- paste0(
            "The value should not be ",
            stringi::stri_replace_all_regex(gsub(" ", "", expression[which(variables %in% report$var[which(report$var_event %in% i)])]), pattern = c("<", ">", ">=", "<=", "&", "\\|", "==", "!=", "%in%NA", "%in%", "%nin%", "is.na\\(\\)"), replacement = c(" less than ", " greater than ", " greater than or equal to ", " less than or equal to ", " and ", " or ", " equal to ", " not equal to ", " missing ", " equal to ", " not equal to ", " missing "), vectorize = FALSE)
          )
        }

        report <- report |> dplyr::select(-"var_event")

        # Add the variables description
        report$descr <- purrr::map_chr(gsub("___.*$", "", report$var), function(x) {
          if (x %in% dic$field_name) {
            # Truncate description name if it exceeds 50 characters
            name <- gsub("<.*?>", "", dic$field_label[dic$field_name %in% x])
            stringr::str_trunc(name, 50)
          } else {
            "-"
          }
        })
      }
    } else {
      # Report of the variables with identified queries (eliminating the ones with zero queries)
      report <- report |>
        dplyr::group_by(.data$var, .data$descr, .data$event, .data$query_descr, .data$dag, .drop = TRUE) |>
        dplyr::summarise("total" = dplyr::n(), .groups = "keep") |>
        dplyr::filter("total" != 0)
    }

    # Truncate descriptions and variable names if too long
    report$descr <- as.character(report$descr)
    report$descr <- stringr::str_trunc(report$descr, 50)

    report$var <- as.character(report$var)
    report$var <- stringr::str_trunc(report$var, 26)

    # Sort the report by the total number of queries
    report <- as.data.frame(report)
    report <- report[order(as.numeric(report$total), decreasing = TRUE), ]

    # Reorder columns and prepare the final report for output
    report <- report |> dplyr::select("dag", "var", "descr", "event", "query_descr", "total")
    names(report) <- c("DAG", "Variables", "Description", "Event", "Query", "Total")
    report[, "Query"] <- gsub("&", " & ", gsub("\\|", " \\| ", report[, "Query"]))
    rownames(report) <- NULL
  } else {
    # If no queries identified, return a report with zero queries
    if (nrow(data) > 0) {
      message("No queries were identified in the dataset.")
    }

    report <- excel_zero
    report$Total <- 0
    rownames(report) <- NULL
  }

  # Handle report title
  if (all(is.na(report_title))) {
    report_title <- "Report of queries"
  } else {
    if (length(report_title) > 1) {
      stop("Multiple report titles found. Please specify only one.", call. = FALSE)
    }
  }

  # Add branching logic information to the report if applicable
  if (length(var_logic) > 0 & (!is.null(logics) | is.null(event_form)) & (!is.null(br_eval) | any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data)))) {
    # Truncate variable names to ensure they don't exceed 26 characters
    branch0$Variable <- stringr::str_trunc(branch0$Variable, 26)

    # Merge branching logic into the report
    report <- merge(report,
      branch0 |> dplyr::select("Variable", "Branching logic"),
      by.x = "Variables", by.y = "Variable", all.x = TRUE
    )

    # Replace NA values in "Branching logic" with a placeholder "-"
    report[, "Branching logic"] <- stringr::str_replace_na(report[, "Branching logic"], "-")
  }

  # Reorder the report by descending total count
  report <- report |>
    dplyr::arrange(dplyr::desc(.data$Total))

  # Handle the case where the 'by_dag' argument is TRUE
  if (by_dag %in% TRUE) {
    report[is.na(report)] <- "-"
    report_dag <- split(report, f = report$DAG)

    # Build individual reports for each DAG
    for (i in seq_along(report_dag)) {
      report_dag[[i]] <- report_dag[[i]] |>
        dplyr::select("DAG", names(report))

      # Create HTML table for each DAG with proper styling and caption
      report_dag[[i]] <- knitr::kable(report_dag[[i]],
        align = "ccccc",
        row.names = FALSE,
        caption = report_title,
        format = "html",
        longtable = TRUE
      )
      report_dag[[i]] <- kableExtra::kable_styling(report_dag[[i]],
        bootstrap_options = c("striped", "condensed"),
        full_width = FALSE
      )
      report_dag[[i]] <- kableExtra::row_spec(report_dag[[i]], 0,
        italic = FALSE,
        extra_css = "border-bottom: 1px solid grey"
      )
    }

    # Prepare final output in the 'by_dag' format
    def <- list(
      queries = split(queries, f = queries$DAG),
      results = report_dag
    )
  } else {
    # Handle case when 'by_dag' argument is FALSE
    # Ensure branching logic is retained if applicable
    if (length(var_logic) > 0 & (!is.null(logics) | is.null(event_form)) & (!is.null(br_eval) | any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data)))) {
      report <- report |>
        dplyr::select(-"DAG") |>
        dplyr::group_by(.data$Variables, .data$Description, .data$Event, .data$Query, .data$`Branching logic`, .drop = FALSE) |>
        dplyr::summarise("Total" = sum(.data$Total), .groups = "keep") |>
        dplyr::select("Variables", "Description", "Event", "Query", "Total", "Branching logic")
    } else {
      report <- report |>
        dplyr::select(-"DAG") |>
        dplyr::group_by(.data$Variables, .data$Description, .data$Event, .data$Query, .drop = FALSE) |>
        dplyr::summarise("Total" = sum(.data$Total), .groups = "keep")
    }

    # If report_zeros is TRUE, handle zero total values
    if (any(!report$Total %in% 0)) {
      if (report_zeros == TRUE) {
        report <- report |>
          dplyr::arrange(dplyr::desc(.data$Total))

        # Print queries with zero totals (if any)
        report$Query[report$Total %in% 0]
      } else {
        report <- report |>
          dplyr::arrange(dplyr::desc(.data$Total)) |>
          dplyr::filter(.data$Total != 0)
      }
    }

    # Replace NAs with "-" in the report
    report[is.na(report)] <- "-"

    # Generate and style the HTML table for the report
    result <- knitr::kable(report, "pipe",
      align = "ccccc",
      caption = report_title
    )
    viewer <- knitr::kable(report,
      align = "ccccc",
      row.names = FALSE,
      caption = report_title,
      format = "html",
      longtable = TRUE
    )
    viewer <- kableExtra::kable_styling(viewer,
      bootstrap_options = c("striped", "condensed"),
      full_width = FALSE
    )
    viewer <- kableExtra::row_spec(viewer, 0,
      italic = FALSE,
      extra_css = "border-bottom: 1px solid grey"
    )

    # Prepare final output without DAG-specific split
    def <- list(
      queries = dplyr::tibble(queries),
      results = viewer
    )
  }

  # Return the final report output
  def
}
