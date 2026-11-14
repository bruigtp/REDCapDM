#' Identify Missing Events in REDCap Data
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Helps identify records in a REDCap longitudinal project that are missing one or more specified events. Because REDCap typically omits empty events from exports, an event that contains no data for a record will not appear. This function finds those absent events and returns a per-record query table and a summarized HTML report.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param event Character vector with one or more REDCap event names to check for missing records.
#' @param filter Optional. A single character string containing a filter expression to subset the dataset before checking for missing events. Example: \code{"age >= 18"}. It can use objects created in your session or in your function.
#' @param query_name Optional character vector describing each query. Defaults to a standard format: `The event (event_name) is missing`.
#' @param add_to Optional output of a previous `rd_query()` or `rd_event()` to which the new queries are appended.
#' @param addTo `r lifecycle::badge("deprecated")` Use `add_to` instead.
#' @param report_title Optional string specifying the title of the final report. Defaults to `"Report of queries"`.
#' @param report_zeros Logical, include variables with zero queries in the report. Default is `FALSE`.
#' @param link Optional list containing project information (`domain`, `redcap_version`, `proj_id`, `event_id`) to generate clickable links for each query.
#'
#'
#' @return A list of class `REDCapDM_report`, which prints a short summary (see [print.REDCapDM_report()]), with two elements:
#' \describe{
#'   \item{\code{queries}}{A data frame listing records missing the specified events.
#'     Columns: \code{Identifier}, \code{DAG}, \code{Event}, \code{Instrument},
#'     \code{Field}, \code{Repetition}, \code{Description}, \code{Query}, \code{Code},
#'     and optionally \code{Link}. If no queries are found this will be an empty
#'     data frame with the expected columns.}
#'   \item{\code{results}}{An HTML table (knitr::kable styled with kableExtra) summarising
#'     the number of missing records per event. Returned as \code{knitr::kable} (HTML).}
#' }
#'
#'
#' @examples
#' # Minimal reproducible example
#' data0 <- data.frame(
#'   record_id = c("100-1", "100-2", "200-1"),
#'   redcap_event_name = c("baseline_arm_1", "baseline_arm_1", "follow_up_arm_1"),
#'   redcap_event_name.factor = factor(c("Baseline", "Baseline", "Follow-up")),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Suppose we want to check that every record has the follow-up event
#' res <- rd_event(
#'   data = data0,
#'   dic = data.frame(),       # placeholder dictionary
#'   event = "follow_up_arm_1",
#'   report_zeros = TRUE
#' )
#'
#' # Records missing the event:
#' res$queries
#'
#' # HTML summary (in RMarkdown or Viewer)
#' res$results
#'
#' @family query functions
#' @export
#' @importFrom rlang .data

rd_event <- function(project = NULL,
                     data = NULL,
                     dic = NULL,
                     event_form = NULL,
                     event,
                     filter = NA,
                     query_name = NA,
                     add_to = NA,
                     report_title = NA,
                     report_zeros = FALSE,
                     link = list(),
                     addTo = deprecated()) {

  # Environment where the user called the function: the filters and expressions look for their objects there
  user_env <- rlang::caller_env()

  # `addTo` was renamed `add_to` in version 1.2.0
  if (lifecycle::is_present(addTo)) {
    lifecycle::deprecate_warn("1.2.0", "rd_event(addTo)", "rd_event(add_to)")
    add_to <- addTo
  }

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    env_vars <- check_proj(project, data, dic, event_form)

    list2env(env_vars, envir = environment())
  }

  # Ensure both `data` and `dic` are provided; stop if either is missing
  if (is.null(data) || is.null(dic)) {
    rd_abort("Both {.arg data} and {.arg dic} (data and dictionary) arguments must be provided.")
  }

  # Ensure the input data is a data frame
  data <- as.data.frame(data)

  # Error: the project has no events
  if (!any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
    rd_abort("rd_event() needs a longitudinal project: the dataset has no {.code redcap_event_name} column.")
  }
  
  # Error: the event to check is required
  if (missing(event)) {
    rd_abort("Specify the event(s) to check with the {.arg event} argument (unique event name or label).")
  }
  
  # Error: invalid arguments
  check_inputs(data, dic)
  check_strings(event, "event", null_ok = FALSE)
  check_string(filter, "filter", na_ok = TRUE)
  for (arg in c("query_name", "report_title")) {
    check_text(get(arg), arg)
  }
  check_flag(report_zeros, "report_zeros")
  if (is.null(add_to)) {
    add_to <- NA
  }
  if (!(length(add_to) == 1 && !is.list(add_to) && is.na(add_to)) && !(is.list(add_to) && is.data.frame(add_to$queries))) {
    rd_abort("{.arg add_to} must be the output of a previous {.fn rd_query} or {.fn rd_event}.", class = "REDCapDM_error_argument")
  }
  if (!is.list(link)) {
    rd_abort("{.arg link} must be a list with {.code domain}, {.code redcap_version}, {.code proj_id} and {.code event_id}.")
  }

  # Create an empty data frame to store identified queries
  queries <- as.data.frame(matrix(ncol = 10, nrow = 0))
  colnames(queries) <- c("Identifier", "DAG", "Event", "Instrument", "Field", "Repetition", "Description", "Query", "Code", "Link")

  # Rename the first column to "record_id" if necessary
  if (!"record_id" %in% names(data)) {
    names(data)[1] <- "record_id"
  }

  # Save the original dataset for reference
  data0 <- data

  # Apply the filter expression to the dataset if provided
  if (!is.na(filter) && length(filter) == 1) {
    # The filter is evaluated with the columns of the data and the objects of the user's environment
    filtered <- tryCatch(filter_text(data, filter, user_env), error = function(e) e)

    if (inherits(filtered, "error")) {
      filter_error <- trimws(gsub("\\s+", " ", conditionMessage(filtered)))
      rd_abort(c("Invalid {.arg filter} argument logic. Please review and correct the expression.", "x" = "R error: {filter_error}"), class = "REDCapDM_error_argument")
    }
    data <- filtered

    # Warn if the filter results in no observations
    if (nrow(data) == 0) {
      rd_warn("The filter applied does not match any records. Please review the {.arg filter} argument.")
    }
  }

  # Validate that the specified events are present in the dataset
  if (all(!is.na(event))) {
    if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data0))) {
      if (any(!event %in% data0$redcap_event_name) && any(!event %in% data0$redcap_event_name.factor)) {
        rd_abort("One or more specified events do not exist in the dataset. Please review the {.arg event} argument.", class = "REDCapDM_error_argument")
      }
    }

    
    # Events given by their label: use their unique event name
    if ("redcap_event_name.factor" %in% names(data0)) {
      for (k in which(!event %in% data0$redcap_event_name)) {
        names_k <- unique(data0$redcap_event_name[data0$redcap_event_name.factor %in% event[k]])

        if (length(names_k) > 1) {
          rd_abort(c("The event label {.val {event[k]}} is shared by several events ({.val {names_k}}).", "i" = "Please use the unique event name in the {.arg event} argument."), class = "REDCapDM_error_argument")
        }

        event[k] <- names_k
      }
    }

    # Iterate over each specified event to identify missing records
    for (k in seq_along(event)) {
      # Find record IDs associated with the current event (factor form)
      if (any(names(data0) == "redcap_event_name.factor") && all(event %in% data0$redcap_event_name.factor)) {
        ids <- data0$record_id[data0$redcap_event_name.factor %in% event[k]]
      }

      # Find record IDs associated with the current event (raw form)
      if (all(event %in% data0$redcap_event_name)) {
        ids <- data0$record_id[data0$redcap_event_name %in% event[k]]
      }


      # Identify records missing the current event
      raw <- data |>
        dplyr::filter(!data$record_id %in% ids) |>
        dplyr::slice(rep(seq_len(dplyr::n()), each = length(event[k])))

      
      # Multi-arm projects: only the records of the arm of the event can have it
      if ("redcap_event_name" %in% names(data0)) {
        event_raw <- if (event[k] %in% data0$redcap_event_name) event[k] else unique(data0$redcap_event_name[data0$redcap_event_name.factor %in% event[k]])
        arm_k <- event_arm(event_raw, event_form)

        if (length(arm_k) > 0 && !anyNA(arm_k)) {
          arm_records <- data0$record_id[event_arm(data0$redcap_event_name, event_form) %in% arm_k]
          raw <- raw |>
            dplyr::filter(.data$record_id %in% arm_records)
        }
      }

      # If missing records are found, create query entries for them
      if (nrow(raw) > 0) {
        # Assign the current event name to the missing records - raw version
        if ("redcap_event_name" %in% names(raw)) {
          raw$redcap_event_name <- rep(event[k], length(event[k]))
        }

        # Assign the current event name to the missing records - factor version
        if ("redcap_event_name.factor" %in% names(raw)) {
          for (i in seq_len(nrow(raw))) {
            raw$redcap_event_name.factor[i] <- unique(data0$redcap_event_name.factor[data0$redcap_event_name %in% raw$redcap_event_name[i]])
          }
        }

        # Queries
        x <- raw[, c("record_id", grep("redcap", names(raw), value = TRUE))]
        excel <- data.frame(
          Identifier = x[, "record_id"],
          DAG = if ("redcap_data_access_group.factor" %in% names(x)) {
            as.character(x[, "redcap_data_access_group.factor"])
          } else if ("redcap_data_access_group" %in% names(x)) {
            as.character(x[, "redcap_data_access_group"])
          } else {
            "-"
          },
          Event = rep(event[k], length(event[k])),
          Instrument = "-",
          Field = "-",
          Repetition = "-",
          Description = if ("redcap_event_name.factor" %in% names(x)) {
            as.character(x[, "redcap_event_name.factor"])
          } else {
            "-"
          },
          Query = if (!all(is.na(query_name))) {
            if (length(query_name) > 1) {
              query_name[k]
            } else {
              query_name
            }
          } else {
            paste0(
              "The event '", if ("redcap_event_name.factor" %in% names(x)) {
                as.character(x[, "redcap_event_name.factor"])
              } else {
                as.character(x[, "redcap_event_name"])
              },
              "' is missing."
            )
          },
          Code = "",
          stringsAsFactors = FALSE
        )

        # Add a hyperlink for the query if link information is provided
        if (all(c("domain", "redcap_version", "proj_id") %in% names(link))) {
          excel[, "Link"] <- paste0("https://", link[["domain"]], "/redcap_v", link[["redcap_version"]], "/DataEntry/record_home.php?pid=", link[["proj_id"]], "&id=", x[, "record_id"])
        }

        # Append the query to the list of identified queries
        queries <- rbind(queries, excel)
      }
    }
  }

  # Merge with an existing query data frame if specified in 'add_to'
  if (is.list(add_to)) {
    # Save the column names of the existing queries
    col_names <- names(queries)

    # Merge the existing queries with the new ones
    queries <- merge(queries,
      add_to$queries,
      by = intersect(names(add_to$queries), names(queries)),
      all = TRUE
    )

    # Reorder the columns to match the original structure
    queries <- queries |>
      dplyr::select(dplyr::all_of(col_names))
  }

  # Classify each query with a unique code if there are queries present
  if (nrow(queries) != 0) {
    # Sort queries by record identifier
    queries <- sort_by_identifier(queries)

    # Remove duplicate queries and ensure only unique rows are retained
    queries <- queries |>
      dplyr::select(-"Code") |>
      unique()

    # Assign a unique code to each query based on Identifier
    queries <- data.frame(queries |>
      dplyr::group_by(.data$Identifier) |>
      dplyr::mutate(cod = dplyr::row_number()))
    queries$Code <- paste0(as.character(queries$Identifier), "-", queries$cod)
    queries <- queries |> dplyr::select(-"cod")

    # Reorder columns if the 'Link' argument is specified
    if ("Link" %in% names(queries)) {
      queries <- queries |>
        dplyr::select("Identifier":"Query", "Code", "Link")
    }

    # Create a summary report of the queries
    report <- data.frame(
      "var" = queries$Event,
      "descr" = queries$Description
    )

    # If no previous report exists, set events and descriptions as factors using the dictionary
    if (!is.list(add_to)) {
      report$var <- factor(report$var, levels = c(unique(event)))
      report$descr <- factor(report$descr)
    }

    # Include all variables in the report, with or without zero queries, based on 'report_zeros'
    if (report_zeros) {
      report <- report |>
        dplyr::group_by(.data$var, .drop = FALSE) |>
        dplyr::summarise("total" = dplyr::n())
    } else {
      report <- report |>
        dplyr::group_by(.data$var, .drop = TRUE) |>
        dplyr::summarise("total" = dplyr::n())
    }
  } else {
    # Handle cases where there are no queries

    # Notify the user that no queries were identified
    rd_inform("No queries identified.")

    # Create an empty report with placeholders for events
    report <- as.data.frame(matrix(ncol = 2, nrow = length(event)))
    colnames(report) <- c("var", "descr")

    # Populate the report with event names (descriptions are added below)
    report$var <- event
    report$descr <- "-"

    # Set total queries for each event to zero
    report$total <- 0
  }

  # Set the report title or validate the provided title
  if (all(is.na(report_title))) {
    report_title <- "Report of queries"
  } else {
    # Ensure only one report title is provided
    if (length(report_title) > 1) {
      rd_abort("Multiple titles provided for the report. Please specify only one..")
    }
  }

  # Describe each event with its label when available, otherwise with its name
  event_descr <- as.character(report$var)
  if (all(c("redcap_event_name", "redcap_event_name.factor") %in% names(data0))) {
    lookup <- unique(data0[, c("redcap_event_name", "redcap_event_name.factor")])
    labels_found <- as.character(lookup$redcap_event_name.factor)[match(event_descr, lookup$redcap_event_name)]
    event_descr <- ifelse(is.na(labels_found), event_descr, labels_found)
  }

  # Finalize and arrange the report for output
  report <- report |>
    dplyr::mutate(descr = event_descr) |>
    dplyr::select("var", "descr", "total") |>
    dplyr::arrange(dplyr::desc(.data$total))

  # Rename columns for the final report
  names(report) <- c("Events", "Description", "Total")
  rownames(report) <- NULL

  # Generate an HTML table for the report
  result <- knitr::kable(report,
    align = c("ccccc"),
    row.names = FALSE,
    caption = report_title,
    format = "html",
    longtable = TRUE
  )
  result <- style_report(result)

  # Return the queries and the formatted report
  new_report(list(
    queries = queries,
    results = result
  ))
}


#' Arm of each event
#'
#' @param events Unique event names.
#' @param event_form Event-form mapping (its `arm_num` column is used when available).
#' @return The arm of each event, taken from `event_form` or, if not available, from the `_arm_<n>` suffix of its name.
#' @noRd
event_arm <- function(events, event_form = NULL) {
  arm <- rep(NA_character_, length(events))

  if (!is.null(event_form) && all(c("arm_num", "unique_event_name") %in% names(event_form))) {
    arm <- as.character(event_form$arm_num[match(events, event_form$unique_event_name)])
  }

  from_name <- ifelse(grepl("_arm_\\d+$", events), sub("^.*_arm_(\\d+)$", "\\1", events), NA_character_)
  ifelse(is.na(arm), from_name, arm)
}
