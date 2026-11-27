#' Identify Missing Events in REDCap Data
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Helps identify records in a REDCap longitudinal project that are missing one or more specified events. Because REDCap typically omits empty events from exports, an event that contains no data for a record will not appear. This function finds those absent events (not whether the events are complete) and returns a per-record query table and a summarized HTML report.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param event Character vector with one or more REDCap event names to check for missing records.
#' @param filter Optional. A single character string containing a filter expression that selects the records to check. Example: \code{"age >= 18"}. It can use objects created in your session or in your function. Whether each selected record has the event is checked with all its data (also the rows that the filter doesn't select).
#' @param query_name Optional character vector describing each query: one message for all the events, or one message for each event of `event` (in its order). Defaults to a standard format with the unique event name: `The event 'event_name' is missing.`, also for the events with an `NA` message.
#' @param add_to Optional output of a previous `rd_query()` or `rd_event()` to which the new queries are appended. Its queries must be a data frame: the output of `rd_query(by_dag = TRUE)`, whose queries are a list of data frames, can't be used.
#' @param addTo `r lifecycle::badge("deprecated")` Use `add_to` instead.
#' @param report_title Optional string specifying the title of the final report. Defaults to `"Report of queries"`.
#' @param report_zeros Logical, include events with zero queries in the report. Default is `FALSE`.
#' @param link Optional list containing project information (`domain`, `redcap_version`, `proj_id`) to generate a link for each query, which opens the home page of the record in REDCap (unlike `rd_query()`, it doesn't use `event_id`).
#'
#'
#' @return A list of class `REDCapDM_report`, which prints a short summary (see [print.REDCapDM_report()]), with two elements:
#' \describe{
#'   \item{\code{queries}}{A data frame listing records missing the specified events.
#'     Columns: \code{Identifier} (the record identifier, \code{record_id} or else the first field of the dictionary, which \code{filter} can use by its name), \code{DAG}, \code{Event} (the unique event name), \code{EventLabel} (its label), \code{Instrument},
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

  # `addTo` was renamed `add_to` in version 1.1.0
  if (lifecycle::is_present(addTo)) {
    lifecycle::deprecate_warn("1.1.0", "rd_event(addTo)", "rd_event(add_to)")
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
  # (an argument that isn't a data frame, e.g. a function, stops with an explanation)
  check_inputs(data, dic)
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
  check_event_form(event_form)
  data <- plain_data(data)
  dic <- plain_dic(dic)
  check_strings(event, "event", null_ok = FALSE)
  check_string(filter, "filter", na_ok = TRUE)
  for (arg in c("query_name", "report_title")) {
    check_text(get(arg), arg)
    # An empty vector (e.g. character(0)) is not given, as NA
    if (length(get(arg)) == 0) assign(arg, NA)
  }
  # Error: `query_name` has a message for all the events or one message for each event
  if (!all(is.na(query_name)) && !length(query_name) %in% c(1, length(event))) {
    rd_abort(
      c("{.arg query_name} must have one message for all the events or one message for each event ({length(event)}), not {length(query_name)}.",
        "i" = "An {.code NA} message uses the default message of its event."),
      class = "REDCapDM_error_argument"
    )
  }
  check_flag(report_zeros, "report_zeros")
  if (is.null(add_to)) {
    add_to <- NA
  }
  if (!(length(add_to) == 1 && !is.list(add_to) && is.na(add_to)) && !(is.list(add_to) && is.data.frame(add_to$queries))) {
    rd_abort("{.arg add_to} must be the output of a previous {.fn rd_query} or {.fn rd_event}.", class = "REDCapDM_error_argument")
  }
  if (!is.list(link)) {
    rd_abort("{.arg link} must be a list with {.code domain}, {.code redcap_version} and {.code proj_id}.")
  }
  # Elements given as NULL are not given
  link <- link[!vapply(link, is.null, logical(1))]

  # Create an empty data frame to store identified queries
  queries <- as.data.frame(matrix(ncol = 11, nrow = 0))
  colnames(queries) <- c("Identifier", "DAG", "Event", "EventLabel", "Instrument", "Field", "Repetition", "Description", "Query", "Code", "Link")

  # Record identifier (record_id or else the first field of the dictionary, see id_field()): its column keeps its name, so that the filter can use it
  id_var <- id_field(data, dic)

  # Save the original dataset for reference
  data0 <- data

  # The identifiers of the report are numbers only when all the record identifiers of the project are plain numbers
  numeric_ids <- ids_are_numbers(data[[id_var]])

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

  # Events of the project that have no records yet (e.g. a new visit, or an arm without records): they are in the
  # event-form mapping but not in the data. No record has them, so all the records of their arm are missing them.
  events_without_records <- character(0)
  if (!is.null(event_form) && "unique_event_name" %in% names(event_form) && "redcap_event_name" %in% names(data0) &&
    any(as.character(data0$redcap_event_name) %in% event_form$unique_event_name)) {
    events_without_records <- setdiff(unique(as.character(event_form$unique_event_name)), as.character(data0$redcap_event_name))
  }

  # Validate that the specified events are present in the dataset
  if (all(!is.na(event))) {
    if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data0))) {
      if (any(!event %in% c(as.character(data0$redcap_event_name), as.character(data0$redcap_event_name.factor), events_without_records))) {
        rd_abort("One or more specified events do not exist in the dataset. Please review the {.arg event} argument.", class = "REDCapDM_error_argument")
      }
    }

    
    # Events given by their label: use their unique event name
    if ("redcap_event_name.factor" %in% names(data0)) {
      for (k in which(!event %in% c(as.character(data0$redcap_event_name), events_without_records))) {
        names_k <- unique(data0$redcap_event_name[data0$redcap_event_name.factor %in% event[k]])

        if (length(names_k) > 1) {
          rd_abort(c("The event label {.val {event[k]}} is shared by several events ({.val {names_k}}).", "i" = "Please use the unique event name in the {.arg event} argument."), class = "REDCapDM_error_argument")
        }

        event[k] <- names_k
      }
    }

    # Iterate over each specified event to identify missing records
    for (k in seq_along(event)) {
      # Find record IDs associated with the current event (raw form, or factor form if the event was given by its label)
      if (event[k] %in% data0$redcap_event_name || event[k] %in% events_without_records) {
        ids <- data0[[id_var]][data0$redcap_event_name %in% event[k]]
      } else {
        ids <- data0[[id_var]][data0$redcap_event_name.factor %in% event[k]]
      }


      # Identify records missing the current event
      # (.data and .env: a field of the project can have the name of an object of the function, e.g. `data` or `event`)
      raw <- data |>
        dplyr::filter(!.data[[id_var]] %in% .env$ids) |>
        dplyr::slice(rep(seq_len(dplyr::n()), each = length(.env$event[.env$k])))

      
      # Multi-arm projects: only the records of the arm of the event can have it
      if ("redcap_event_name" %in% names(data0)) {
        event_raw <- if (event[k] %in% c(as.character(data0$redcap_event_name), events_without_records)) event[k] else unique(data0$redcap_event_name[data0$redcap_event_name.factor %in% event[k]])
        arm_k <- event_arm(event_raw, event_form)

        if (length(arm_k) > 0 && !anyNA(arm_k)) {
          arm_records <- data0[[id_var]][event_arm(data0$redcap_event_name, event_form) %in% arm_k]
          raw <- raw |>
            dplyr::filter(.data[[id_var]] %in% .env$arm_records)
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
          raw$redcap_event_name.factor <- as.character(raw$redcap_event_name.factor)
          for (i in seq_len(nrow(raw))) {
            label <- unique(as.character(data0$redcap_event_name.factor[data0$redcap_event_name %in% raw$redcap_event_name[i]]))
            # (an event without records has no label in the data: its unique name is used)
            raw$redcap_event_name.factor[i] <- if (length(label) > 0) label[1] else raw$redcap_event_name[i]
          }
        }

        # Queries
        x <- raw[, unique(c(id_var, grep("redcap", names(raw), value = TRUE))), drop = FALSE]
        excel <- data.frame(
          Identifier = x[, id_var],
          DAG = if ("redcap_data_access_group.factor" %in% names(x)) {
            as.character(x[, "redcap_data_access_group.factor"])
          } else if ("redcap_data_access_group" %in% names(x)) {
            as.character(x[, "redcap_data_access_group"])
          } else {
            "-"
          },
          Event = rep(event[k], length(event[k])),
          EventLabel = if ("redcap_event_name.factor" %in% names(x)) {
            as.character(x[, "redcap_event_name.factor"])
          } else {
            rep(event[k], length(event[k]))
          },
          Instrument = "-",
          Field = "-",
          Repetition = "-",
          Description = if ("redcap_event_name.factor" %in% names(x)) {
            as.character(x[, "redcap_event_name.factor"])
          } else {
            "-"
          },
          Query = if (!is.na(query_name[min(k, length(query_name))])) {
            query_name[min(k, length(query_name))]
          } else {
            # (with the unique event name, as `Event`: the label of the event, in `EventLabel`, can be renamed; also the
            # events with an NA message in `query_name`)
            paste0("The event '", event[k], "' is missing.")
          },
          Code = "",
          stringsAsFactors = FALSE
        )

        # Add a hyperlink for the query if link information is provided
        if (all(c("domain", "redcap_version", "proj_id") %in% names(link))) {
          excel[, "Link"] <- paste0("https://", link[["domain"]], "/redcap_v", link[["redcap_version"]], "/DataEntry/record_home.php?pid=", link[["proj_id"]], "&id=", id_text(x[, id_var]))
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

    # (the events of the previous queries with their unique name and their current label)
    add_to$queries <- query_events(add_to$queries, event_labels(data0))

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
    queries <- sort_by_identifier(queries, numeric = numeric_ids)

    # Remove duplicate queries and ensure only unique rows are retained
    queries <- queries |>
      dplyr::select(-"Code") |>
      unique()

    # Assign a unique code to each query based on Identifier
    queries <- data.frame(queries |>
      dplyr::group_by(.data$Identifier) |>
      dplyr::mutate(cod = dplyr::row_number()))
    queries$Code <- paste0(id_text(queries$Identifier), "-", queries$cod)
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
      rd_abort("Multiple titles provided for the report. Please specify only one.")
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
