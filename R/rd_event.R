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
#' @param filter Optional. A single character string containing a filter expression to subset the dataset before checking for missing events. Example: \code{"age >= 18"}.
#' @param query_name Optional character vector describing each query. Defaults to a standard format: `The event (event_name) is missing`.
#' @param addTo Optional data frame from a previous query report to which the new results can be appended.
#' @param report_title Optional string specifying the title of the final report. Defaults to `"Report of queries"`.
#' @param report_zeros Logical, include variables with zero queries in the report. Default is `FALSE`.
#' @param link Optional list containing project information (`domain`, `redcap_version`, `proj_id`, `event_id`) to generate clickable links for each query.
#'
#'
#' @return A named list with two elements:
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
#' @export
#' @importFrom rlang .data

rd_event <- function(project = NULL,
                     data = NULL,
                     dic = NULL,
                     event_form = NULL,
                     event,
                     filter = NA,
                     query_name = NA,
                     addTo = NA,
                     report_title = NA,
                     report_zeros = FALSE,
                     link = list()) {

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    env_vars <- check_proj(project, data, dic, event_form)

    list2env(env_vars, envir = environment())
  }

  # Ensure both `data` and `dic` are provided; stop if either is missing
  if (is.null(data) | is.null(dic)) {
    stop("Both `data` and `dic` (data and dictionary) arguments must be provided.")
  }

  # Ensure the input data is a data frame
  data <- as.data.frame(data)

  # Error: the project has no events
  if (!any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
    stop("rd_event() needs a longitudinal project: the dataset has no `redcap_event_name` column.", call. = FALSE)
  }

  # Create an empty data frame to store identified queries
  queries <- as.data.frame(matrix(ncol = 10, nrow = 0))
  colnames(queries) <- c("Identifier", "DAG", "Event", "Instrument", "Field", "Repetition", "Description", "Query", "Code", "Link")

  # Rename the first column to "record_id" if necessary
  if (!"record_id" %in% names(data)) {
    names(data)[1] <- "record_id"
  }

  # Error: Stop if more than one filter is provided
  if (!is.na(filter) & length(filter) > 1) {
    stop("More than one filter applied, please select only one.", call. = FALSE)
  }

  # Save the original dataset for reference
  data0 <- data

  # Apply the filter expression to the dataset if provided
  if (!is.na(filter) & length(filter) == 1) {
    command <- paste0("data", "<-dplyr::filter(data,", filter, ")")

    # Test the validity of the filter logic
    evaluation <- try(eval(parse(text = command)), silent = TRUE)

    if (inherits(evaluation, "try-error")) {
      stop("Invalid `filter` argument logic. Please review and correct the expression.", call. = FALSE)
    } else {
      eval(parse(text = command))
    }

    # Warn if the filter results in no observations
    if (nrow(data) == 0) {
      warning("The filter applied does not match any records. Please review the `filter` argument.", call. = FALSE)
    }
  }

  # Validate that the specified events are present in the dataset
  if (all(!is.na(event))) {
    if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data0))) {
      if (any(!event %in% data0$redcap_event_name) & any(!event %in% data0$redcap_event_name.factor)) {
        stop("One or more specified events do not exist in the dataset. Please review the 'event' argument.", call. = FALSE)
      }
    }

    # Iterate over each specified event to identify missing records
    for (k in seq_along(event)) {
      # Find record IDs associated with the current event (factor form)
      if (any(names(data0) == "redcap_event_name.factor") & all(event %in% data0$redcap_event_name.factor)) {
        ids <- data0$record_id[data0$redcap_event_name.factor %in% event[k]]
      }

      # Find record IDs associated with the current event (raw form)
      if (all(event %in% data0$redcap_event_name)) {
        ids <- data0$record_id[data0$redcap_event_name %in% event[k]]
      }


      # Identify records missing the current event
      raw <- data |>
        dplyr::filter(!data$record_id %in% ids) |>
        dplyr::slice(rep(1:dplyr::n(), each = length(event[k])))

      
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
          Query = if (!is.na(query_name)) {
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

  # Merge with an existing query data frame if specified in 'addTo'
  if (!is.na(addTo)) {
    # Save the column names of the existing queries
    col_names <- names(queries)

    # Merge the existing queries with the new ones
    queries <- merge(queries,
      addTo$queries,
      by = intersect(names(addTo$queries), names(queries)),
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
      dplyr::mutate(cod = 1:dplyr::n()))
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
    if (all(addTo %in% NA)) {
      report$var <- factor(report$var, levels = c(unique(event)))
      report$descr <- factor(report$descr)
    }

    # Include all variables in the report, with or without zero queries, based on 'report_zeros'
    if (report_zeros == TRUE) {
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
    message("No queries identified.")

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
      stop("Multiple titles provided for the report. Please specify only one..", call. = FALSE)
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
  result <- kableExtra::kable_styling(result,
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE
  )
  result <- kableExtra::row_spec(result, 0, italic = FALSE, extra_css = "border-bottom: 1px solid grey")

  # Return the queries and the formatted report
  list(
    queries = queries,
    results = result
  )
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
