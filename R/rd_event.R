#' Identify Missing Events in REDCap Data
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function identifies records in a REDCap longitudinal project that are missing specific events.
#' REDCap does not export events with no data by default, which can create challenges in verifying completeness.
#' This function provides insights into missing events, allowing you to identify which records do not contain information about a particular event.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping, typically the output of the `redcap_data` function. If provided, it overrides individual `data`, `dic`, and `event_form` arguments.
#' @param data A `data.frame` or `tibble` representing the REDCap dataset containing the checkbox variables.
#' @param dic A `data.frame` representing the REDCap dictionary with metadata, including field names, field types, and branching logic.
#' @param event_form A `data.frame` or `list` mapping event names to forms for longitudinal projects. Optional; defaults to `NULL` if not applicable.
#' @param event A character vector specifying the name(s) of the REDCap event(s) to analyze for missing records.
#' @param filter An optional filter to apply to the dataset. This can be used to identify missing events in a subset of the data.
#' @param query_name A description of the query. Defaults to "The event (event_name) is missing" for each event if not provided.
#' @param addTo A data frame of previous query results to which new queries can be appended. If not provided, the function creates a new data frame.
#' @param report_title An optional title for the report.
#' @param report_zeros Logical; if `TRUE`, includes a report of variables without missing data.
#' @param link A list containing project information used to generate links for each missing event. Requires `domain`, `redcap_version`, and `proj_id` keys.
#'
#' @return A list with two elements:
#' \item{queries}{A data frame listing records with missing events, including metadata for each record.}
#' \item{results}{A summary table (HTML) showing the count of missing events for each analyzed event.}
#'
#'
#' @details
#' The function is designed to work with REDCap longitudinal projects, which may not include empty events in their exports.
#' By specifying the events of interest, users can quickly identify missing records for a specific event.
#' Filters can be applied to focus the analysis on specific subsets of the data.
#'
#' If project information (`link`) is provided, the output will include clickable URLs for each missing record.
#'
#' @examples
#' # Example usage with a REDCap dataset:
#' example <- covican |> rd_event(event = "follow_up_visit_da_arm_1")
#'
#' example$queries
#' example$results
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

  # Create an empty data frame to store identified queries
  queries <- as.data.frame(matrix(ncol = 10, nrow = 0))
  colnames(queries) <- c("Identifier", "DAG", "Event", "Instrument", "Field", "Repetition", "Description", "Query", "Code", "Link")

  # Rename the first column to "record_id" if necessary
  if ("record_id" %in% names(data)) {
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
          DAG = if (any(c("redcap_data_access_group", "redcap_data_access_group.factor") %in% names(x))) {
            ifelse("redcap_data_access_group.factor" %in% names(x),
              as.character(x[, "redcap_data_access_group.factor"]),
              as.character(x[, "redcap_data_access_group"])
            )
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
    # Handle cases where the Identifier contains a center and id separated by a dash
    if (all(grepl("-", queries$Identifier))) {
      # Separate the Identifier into center and id components
      queries <- queries |>
        tidyr::separate("Identifier", c("center", "id"), sep = "([-])", remove = FALSE)

      # Convert center and id to numeric for sorting
      queries[, "center"] <- as.numeric(queries[, "center"])
      queries[, "id"] <- as.numeric(queries[, "id"])

      # Sort queries by center and id
      queries <- queries[order(queries[, "center"], queries[, "id"]), ]

      # Reset row names and remove temporary columns
      rownames(queries) <- NULL
      queries <- queries |> dplyr::select(-"center", -"id")
    } else {
      # If Identifier doesn't contain a dash, sort numerically by Identifier
      queries$Identifier <- as.numeric(queries$Identifier)
      queries <- queries[order(queries$Identifier), ]
    }

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

    # Populate the report with event names and placeholders for descriptions
    report$var <- event
    report$descr <- if ("redcap_event_name.factor" %in% names(data)) {
      as.character(unique(data0$redcap_event_name.factor[which(data0$redcap_event_name %in% event)]))
    } else {
      "-"
    }

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

  # Finalize and arrange the report for output
  report <- report |>
    dplyr::mutate(descr = unique(data0$redcap_event_name.factor[data0$redcap_event_name %in% report$var])) |>
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
