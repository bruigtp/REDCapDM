#' Insert Missing Values Using a Filter
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Sets selected variables to `NA` when a filter condition is satisfied. Useful for managing checkboxes or other fields without explicit gatekeeper questions.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param vars Character vector of variable names to set to `NA`.
#' @param filter A single logical expression (as string). Rows where the filter evaluates to `TRUE` will have the corresponding `vars` set to `NA`.
#'
#' @details
#' * Each variable is only updated in rows/events where both the variable and filter are present.
#' * For longitudinal projects, `event_form` must be provided for proper event-level filtering.
#' * Only one filter expression is allowed.
#' * Variables and filter columns must exist in both `data` and `dictionary`.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{The dataset with `NA` inserted where the filter applies.}
#'   \item{dictionary}{The unchanged dictionary.}
#'   \item{event_form}{The `event_form` passed in (if applicable).}
#'   \item{results}{Summary message of the changes applied.}
#' }
#'
#' @examples
#' # Set 'potassium' to NA where age < 65
#' \dontrun{
#' data <- rd_insert_na(
#'   data = covican$data,
#'   dic = covican$dictionary,
#'   vars = "potassium",
#'   filter = "age < 65"
#' )
#' table(data$potassium)
#' }
#'
#' @export
#' @importFrom rlang .data parse_expr eval_tidy

rd_insert_na <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, vars, filter) {

  results <- NULL

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

  # Determine if the dataset is longitudinal
  longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

  # Error: For longitudinal data, ensure `event_form` is specified
  if (is.null(event_form) & longitudinal) {
    stop("The dataset contains multiple events, but the `event_form` mapping was not provided. Please specify it.")
  }

  # Validate there is exactly one filter and allow multiple vars
  if (length(filter) != 1) {
    stop("Please provide exactly one filter.")
  } else {
    # Parse variables within the single filter expression once
    vars_filter <- trimws(unlist(stringr::str_split(filter[1], "[&|]")))
    vars_filter <- gsub("!?is.na\\(", "", vars_filter)
    vars_filter <- gsub("\\[|\\]", "", vars_filter)
    vars_filter <- gsub("data\\$", "", vars_filter)
    vars_filter <- unlist(stringr::str_extract_all(vars_filter, "^\\w+"))
    vars_filter <- unique(vars_filter)

    # check filter vars exist in the dataset
    missing_in_data <- vars_filter[!vars_filter %in% names(data)]
    if (length(missing_in_data) > 0) {
      stop(
        sprintf(
          "Filter variable(s) not found in data: %s",
          paste(shQuote(missing_in_data), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # check filter vars exist in the dictionary
    missing_in_dic <- vars_filter[!vars_filter %in% dic$field_name]
    if (length(missing_in_dic) > 0) {
      stop(
        sprintf(
          "Filter variable(s) not found in dictionary: %s",
          paste(shQuote(missing_in_dic), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # Extract corresponding events for filter variables
    event_filter <- tibble::tibble(vars_filter = vars_filter) |>
      dplyr::mutate(
        form = purrr::map_chr(.data$vars_filter, ~ dic |>
                                dplyr::filter(.data$field_name %in% .x) |>
                                dplyr::pull(.data$form_name)),
        event = purrr::map(.data$form, ~ event_form |>
                             dplyr::filter(.data$form %in% .x) |>
                             dplyr::pull(.data$unique_event_name))
      )

    # Identify common events for filter variables
    events <- Reduce(intersect, event_filter$event)

    # Stop if there are no common events among filter variables
    if (length(events) == 0) {
      stop("The variables in the filter belong to different events.")
    }

    # Evaluate the filter expression once to get logical mask for rows
    filter_expr <- tryCatch({
      rlang::parse_expr(filter[1])
    }, error = function(e) {
      stop(sprintf("Unable to parse filter expression '%s'.", filter[1]), call. = FALSE)
    })

    rows_mask <- tryCatch({
      # Evaluate in the context of `data`; result should be logical vector of length nrow(data)
      rlang::eval_tidy(filter_expr, data = data)
    }, error = function(e) {
      stop(sprintf("Error evaluating filter '%s'.", filter[1]), call. = FALSE)
    })

    if (!is.logical(rows_mask) || length(rows_mask) != nrow(data)) {
      stop(sprintf("Filter '%s' did not return a logical vector with length equal to nrow(data).", filter[1]))
    }

    # Validate that provided vars exist in data and in dictionary
    missing_vars_in_data <- vars[!vars %in% names(data)]
    if (length(missing_vars_in_data) > 0) {
      stop(sprintf("Variable(s) not found in data: %s", paste(shQuote(missing_vars_in_data), collapse = ", ")), call. = FALSE)
    }
    missing_vars_in_dic <- vars[!vars %in% dic$field_name]
    if (length(missing_vars_in_dic) > 0) {
      stop(sprintf("Variable(s) not found in dictionary: %s", paste(shQuote(missing_vars_in_dic), collapse = ", ")), call. = FALSE)
    }

    # Try to detect an event column in `data` (common names or values matching event_form)
    event_col <- NULL
    candidate_names <- c("redcap_event_name", "unique_event_name", "event_name", "event")
    event_col <- intersect(names(data), candidate_names)[1]
    if (is.null(event_col)) {
      for (col in names(data)) {
        col_values <- unique(data[[col]])
        if (any(col_values %in% event_form$unique_event_name, na.rm = TRUE)) {
          event_col <- col
          break
        }
      }
    }

    # Loop through the variables to transform
    for (j in seq_along(vars)) {
      var_j <- vars[j]

      if (longitudinal) {
        # Identify events for the variable to be transformed
        form_var <- dic |>
          dplyr::filter(.data$field_name == var_j) |>
          dplyr::pull(.data$form_name)

        event_var <- event_form |>
          dplyr::filter(.data$form == form_var) |>
          dplyr::pull(.data$unique_event_name)

        # Ensure the variable's events overlap with filter events
        match_events <- intersect(events, event_var)

        # Error: no overlapping events between the variable and the filter
        if (length(match_events) == 0) {
          stop(stringr::str_glue("The variable `{var_j}` and the filter do not overlap in any events."), call. = FALSE)
        } else {
          # Warn: variable present in more events than the filter
          if (!all(event_var %in% match_events)) {
            warning(stringr::str_glue(
              "The variable `{var_j}` is present in more events than the filter. ",
              "Only rows in common events ({paste(match_events, collapse = ', ')}) will be transformed."
            ))
          }
        }
      } else {
        # not longitudinal: no per-event checks required
        match_events <- NULL
      }

      # Compute candidate row ids where the filter is TRUE
      ids <- which(rows_mask)

      # If longitudinal and an event column is found, restrict ids to the overlapping events for this var
      if (longitudinal && !is.null(event_col)) {
        ids <- ids[which(data[[event_col]][ids] %in% match_events)]
      } else if (longitudinal && is.null(event_col)) {
        # No event column found in data — warn that event-level restriction cannot be applied
        warning("No event column detected in `data`. The filter will be applied across all rows (event-level restriction skipped).")
      }

      # Apply the transformation (set selected rows for var_j to NA)
      if (length(ids) > 0) {
        data[ids, var_j] <- NA
      } else {
        # No rows to change for this variable (possible due to event restriction)
        # We do not stop here; just inform via a message (could be silent depending on preference)
        message(sprintf("No rows matched for variable '%s' after applying filter and event overlap.", var_j))
      }
    }

    # Reapply variable labels to the data after transformation
    data <- data |>
      labelled::set_variable_labels(.labels = labels |> as.list(), .strict = FALSE)

    # Update results with this transformation
    inserted_msg <- stringr::str_glue("Inserting missing values into variable(s): {paste(vars, collapse = ', ')}. (rd_insert_na)\n")
    if (is.null(results)) {
      results <- c(results, inserted_msg)
    } else {
      if (grepl("^[A-Z]", results[1])) {
        results[1] <- paste("1.", results[1])
      }

      last_val_res <- results |>
        stringr::str_extract("^(\n)?\\d+\\.") |>
        na.omit() |>
        dplyr::last() |>
        stringr::str_remove("\\.") |>
        as.numeric()

      results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. {inserted_msg}\n"))
    }

    # Return the updated data, dictionary, event_form, and results (if present)
    list(
      data = data,
      dictionary = dic,
      event_form = event_form,
      results = stringr::str_glue("{results}")
    ) |>
      purrr::compact() # Remove NULL elements from the list
  }
}
