#' Insert Missing Values Using a Filter
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function allows you to manually insert a missing value into certain variables (`vars`) if the specified filter/s (`filter`) are satisfied.
#' It's particularly useful for managing checkboxes without explicit gatekeeper questions in their branching logic.
#' Note that the variable is only transformed in the events where both the variable and the filter evaluation are present, so they must have at least one event in common.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping, typically the output of the `redcap_data` function. If provided, it overrides individual `data`, `dic`, and `event_form` arguments.
#' @param data A `data.frame` or `tibble` representing the REDCap dataset containing the checkbox variables.
#' @param dic A `data.frame` representing the REDCap dictionary with metadata, including field names, field types, and branching logic.
#' @param event_form A `data.frame` or `list` mapping event names to forms for longitudinal projects. Optional; defaults to `NULL` if not applicable.
#' @param vars A character vector with the names of the variables to be transformed.
#' @param filter A character vector of logical expressions to evaluate. If the evaluation is `TRUE`, the corresponding variable in `vars` is set to `NA`.
#'
#' @return The modified data frame with the specified variables updated.
#'
#' @examples
#'
#' # Example usage:
#' table(is.na(covican$data$potassium))
#'
#' data <- covican |>
#'   rd_insert_na(
#'     vars = "potassium",
#'     filter = "age < 65"
#'   )
#'
#' table(data$potassium)
#'
#' @export
#' @importFrom rlang .data

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

  # Validate matching lengths of `vars` and `filter`
  if (length(filter) != length(vars)) {
    stop("The number of variables (`vars`) does not match the number of filters (`filter`). Ensure both have equal length.")
  } else {
    # Loop through variables and filters to apply transformations
    for (i in seq_along(filter)) {
      # For every filter & variable get the variables specified in the filter and their events (if there is more than one event)
      if (longitudinal) {
        # Parse variables within the filter expression
        vars_filter <- trimws(unlist(stringr::str_split(filter[i], "[&|]")))
        vars_filter <- gsub("!?is.na\\(", "", vars_filter)
        vars_filter <- unlist(stringr::str_extract_all(vars_filter, "^\\w+"))

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

        # Stop if there are no common events
        if (length(events) == 0) {
          stop("The variables in the filter belong to different events.")
        }

        # Identify events for the variable to be transformed
        form_var <- dic |>
          dplyr::filter(.data$field_name == vars[i]) |>
          dplyr::pull(.data$form_name)

        event_var <- event_form |>
          dplyr::filter(.data$form == form_var) |>
          dplyr::pull(.data$unique_event_name)

        # Ensure the variable's events overlap with filter events
        match_events <- intersect(events, event_var)

        # Error: filter variables are in different events from the variable to be transformed
        if (length(match_events) == 0) {
          stop("The variable `{vars[i]}` and the filter do not overlap in any events.")
        } else {
          # Warn: one of the events of the variable is not present in the filter
          if (!all(event_var %in% match_events)) {
            warning(stringr::str_glue(
              "The variable `{vars[i]}` is present in more events than the filter. ",
              "Only rows in common events ({paste(match_events, collapse = ', ')}) will be transformed."
            ))
          }
        }
      }

      # Apply transformation: set specified variable to NA if filter is true
      id <- data |>
        dplyr::mutate(id = dplyr::row_number()) |>
        dplyr::filter(eval(parse(text = filter[i]))) |>
        dplyr::pull(id)

      data[id, vars[i]] <- NA
    }

    # Reapply variable labels to the data after transformation
    data <- data |>
      labelled::set_variable_labels(.labels = labels |> as.list(), .strict = FALSE)

    # Update results with the this transformation
    if (is.null(results)) {
      results <- c(results, stringr::str_glue("1. Inserting missing values into certain variables. (rd_insert_na)\n"))
    } else {
      last_val_res <- results |>
        stringr::str_extract("^(\n)?\\d+\\.") |>
        na.omit() |>
        dplyr::last() |>
        stringr::str_remove("\\.") |>
        as.numeric()

      results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. Inserting missing values into certain variables. (rd_insert_na)\n"))
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
