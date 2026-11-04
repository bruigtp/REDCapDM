#' Delete Variables from REDCap Dataset and Dictionary
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Deletes selected variables from a REDCap dataset and its dictionary, keeping them consistent and preserving variable labels.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param vars Optional. A character vector of variable names to remove from both the dataset and dictionary.
#' @param pattern Optional. A character vector of regular expression patterns. Variables matching these patterns will be removed from the dataset and dictionary.
#'
#'
#' @details
#' - Ensure that at least one of `vars` or `pattern` is specified.
#' - Removes specified variables and their factor versions (e.g., `variable.factor`) from the dataset.
#' - Removes matching variables from the dictionary.
#' - Warns about factor versions of variables matching patterns, recommending use of `rd_factor()` if necessary.
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{The updated dataset with specified variables removed.}
#'   \item{dictionary}{The updated REDCap dictionary.}
#'   \item{event_form}{The original event-form mapping (if applicable).}
#'   \item{results}{A summary message describing the variable removal operation.}
#' }
#'
#' @examples
#' # Delete specific variables by name
#' result <- rd_delete_vars(
#'   project = covican,
#'   vars = c("potassium", "leuk_lymph")
#' )
#'
#' # Delete variables matching patterns
#' result <- rd_delete_vars(
#'   data = covican$data,
#'   dic = covican$dictionary,
#'   pattern = c("_complete$", "_other$")
#' )
#'
#' @export

rd_delete_vars <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, vars = NULL, pattern = NULL) {
  results <- NULL

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    env_vars <- check_proj(project, data, dic, event_form)
    list2env(env_vars, envir = environment())
  }

  # Ensure both `data` and `dic` are provided; stop if either is missing
  if (is.null(data) | is.null(dic)) {
    stop("Both `data` and `dic` (data and dictionary) arguments must be provided.", call. = FALSE)
  }

  # Ensure one of the arguments is fullfilled
  if (is.null(vars) & is.null(pattern)) {
    stop("At least one of the 'vars' or 'pattern' arguments must be provided.", call. = FALSE)
  }

  # Extract labels from the data to reapply later
  labels <- purrr::map_chr(data, function(x) {
    lab <- attr(x, "label")
    if (!is.null(lab)) {
      lab
    } else {
      ""
    }
  })

  # If `vars` is specified, iterate through each variable and remove it from the data and dictionary
  if (!is.null(vars)) {
    vars_missing <- setdiff(vars, names(data))

    if (length(vars_missing) > 0) {
      stop(stringr::str_glue("The following variables are not present in the dataset: {paste0(vars_missing, collapse = ', ')}.\nPlease remove them from the `vars` argument."), call. = FALSE)
    }

    for (i in seq_along(vars)) {
      # Remove the variable from the dataset
      data <- data |>
        dplyr::select(!vars[i])

      # Check if the factor version of the variable exists and remove it if present
      if (paste0(vars[i], ".factor") %in% names(data)) {
        data <- data |>
          dplyr::select(!paste0(vars[i], ".factor"))
      }

      # Remove the variable from the dictionary
      dic <- dic |>
        dplyr::filter(.data$field_name != vars[i])
    }
  }


  # If `pattern` is specified, use it to identify and remove matching variables
  if (!is.null(pattern)) {
    # Collapse patterns used
    comb_pattern <- paste(pattern, collapse = "|")

    # Create factor versions of the patterns for additional checks
    pattern_factor <- data |>
      dplyr::select(grep(comb_pattern, names(data), value = TRUE)) |>
      names()

    if (length(pattern_factor) > 0) {
      pattern_factor <- ifelse(
        endsWith(pattern_factor, ".factor"),
        pattern_factor,
        paste0(pattern_factor, ".factor")
      )

      vars_eliminated <- data |>
        dplyr::select(!dplyr::matches(comb_pattern))

      # Warn if factor versions of the variables matching the patterns are present in the dataset
      if (any(pattern_factor %in% names(vars_eliminated)) & any(grepl("\\$", pattern))) {
        warning("The dataset contains factor versions of variables matching the specified patterns. To properly remove them, use the `rd_factor` function first.", call. = FALSE)
      }
    }

    # Remove variables matching the pattern from the dataset
    data <- data |>
      dplyr::select(!dplyr::matches(comb_pattern))

    # Remove variables matching the pattern from the dictionary
    dic <- dic |>
      dplyr::filter(!grepl(comb_pattern, .data$field_name))
  }

  # Reapply variable labels to the dataset after modifications
  data <- data |>
    labelled::set_variable_labels(
      .labels = labels |> as.list(), .strict = FALSE
    )

  # Update results with the this transformation
  if (is.null(results)) {
    results <- c(results, stringr::str_glue("Removing selected variables (rd_delete_vars)\n"))
  } else {

    if(grepl("^[A-Z]", results[1])) {
      results[1] <- paste("1.", results[1])
    }

    last_val_res <- results |>
      stringr::str_extract("^(\n)?\\d+\\.") |>
      na.omit() |>
      dplyr::last() |>
      stringr::str_remove("\\.") |>
      as.numeric()

    results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. Removing selected variables (rd_delete_vars)\n"))
  }

  # Return the updated dataset, dictionary, event_form, and results
  list(
    data = data,
    dictionary = dic,
    event_form = event_form,
    results = stringr::str_glue("{results}")
  ) |>
    purrr::compact() # Remove any NULL elements from the output list
}
