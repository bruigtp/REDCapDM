#' Delete Variables from REDCap Data and Dictionary
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function removes variables from a REDCap dataset and its associated dictionary based on
#' specific variable names or patterns. It ensures consistency between the data and dictionary
#' while preserving labels.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping,
#' typically the output of the `redcap_data` function. If provided,
#' it overrides individual `data`, `dic`, and `event_form` arguments.
#' @param data A `data.frame` or `tibble` representing the REDCap dataset.
#' @param dic A `data.frame` representing the REDCap dictionary with metadata,
#' including field names, field types, and branching logic.
#' @param event_form A `data.frame` or `list` mapping event names to forms for longitudinal projects.
#' Optional; defaults to `NULL` if not applicable.
#' @param vars A character vector specifying variable names to delete from the dataset and dictionary.
#'   These variables will be removed from both the `data` and `dic`.
#' @param pattern A character vector of regular expression patterns. Variables matching these patterns
#'   will be removed from the `data` and `dic`.
#'
#' @return A list containing the following elements:
#'   \item{data}{The updated dataset with specified variables removed.}
#'   \item{dictionary}{The updated data dictionary with corresponding variables removed.}
#'   \item{event_form}{The original event-form mapping passed to the function (if applicable).}
#'
#' @details
#' The function performs the following operations:
#' - Removes variables specified in the `vars` argument from both the dataset and dictionary.
#' - Removes variables matching patterns provided in the `pattern` argument.
#'
#' @examples
#' # Example usage:
#'
#' # Deleting specific variables
#' result <- rd_delete_vars(covican,
#'   vars = c("potassium", "leuk_lymph")
#' )
#'
#' # Deleting variables based on patterns
#' result <- rd_delete_vars(
#'   data = covican$data,
#'   dic = covican$dictionary,
#'   pattern = c("_complete$", "_other$")
#' )
#'
#' @export
#' @importFrom stats na.omit

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
      pattern_factor <- paste0(pattern_factor, ".factor")

      # Warn if factor versions of the variables matching the patterns are present in the dataset
      if (any(pattern_factor %in% names(data) & grepl("\\$", pattern))) {
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
    results <- c(results, stringr::str_glue("1. Removing selected variables (rd_delete_vars)\n"))
  } else {
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
