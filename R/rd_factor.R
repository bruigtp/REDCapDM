#' Convert Variables to Factors in a REDCap Dataset
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Converts variables in a REDCap dataset with associated `.factor` columns into actual factor variables, while allowing the exclusion of specific variables. Ensures consistency with the dataset and preserves variable labels.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param exclude Optional character vector of variable names (use original names **without** the `.factor` suffix) to exclude from conversion.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{data}{The transformed dataset with `.factor` columns applied as factors.}
#'   \item{dictionary}{The dictionary used (unchanged).}
#'   \item{event_form}{The event-form mapping used (if applicable).}
#'   \item{results}{A brief text summary of the transformation.}
#' }
#'
#' @details
#' The function looks for columns ending in `.factor` and replaces the original variable values with those `.factor` values (converted to factors). It preserves variable labels. The `exclude` argument must contain base variable names (no `.factor` suffix); if any `.factor` names are passed to `exclude` the function will throw an informative error. The columns `redcap_event_name`, `redcap_repeat_instrument` and `redcap_data_access_group` (and their `.factor` counterparts) are handled specially to avoid altering event or access-group data.
#'
#' @examples
#' \dontrun{
#' result <- rd_factor(covican)
#' result <- rd_factor(covican, exclude = c("available_analytics", "urine_culture"))
#' transformed_data <- result$data
#' }
#'
#' @export
#' @importFrom stats na.omit

rd_factor <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, exclude = NULL) {

  results <- NULL

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    env_vars <- check_proj(project, data, dic, event_form)

    list2env(env_vars, envir = environment())
  }

  # Ensure both `data` and `dic` are provided; stop if either is missing
  if (is.null(data) | is.null(dic)) {
    stop("Both `data` and `dic` (data and dictionary) arguments must be provided.")
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

  # We need to preserve the original values of `redcap_event_name` and `redcap_data_access_group`, so exclude them from conversion
  keep <- c("redcap_event_name.factor", "redcap_repeat_instrument.factor", "redcap_data_access_group.factor")
  keep_factors <- data |>
    dplyr::select(dplyr::any_of(keep))

  # Remove the factor versions of the variables to prevent them from being affected
  data <- data |>
    dplyr::select(-dplyr::any_of(keep))

  # Identify factor columns and remove orphans
  factor_cols <- grep("\\.factor$", names(data), value = TRUE)
  factors <- sub("\\.factor$", "", factor_cols)
  factors <- setdiff(factors, sub("\\.factor$", "", keep))

  # Detect orphan .factor columns (base column missing)
  orphan <- factor_cols[!factors %in% names(data)]

  if (length(orphan) > 0) {
    warning(
      stringr::str_glue(
        "Removed {length(orphan)} '.factor' column(s) whose original variables no longer exist. This usually happens if the original variables were deleted earlier using `rd_delete_vars` and the '.factor' version was kept."
      ),
      call. = FALSE
    )
    # Remove orphan columns
    data <- data[, setdiff(names(data), orphan)]
    # Update factors vector after removal
    factors <- setdiff(factors, sub("\\.factor$", "", orphan))
  }

  # If there are no factor variables, stop the function
  if (length(factors) == 0) {
    warning("There are no variables in the data which can be converted to factors.", call. = FALSE)
  } else {
    if (!is.null(exclude)) {
      bad_vars <- exclude[grepl("\\.factor$", exclude)]
      if (length(bad_vars) > 0) {
        stop(
          sprintf(
            "Please use the original form of the variable(s) without '.factor' in the exclude argument: %s",
            paste(bad_vars, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
    # Exclude specified variables that should not be converted to factors
    factors <- setdiff(factors, exclude)

    # If no variables are left to convert, stop and ask to review the exclude argument
    if (length(factors) == 0) {
      stop("All variables in the data which can be converted to factors are specified in the `exclude` argument. Please, review the `exclude` argument.", call. = FALSE)
    }

    # Perform the transformation of factor columns into actual factor variables
    data <- data |>
      # Assign the values from the factor columns to the original columns and remove the '.factor' versions
      dplyr::mutate(dplyr::across(tidyselect::all_of(factors), ~ get(
        stringr::str_glue("{dplyr::cur_column()}.factor")
      ))) |>
      dplyr::select(-stringr::str_glue("{factors}.factor"))
  }

  # If there were any variables that were excluded from conversion, reattach them to the data
  if (length(keep_factors) > 0) {
    data <- data |>
      dplyr::bind_cols(keep_factors)

    # Relocate the kept factor variables to the correct position in the data
    for (i in seq_along(keep_factors)) {
      data <- data |>
        dplyr::relocate(names(keep_factors)[i], .after = sub("\\.factor$", "", names(keep_factors)[i]))
    }

    # Alternativa
    # data <- purrr::reduce2(
    #   .x = c(names(keep_factors)),
    #   .y = c(sub("\\.factor$", "", names(keep_factors))),
    #   .f = ~ dplyr::relocate(..1, ..2, .after = ..3),
    #   .init = data
    # )
  }

  # Apply the labels to the data
  data <- data |>
    labelled::set_variable_labels(.labels = labels |> as.list(), .strict = FALSE)

  # Update results with the this transformation
  if (is.null(results)) {
    results <- c(results, stringr::str_glue("Replacing original variables for their factor version. (rd_factor)\n"))
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

    results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. Replacing original variables for their factor version. (rd_factor)\n"))
  }

  # Return the results: the transformed data, event_form, and results
  list(
    data = data,
    dictionary = dic,
    event_form = event_form,
    results = stringr::str_glue("{results}")
  ) |>
    purrr::compact() # Remove any NULL elements from the output list
}
