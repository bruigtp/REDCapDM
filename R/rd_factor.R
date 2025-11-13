#' Convert Variables to Factors in a REDCap Dataset
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function converts variables in a REDCap dataset that have associated `.factor` columns into actual factor variables. It also allows for the exclusion of specific variables from being converted into factors.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping, typically the output of the `redcap_data` function. If provided, it overrides individual `data`, `dic`, and `event_form` arguments.
#' @param data A `data.frame` or `tibble` representing the REDCap dataset containing the checkbox variables.
#' @param dic A `data.frame` representing the REDCap dictionary with metadata, including field names, field types, and branching logic.
#' @param event_form A `data.frame` or `list` mapping event names to forms for longitudinal projects. Optional; defaults to `NULL` if not applicable.
#' @param exclude A character vector of variable names to exclude from being converted into factors.
#'
#' @return A list containing:
#' \item{data}{The transformed dataset with factor variables applied.}
#' \item{dictionary}{The dictionary used.}
#' \item{event_form}{The event-form mapping used (if provided).}
#' \item{results}{A string summarizing the changes made during the transformation.}
#'
#' @details
#' This function searches for columns in the data that have a `.factor` suffix (indicating that they can be converted into factors) and converts them into factors based on their labels.
#' The `exclude` argument allows you to specify which variables should not be converted.
#' The function also modifies the branching logic in the dictionary to reflect the changes made in the data.
#'
#' Variables with the names `redcap_event_name.factor` and `redcap_data_access_group.factor` are excluded from the conversion process to avoid altering event and access group information.
#'
#' @examples
#' result <- REDCapDM::rd_factor(covican, exclude = c("available_analytics", "urine_culture"))
#'
#' transformed_data <- result$data
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

  # Identify the columns ending with '.factor' (these are the potential factor variables)
  factors <- data |>
    dplyr::select(dplyr::matches("\\.factor$")) |>
    names() |>
    stringr::str_remove("\\.factor$")

  factors <- setdiff(factors, stringr::str_remove(keep, "\\.factor$"))

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
