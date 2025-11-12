#' Transform Dates and Datetimes in REDCap Data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function processes and transforms date and datetime fields in a REDCap dataset.
#' It ensures proper handling of data, dictionary (metadata), and event-form mapping,
#' and applies labels to the dataset for better usability.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping, typically the output of the `redcap_data` function. If provided, it overrides individual `data`, `dic`, and `event_form` arguments.
#' @param data A `data.frame` or `tibble` representing the REDCap dataset containing the checkbox variables.
#' @param dic A `data.frame` representing the REDCap dictionary with metadata, including field names, field types, and branching logic.
#' @param event_form A `data.frame` or `list` mapping event names to forms for longitudinal projects. Optional; defaults to `NULL` if not applicable.
#'
#' @return A list containing the following elements:
#'   \item{data}{The transformed dataset with date and datetime variables correctly formatted.}
#'   \item{dictionary}{The original data dictionary passed to the function.}
#'   \item{event_form}{The original event-form mapping passed to the function (if applicable).}
#'
#' @details
#' The function performs the following tasks:
#' - Extracts date and datetime fields from the data dictionary using validation types
#'   (`date_*` and `datetime_*`).
#' - Converts these fields in the dataset to `Date` and `POSIXct` objects, respectively.
#'
#'
#' @examples
#'
#' # Example usage:
#' result <- rd_dates(data = covican$data, dic = covican$dictionary)
#'
#' result <- covican |> rd_dates()
#'
#' @export
#' @importFrom stats na.omit

rd_dates <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL) {
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

  # Extract labels from the data to reapply later
  labels <- labels <- purrr::map_chr(data, function(x) {
    lab <- attr(x, "label")
    if (!is.null(lab)) {
      lab
    } else {
      ""
    }
  })

  # Identify date variables in the dictionary (those with `date_` in validation type)
  var_date <- dic |>
    dplyr::filter(grepl("^date_", .data$text_validation_type_or_show_slider_number)) |>
    dplyr::pull(.data$field_name)

  var_date_valid <- data |>
    dplyr::select(dplyr::all_of(var_date)) |>
    purrr::keep(~ inherits(.x, "Date")) |>
    names()

  # Identify datetime variables in the dictionary (those with `datetime_` in validation type)
  var_datetime <- dic |>
    dplyr::filter(grepl("^datetime_", .data$text_validation_type_or_show_slider_number)) |>
    dplyr::pull(.data$field_name)

  var_datetime_valid <- data |>
    dplyr::select(dplyr::all_of(var_datetime)) |>
    purrr::keep(~ inherits(.x, "POSIXct")) |>
    names()

  # Validation for Date/Datetime Formatting
  if (identical(var_date, var_date_valid) & identical(var_datetime, var_datetime_valid)) {
    warning("All date and datetime variables are already in the correct format. No transformation applied.", call. = FALSE)
  } else {
    var_date <- setdiff(var_date, var_date_valid)

    var_datetime <- setdiff(var_datetime, var_datetime_valid)

    # Convert date variables in the data to `Date` class
    data <- data |>
      dplyr::mutate_at(var_date, as.Date) |>
      # Convert datetime variables to `POSIXct` class, handling empty strings as NA
      dplyr::mutate_at(var_datetime, function(x) {
        x <- dplyr::case_when(x == "" ~ NA, TRUE ~ x)
        as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
      })
  }

  # Reapply variable labels to the data after transformation
  data <- data |>
    labelled::set_variable_labels(.labels = labels |> as.list(), .strict = FALSE)

  # Update results with the this transformation
  if (is.null(results)) {
    results <- c(results, stringr::str_glue("1. Transforming date and datetime fields. (rd_dates)\n"))
  } else {
    last_val_res <- results |>
      stringr::str_extract("^(\n)?\\d+\\.") |>
      na.omit() |>
      dplyr::last() |>
      stringr::str_remove("\\.") |>
      as.numeric()

    results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. Transforming date and datetime fields. (rd_dates)\n"))
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
