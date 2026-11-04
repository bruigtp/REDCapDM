#' Transform Dates and Datetimes in REDCap Data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Converts date and datetime fields in a REDCap dataset to appropriate R classes.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{data}{The transformed dataset with date and datetime fields formatted as `Date` and `POSIXct`.}
#'   \item{dictionary}{The original REDCap dictionary passed to the function.}
#'   \item{event_form}{The original event-form mapping (if applicable).}
#'   \item{results}{A summary of the transformations performed.}
#' }
#'
#' @details
#' The function performs the following tasks:
#' * Detects date and datetime fields from the REDCap dictionary (`date_*` and `datetime_*` validation types).
#' * Converts date fields to `Date` class.
#' * Converts datetime fields to `POSIXct` class, treating empty strings as `NA`.
#'
#'
#' @examples
#' result <- rd_dates(covican)
#' transformed_data <- result$data
#'
#' @export
#' @importFrom stats na.omit

rd_dates <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL) {
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
    results <- c(results, stringr::str_glue("Transforming date and datetime fields. (rd_dates)\n"))
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
