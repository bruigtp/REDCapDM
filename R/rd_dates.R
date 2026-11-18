#' Transform Dates and Datetimes in REDCap Data
#'
#' @description
#' `r lifecycle::badge('stable')`
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
#' * Each value is converted on its own (`YYYY-MM-DD` or `YYYY/MM/DD`, with or without time): a value that isn't a date or a
#'   datetime (e.g. a Missing Data Code that wasn't removed) is set to `NA`, and a warning lists the fields and the values.
#'
#'
#' @examples
#' result <- rd_dates(covican)
#' transformed_data <- result$data
#'
#' @family process functions
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
  if (is.null(data) || is.null(dic)) {
    rd_abort("Both {.arg data} and {.arg dic} (data and dictionary) arguments must be provided.")
  }
  
  # Error: invalid arguments
  check_inputs(data, dic, dic_cols = c("field_name", "text_validation_type_or_show_slider_number"))
  data <- plain_data(data)
  dic <- plain_dic(dic)

  # Extract labels from the data to reapply later
  labels <- labels <- purrr::map_chr(data, function(x) {
    lab <- attr(x, "label")
    if (!is.null(lab)) {
      lab
    } else {
      ""
    }
  })

  # Identify date variables in the dictionary (those with `date_` in validation type) that are in the data
  var_date <- dic |>
    dplyr::filter(grepl("^date_", .data$text_validation_type_or_show_slider_number)) |>
    dplyr::pull(.data$field_name) |>
    intersect(names(data))

  var_date_valid <- data |>
    dplyr::select(dplyr::all_of(var_date)) |>
    purrr::keep(~ inherits(.x, "Date")) |>
    names()

  # Identify datetime variables in the dictionary (those with `datetime_` in validation type)
  var_datetime <- dic |>
    dplyr::filter(grepl("^datetime_", .data$text_validation_type_or_show_slider_number)) |>
    dplyr::pull(.data$field_name) |>
    intersect(names(data))

  var_datetime_valid <- data |>
    dplyr::select(dplyr::all_of(var_datetime)) |>
    purrr::keep(~ inherits(.x, "POSIXct")) |>
    names()

  # Validation for Date/Datetime Formatting
  if (identical(var_date, var_date_valid) && identical(var_datetime, var_datetime_valid)) {
    rd_inform("All date and datetime variables are already in the correct format. No transformation applied.")
  } else {
    var_date <- setdiff(var_date, var_date_valid)

    var_datetime <- setdiff(var_datetime, var_datetime_valid)

    # Convert date variables to `Date` class and datetime variables to `POSIXct` class (empty strings are NA)
    # (value by value: as.Date() and as.POSIXct() choose one format for the whole column, so a value that isn't a date stops
    # them or turns other dates into NA)
    not_dates <- list()
    for (var in var_date) {
      converted <- convert_dates(data[[var]], c("%Y-%m-%d", "%Y/%m/%d"), datetime = FALSE)
      data[[var]] <- converted$value
      not_dates[[var]] <- converted$not_dates
    }
    for (var in var_datetime) {
      converted <- convert_dates(
        data[[var]],
        c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d"),
        datetime = TRUE
      )
      data[[var]] <- converted$value
      not_dates[[var]] <- converted$not_dates
    }

    # Warning: values that aren't dates
    not_dates <- not_dates[lengths(not_dates) > 0]
    if (length(not_dates) > 0) {
      details <- vapply(names(not_dates), function(var) {
        values <- unique(not_dates[[var]])
        shown <- paste0("\"", gsub("([{}])", "\\1\\1", utils::head(values, 3)), "\"", collapse = ", ")
        sprintf("{.var %s}: %d value%s (%s%s)", var, length(not_dates[[var]]), if (length(not_dates[[var]]) > 1) "s" else "", shown, if (length(values) > 3) ", ..." else "")
      }, character(1))
      rd_warn(c(
        "Some values of the date and datetime fields aren't dates. They are set to NA:",
        stats::setNames(details, rep("*", length(details))),
        "i" = "If they are Missing Data Codes of the project, give them in the {.arg mdc} argument of {.fn redcap_data}."
      ))
    }
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


#' Convert the values of a date or datetime field, one by one
#'
#' @param x Values of the field.
#' @param formats Formats tried in order for each value.
#' @param datetime Whether the field is a datetime (`POSIXct` in UTC) or a date (`Date`).
#' @return A list with the converted values (`value`) and the values that aren't dates (`not_dates`).
#' @noRd
convert_dates <- function(x, formats, datetime) {
  # Numbers are days (dates) or seconds (datetimes) since 1970-01-01, as before
  if (is.numeric(x)) {
    value <- if (datetime) as.POSIXct(x, origin = "1970-01-01", tz = "UTC") else as.Date(x, origin = "1970-01-01")
    return(list(value = value, not_dates = character(0)))
  }

  text <- trimws(as.character(x))
  text[text %in% ""] <- NA
  value <- if (datetime) as.POSIXct(rep(NA_real_, length(text)), origin = "1970-01-01", tz = "UTC") else as.Date(rep(NA_real_, length(text)), origin = "1970-01-01")
  for (format in formats) {
    pending <- is.na(value) & !is.na(text)
    if (!any(pending)) {
      break
    }
    value[pending] <- if (datetime) as.POSIXct(text[pending], format = format, tz = "UTC") else as.Date(text[pending], format = format)
  }

  list(value = value, not_dates = text[is.na(value) & !is.na(text)])
}
