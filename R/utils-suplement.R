############### Other functions###############

## Fill_data ----

#' Fill Rows with Values from One Event
#' @description
#' This function fills all rows in the dataset with the value of a particular variable in a specified event. It is an auxiliary function used in the `rd_rlogic` function.
#' @param which_event String specifying the name of the event.
#' @param which_var String specifying the name of the variable.
#' @param data Dataset containing the REDCap data.
#' @noRd
fill_data <- function(which_event, which_var, data) {

  if (!which_event %in% data$redcap_event_name) {
    stop("The logic can't be evaluated after the translation", call. = FALSE)
  }

  fill_values <- data |>
    dplyr::select("record_id", "redcap_event_name", dplyr::all_of(which_var)) |>
    dplyr::rename(var = dplyr::all_of(which_var)) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::mutate(var = dplyr::case_when(.data$redcap_event_name != which_event ~ NA, TRUE ~ .data$var)) |>
    tidyr::fill("var", .direction = "downup") |>
    dplyr::pull("var")

  data[[which_var]] <- fill_values

  data
}


## Check_proj ----

#' Handle Project Arguments
#'
#' This helper function processes the `project` argument in the several other functions.
#' It extracts data, dictionary, event form, and results from the project object while handling
#' potential duplication and providing warnings when arguments are provided redundantly.
#'
#' @param project A list or object containing `data`, `dictionary`, and optionally `event_form` and `results`.
#' @param data A data frame (optional) that may be overridden if provided in the `project` object.
#' @param dic A data dictionary (optional) that may be overridden if provided in the `project` object.
#' @param event_form An optional event-form object that may be overridden if provided in the `project`.
#' @noRd
check_proj <- function(project, data = NULL, dic = NULL, event_form = NULL) {
  # Ensure 'project' is a list
  if (!is.list(project)) {
    stop("The 'project' argument must be a list.", call. = FALSE)
  }
  
  # Error: a data frame instead of the list of redcap_data()
  if (is.data.frame(project)) {
    stop("The 'project' argument must be the list returned by redcap_data() (with `data` and `dictionary`), not a data frame. To use a data frame, pass it with `data` and the dictionary with `dic`.", call. = FALSE)
  }

  # Check for `data` duplication
  if (!is.null(data)) {
    warning("Data has been provided twice. The function will ignore the `data` argument.")
  }

  # Check for `dic` duplication
  if (!is.null(dic)) {
    warning("Dictionary has been provided twice. The function will ignore the `dic` argument.")
  }

  # Extract data and dictionary from the project
  data <- if (!is.null(project$data)) project$data else if (!is.null(project$dat)) project$dat else NULL
  dic <- if (!is.null(project$dictionary)) project$dictionary else if (!is.null(project$dic)) project$dic else NULL

  # Handle `event_form` duplication
  if ("event_form" %in% names(project)) {
    if (!is.null(event_form)) {
      warning("Event-form has been provided twice. The function will ignore the `event_form` argument.")
    }
    event_form <- project$event_form
  }

  # Extract results from the project if present
  if ("results" %in% names(project)) {
    results <- project$results
  } else {
    results <- NULL
  }

  # Return updated arguments as a list
    list(
      data = data, dic = dic, event_form = event_form, results = results
    )
}


#' Round Numbers to a Specified Number of Digits ----
#'
#' This function rounds numeric values to the specified number of decimal digits,
#' mimicking the behavior of the base R `round()` function but implemented manually.
#'
#' @param x A numeric vector to be rounded.
#' @param digits Integer indicating the number of decimal places to round to.
#'
#' @return A numeric vector rounded to the specified number of digits.
#' @examples
#' round(3.14159, 2)
#' round(c(-2.718, 3.14159), 1)
#' @noRd
round <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}




## Sort_by_identifier ----

#' Sort a query report by record identifier
#'
#' Uses natural ordering, so `"100-2"` comes before `"100-10"` and `"ABC2"`
#' before `"ABC10"`. `Identifier` is converted to numeric only when every
#' identifier is a plain number.
#'
#' @param queries Data frame with an `Identifier` column.
#' @param by_code Logical. If `TRUE`, ties are broken by the `Code` column.
#' @return The sorted data frame.
#' @noRd
sort_by_identifier <- function(queries, by_code = FALSE) {
  id <- as.character(queries$Identifier)
  keys <- list(natural_rank(id))

  if (by_code && "Code" %in% names(queries)) {
    keys <- c(keys, list(natural_rank(queries$Code)))
  }

  queries <- queries[do.call(order, keys), , drop = FALSE]
  rownames(queries) <- NULL

  if (all(grepl("^-?[0-9]+(\\.[0-9]+)?$", id[!is.na(id)]))) {
    queries$Identifier <- as.numeric(queries$Identifier)
  }

  queries
}

natural_rank <- function(x) {
  x <- as.character(x)
  sorted <- unique(x[stringi::stri_order(x, opts_collator = stringi::stri_opts_collator(numeric = TRUE))])
  match(x, sorted)
}



#' Columns with the Missing Data Codes of a field
#'
#' `redcap_data(rm_mdc = FALSE)` keeps the codes of the validated text fields in
#' a `<field>_mdc` column next to the field. Like the `.factor` versions, they are
#' not in the dictionary.
#'
#' @param data_names Column names of the dataset.
#' @param dic_fields Field names of the dictionary (a REDCap field called
#'   `x_mdc` is never taken as one of these columns).
#' @return The names of the `_mdc` columns.
#' @noRd
mdc_columns <- function(data_names, dic_fields = NULL) {
  cols <- grep("_mdc$", data_names, value = TRUE)
  cols[sub("_mdc$", "", cols) %in% data_names & !cols %in% dic_fields]
}



## Style_report ----

#' Style an HTML report table
#'
#' Applies the style of the reports of the package with kableExtra when it is installed. Otherwise the table is returned as
#' it is, so that the functions don't need kableExtra.
#'
#' @param x An HTML table created with `knitr::kable()`.
#' @return The styled table.
#' @noRd
style_report <- function(x) {
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    return(x)
  }

  x <- kableExtra::kable_styling(x, bootstrap_options = c("striped", "condensed"), full_width = FALSE)
  kableExtra::row_spec(x, 0, italic = FALSE, extra_css = "border-bottom: 1px solid grey")
}
