## Argument checks ----
# Every exported function checks its arguments with these helpers, so that a wrong value stops with a message that names the
# argument and what it expects, instead of an internal R error.

#' Check a TRUE/FALSE argument
#'
#' @param x Value of the argument.
#' @param arg Name of the argument.
#' @noRd
check_flag <- function(x, arg) {
  if (!isTRUE(x) && !isFALSE(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", arg), call. = FALSE)
  }

  invisible(x)
}

#' Check an argument that is a single text value
#'
#' @param x Value of the argument.
#' @param arg Name of the argument.
#' @param null_ok,na_ok Whether `NULL` or `NA` (the argument isn't used) are allowed.
#' @noRd
check_string <- function(x, arg, null_ok = FALSE, na_ok = FALSE) {
  if (is.null(x) && null_ok) {
    return(invisible(x))
  }

  if (na_ok && length(x) == 1 && !is.list(x) && is.na(x)) {
    return(invisible(x))
  }

  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a single text value, e.g. \"abc\".", arg), call. = FALSE)
  }

  invisible(x)
}

#' Check an argument that is a character vector (names of variables, forms, events, patterns...)
#'
#' @param x Value of the argument.
#' @param arg Name of the argument.
#' @param null_ok Whether `NULL` (the argument isn't used) is allowed.
#' @noRd
check_strings <- function(x, arg, null_ok = TRUE) {
  if (is.null(x) && null_ok) {
    return(invisible(x))
  }

  if (!is.character(x) || length(x) == 0 || anyNA(x) || any(!nzchar(x))) {
    stop(sprintf("`%s` must be a character vector without empty values, e.g. c(\"a\", \"b\").", arg), call. = FALSE)
  }

  invisible(x)
}

#' Check an optional text argument whose default is `NA` (query names, titles...)
#'
#' @param x Value of the argument.
#' @param arg Name of the argument.
#' @noRd
check_text <- function(x, arg) {
  if (length(x) == 1 && !is.list(x) && is.na(x)) {
    return(invisible(x))
  }

  if (!is.character(x)) {
    stop(sprintf("`%s` must be text (or NA, its default).", arg), call. = FALSE)
  }

  invisible(x)
}

#' Check an argument with a fixed set of options
#'
#' @param x Value of the argument.
#' @param arg Name of the argument.
#' @param choices The options.
#' @param partial Whether the start of an option is enough (as with `match.arg()`).
#' @return The option chosen.
#' @noRd
check_choice <- function(x, arg, choices, partial = FALSE) {
  hit <- if (is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)) {
    if (x %in% choices) x else if (partial) choices[startsWith(choices, x)] else character(0)
  } else {
    character(0)
  }

  if (length(hit) != 1) {
    stop(sprintf("`%s` should be one of %s.", arg, paste0("\"", choices, "\"", collapse = ", ")), call. = FALSE)
  }

  hit
}

#' Check an argument that is a single number
#'
#' @param x Value of the argument.
#' @param arg Name of the argument.
#' @param min Minimum value.
#' @param null_ok Whether `NULL` (the argument isn't used) is allowed.
#' @noRd
check_number <- function(x, arg, min = 1, null_ok = TRUE) {
  if (is.null(x) && null_ok) {
    return(invisible(x))
  }

  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x) || x < min) {
    stop(sprintf("`%s` must be a number greater than or equal to %s.", arg, min), call. = FALSE)
  }

  invisible(x)
}

#' Check the data and the dictionary
#'
#' @param data,dic The data and the dictionary.
#' @param dic_cols Columns of the dictionary that the function needs.
#' @noRd
check_inputs <- function(data, dic, dic_cols = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame: the `data` element of the output of redcap_data().", call. = FALSE)
  }

  if (!is.data.frame(dic)) {
    stop("`dic` must be a data frame: the `dictionary` element of the output of redcap_data().", call. = FALSE)
  }

  not_found <- setdiff(dic_cols, names(dic))
  if (length(not_found) > 0) {
    stop(sprintf("The dictionary doesn't have the column(s) %s. Please use the dictionary returned by redcap_data().", paste0("`", not_found, "`", collapse = ", ")), call. = FALSE)
  }

  invisible(TRUE)
}
