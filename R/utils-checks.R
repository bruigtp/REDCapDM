## Argument checks ----
# Every exported function checks its arguments with these helpers, so that a wrong value stops with a message that names the
# argument and what it expects, instead of an internal R error. The errors have the class `REDCapDM_error_argument` and show
# the function that the user called (`call`).

#' Check a TRUE/FALSE argument
#'
#' @param x Value of the argument.
#' @param arg Name of the argument.
#' @param call The function shown in the error.
#' @noRd
check_flag <- function(x, arg, call = rlang::caller_env()) {
  if (!isTRUE(x) && !isFALSE(x)) {
    rd_abort("{.arg {arg}} must be TRUE or FALSE, not {.obj_type_friendly {x}}.", class = "REDCapDM_error_argument", call = call)
  }

  invisible(x)
}

#' Check an argument that is a single text value
#'
#' @inheritParams check_flag
#' @param null_ok,na_ok Whether `NULL` or `NA` (the argument isn't used) are allowed.
#' @noRd
check_string <- function(x, arg, null_ok = FALSE, na_ok = FALSE, call = rlang::caller_env()) {
  if (is.null(x) && null_ok) {
    return(invisible(x))
  }

  if (na_ok && length(x) == 1 && !is.list(x) && is.na(x)) {
    return(invisible(x))
  }

  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    rd_abort(
      c("{.arg {arg}} must be a single text value, e.g. {.val abc}.", "x" = "It is {.obj_type_friendly {x}}."),
      class = "REDCapDM_error_argument", call = call
    )
  }

  invisible(x)
}

#' Check an argument that is a character vector (names of variables, forms, events, patterns...)
#'
#' @inheritParams check_flag
#' @param null_ok Whether `NULL` (the argument isn't used) is allowed.
#' @noRd
check_strings <- function(x, arg, null_ok = TRUE, call = rlang::caller_env()) {
  if (is.null(x) && null_ok) {
    return(invisible(x))
  }

  if (!is.character(x) || length(x) == 0 || anyNA(x) || any(!nzchar(x))) {
    problem <- if (is.character(x) && length(x) > 0) "It has missing or empty values." else "It is {.obj_type_friendly {x}}."
    rd_abort(
      c("{.arg {arg}} must be a character vector without missing or empty values, e.g. {.code c(\"a\", \"b\")}.", "x" = problem),
      class = "REDCapDM_error_argument", call = call
    )
  }

  invisible(x)
}

#' Check an optional text argument whose default is `NA` (query names, titles...)
#'
#' @inheritParams check_flag
#' @noRd
check_text <- function(x, arg, call = rlang::caller_env()) {
  if (length(x) == 1 && !is.list(x) && is.na(x)) {
    return(invisible(x))
  }

  if (!is.character(x)) {
    rd_abort("{.arg {arg}} must be text (or NA, its default), not {.obj_type_friendly {x}}.", class = "REDCapDM_error_argument", call = call)
  }

  invisible(x)
}

#' Check an argument with a fixed set of options
#'
#' @inheritParams check_flag
#' @param choices The options.
#' @param partial Whether the start of an option is enough (as with `match.arg()`).
#' @return The option chosen.
#' @noRd
check_choice <- function(x, arg, choices, partial = FALSE, call = rlang::caller_env()) {
  hit <- if (is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)) {
    if (x %in% choices) x else if (partial) choices[startsWith(choices, x)] else character(0)
  } else {
    character(0)
  }

  if (length(hit) != 1) {
    rd_abort("{.arg {arg}} should be one of {.or {.val {choices}}}.", class = "REDCapDM_error_argument", call = call)
  }

  hit
}

#' Check an argument that is a single number
#'
#' @inheritParams check_flag
#' @param min Minimum value.
#' @param null_ok Whether `NULL` (the argument isn't used) is allowed.
#' @noRd
check_number <- function(x, arg, min = 1, null_ok = TRUE, call = rlang::caller_env()) {
  if (is.null(x) && null_ok) {
    return(invisible(x))
  }

  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x) || x < min) {
    rd_abort("{.arg {arg}} must be a number greater than or equal to {min}, not {.obj_type_friendly {x}}.", class = "REDCapDM_error_argument", call = call)
  }

  invisible(x)
}

#' Check the data and the dictionary
#'
#' @param data,dic The data and the dictionary.
#' @param dic_cols Columns of the dictionary that the function needs.
#' @param dic_rows Whether the function needs a dictionary with rows.
#' @param call The function shown in the error.
#' @noRd
check_inputs <- function(data, dic, dic_cols = NULL, dic_rows = FALSE, call = rlang::caller_env()) {
  if (!is.data.frame(data)) {
    rd_abort(
      c("{.arg data} must be a data frame, not {.obj_type_friendly {data}}.", "i" = "Use the {.field data} element of the output of {.fn redcap_data}."),
      class = "REDCapDM_error_argument", call = call
    )
  }

  if (!is.data.frame(dic)) {
    rd_abort(
      c("{.arg dic} must be a data frame, not {.obj_type_friendly {dic}}.", "i" = "Use the {.field dictionary} element of the output of {.fn redcap_data}."),
      class = "REDCapDM_error_argument", call = call
    )
  }

  if (dic_rows && nrow(dic) == 0) {
    rd_abort(
      c("The dictionary has no rows.", "i" = "Use the dictionary returned by {.fn redcap_data}."),
      class = "REDCapDM_error_argument", call = call
    )
  }

  not_found <- setdiff(dic_cols, names(dic))
  if (length(not_found) > 0) {
    rd_abort(
      c("The dictionary doesn't have the column{?s} {.code {not_found}}.", "i" = "Use the dictionary returned by {.fn redcap_data}."),
      class = "REDCapDM_error_argument", call = call
    )
  }

  invisible(TRUE)
}
