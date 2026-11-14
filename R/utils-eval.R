## Conditions written as text ----
# The `filter` and `expression` arguments of rd_query(), rd_event() and rd_insert_na() are R conditions written as
# text. They are parsed into R expressions (never pasted into other code and run with eval(parse())) and evaluated as
# in dplyr::filter(): first with the columns of the data and then with the objects of the environment where the user
# called the function (the console, a function or a loop). `data` refers to the data being filtered, so that the
# branching logic translated by rd_dictionary() (`data$var == 1`) can be used as a filter.

#' Parse conditions written as text
#'
#' Several conditions separated by commas must all be met, as in `dplyr::filter()`.
#'
#' @param text A single string, e.g. `"age > 50, dm == 1"`.
#' @return A list of expressions (empty when the text is empty).
#' @noRd
parse_conditions <- function(text) {
  # The final newline closes the call even if the text ends with a comment
  call <- rlang::parse_expr(paste0("list(", text, "\n)"))
  as.list(call)[-1]
}

#' Replace the variable `x` of an expression of rd_query()
#'
#' Only the name `x` is replaced: an "x" inside a text value (`x == 'x'`) or inside another name (`max_x`) is kept.
#'
#' @param expr An expression.
#' @param variable Name of the variable.
#' @noRd
replace_x <- function(expr, variable) {
  if (is.symbol(expr) && identical(expr, quote(x))) {
    return(as.symbol(variable))
  }
  if (is.call(expr)) {
    return(as.call(lapply(as.list(expr), replace_x, variable = variable)))
  }
  expr
}

#' Keep the rows of a data frame that meet conditions written as text
#'
#' @param data A data frame.
#' @param text The conditions, a single string.
#' @param env Environment where the user called the exported function.
#' @param x Name of the variable that `x` stands for (only for the `expression` of rd_query()).
#' @noRd
filter_text <- function(data, text, env, x = NULL) {
  conditions <- parse_conditions(text)
  if (!is.null(x)) {
    conditions <- lapply(conditions, replace_x, variable = x)
  }
  eval_env <- rlang::env(env, data = data)
  dplyr::filter(data, !!!lapply(conditions, rlang::new_quosure, env = eval_env))
}
