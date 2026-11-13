## Conditions ----
# Errors, warnings and messages of the package are signalled with cli, so that they are formatted (argument names, values,
# functions...) and have classes that allow to handle them, e.g. `tryCatch(..., REDCapDM_error = function(e) ...)`.

#' Signal an error
#'
#' @param message The message, with cli markup (e.g. `{.arg x}`, `{.val {value}}`) and bullets (`"i"`, `"x"`, `"!"`).
#' @param ... Passed to `cli::cli_abort()`.
#' @param class Classes of the error, added to `REDCapDM_error`.
#' @param call The function shown in the error: by default the one that called `rd_abort()`.
#' @param .envir Environment where the values of the message are evaluated.
#' @noRd
rd_abort <- function(message, ..., class = NULL, call = rlang::caller_env(), .envir = parent.frame()) {
  cli::cli_abort(message, ..., class = c(class, "REDCapDM_error"), call = call, .envir = .envir)
}

#' Signal a warning
#'
#' @inheritParams rd_abort
#' @param class Classes of the warning, added to `REDCapDM_warning`.
#' @noRd
rd_warn <- function(message, ..., class = NULL, .envir = parent.frame()) {
  cli::cli_warn(message, ..., class = c(class, "REDCapDM_warning"), .envir = .envir)
}

#' Show an informative message
#'
#' The messages are not shown with `options(REDCapDM.quiet = TRUE)`.
#'
#' @inheritParams rd_abort
#' @param class Classes of the message, added to `REDCapDM_message`.
#' @noRd
rd_inform <- function(message, ..., class = NULL, .envir = parent.frame()) {
  if (isTRUE(getOption("REDCapDM.quiet"))) {
    return(invisible(NULL))
  }

  cli::cli_inform(message, ..., class = c(class, "REDCapDM_message"), .envir = .envir)
}
