############### Repeated instruments and repeating events ###############

# In a REDCap export every record has one base row per event
# (`redcap_repeat_instrument` empty) holding the non-repeating forms, plus one
# row per instance of each repeating instrument, where only that instrument's
# fields are filled. Rows of a repeating event have an empty
# `redcap_repeat_instrument` but a filled `redcap_repeat_instance`.

#' Repeating structure of a REDCap dataset
#'
#' @param data REDCap dataset.
#' @return A list with `instruments` (are there repeating instruments?),
#'   `events` (are there repeating events?), `any` and `pairs`: a data frame with
#'   the `event`/`form` combinations in which `form` is a repeating instrument.
#' @noRd
repeat_info <- function(data) {
  instrument <- repeat_instrument_values(data)
  instance <- if ("redcap_repeat_instance" %in% names(data)) data$redcap_repeat_instance else rep(NA, nrow(data))
  event <- event_values(data)

  is_rep_instr <- !is.na(instrument)

  list(
    instruments = any(is_rep_instr),
    events = any(!is_rep_instr & !is.na(instance)),
    any = any(is_rep_instr | !is.na(instance)),
    pairs = unique(data.frame(
      event = event[is_rep_instr],
      form = instrument[is_rep_instr],
      stringsAsFactors = FALSE
    ))
  )
}

#' Rows of the dataset in which a variable is collected
#'
#' A variable of a repeating instrument only lives in that instrument's rows; any
#' other variable lives in the base rows (which include repeating-event rows).
#' Elsewhere it is empty by design, not missing.
#'
#' @param data REDCap dataset.
#' @param dic REDCap dictionary.
#' @param var Variable name as it appears in `data`.
#' @param pairs `repeat_info()$pairs` computed on the complete dataset, so that
#'   rows removed beforehand (events, filters) don't hide repeating forms.
#' @return Logical vector of length `nrow(data)`. All `TRUE` when the project has
#'   no repeating instruments or when the variable's form can't be determined.
#' @noRd
repeat_home_rows <- function(data, dic, var, pairs = repeat_info(data)$pairs) {
  all_rows <- rep(TRUE, nrow(data))

  instrument <- repeat_instrument_values(data)
  repeating_forms <- unique(c(pairs$form, stats::na.omit(instrument)))
  if (length(repeating_forms) == 0) {
    return(all_rows)
  }

  form <- var_form(var, dic)
  # Unknown variable (e.g. REDCap system fields) or instrument names that don't
  # match the dictionary forms: keep every row, as before.
  if (is.na(form) || !any(repeating_forms %in% dic$form_name)) {
    return(all_rows)
  }

  event <- event_values(data)
  repeats_here <- paste(event, form, sep = "\r") %in% paste(pairs$event, pairs$form, sep = "\r")

  (!is.na(instrument) & instrument == form) | (is.na(instrument) & !repeats_here)
}

#' Form a dataset variable belongs to
#'
#' Handles `.factor` versions, checkbox options (`var___1`), `_recalc` fields and
#' the `_complete`/`_timestamp` fields REDCap adds to every form.
#'
#' @param var Variable name as it appears in the dataset.
#' @param dic REDCap dictionary.
#' @return The form name, or `NA` when it can't be determined. `record_id` is
#'   present in every row, so it returns `NA` for it too.
#' @noRd
var_form <- function(var, dic) {
  if (is.null(dic) || !all(c("field_name", "form_name") %in% names(dic)) || var == "record_id") {
    return(NA_character_)
  }

  base <- sub("\\.factor$", "", var)
  candidates <- unique(c(base, sub("___.*$", "", base), sub("_recalc$", "", base)))
  form <- dic$form_name[match(candidates, dic$field_name)]
  form <- form[!is.na(form)]

  if (length(form) > 0) {
    return(as.character(form[1]))
  }

  form_field <- sub("_(complete|timestamp)$", "", base)
  if (form_field != base && form_field %in% dic$form_name) {
    return(form_field)
  }

  NA_character_
}

#' Label of the repetition of each row of a query report
#'
#' @param x Data frame with the REDCap system columns of the queried rows.
#' @return `"instrument-instance"` for repeating instruments, `"instance"` for
#'   repeating events and `"-"` otherwise.
#' @noRd
repetition_label <- function(x) {
  instrument <- repeat_instrument_values(x)
  instance <- if ("redcap_repeat_instance" %in% names(x)) as.character(x$redcap_repeat_instance) else rep(NA_character_, nrow(x))

  dplyr::case_when(
    !is.na(instrument) & !is.na(instance) ~ paste0(instrument, "-", instance),
    !is.na(instance) ~ instance,
    .default = "-"
  )
}

repeat_instrument_values <- function(data) {
  if (!"redcap_repeat_instrument" %in% names(data)) {
    return(rep(NA_character_, nrow(data)))
  }
  x <- as.character(data$redcap_repeat_instrument)
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x
}

event_values <- function(data) {
  if ("redcap_event_name" %in% names(data)) {
    as.character(data$redcap_event_name)
  } else {
    rep("", nrow(data))
  }
}
