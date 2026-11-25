############### Other functions###############

## Record identifier ----

#' Record identifier of the data
#'
#' `record_id`, the name that `redcap_data()` gives to the identifier. Otherwise, REDCap's record ID is the first field of the dictionary, whatever its name (e.g. `subject_id`) and its position in the data (e.g. with another field, or a column that isn't in the dictionary, in the first column). Only without dictionary, or when its first field isn't in the data, the first column.
#'
#' @param data REDCap dataset.
#' @param dic REDCap dictionary (optional).
#' @return Name of the column of the record identifier.
#' @noRd
id_field <- function(data, dic = NULL) {
  columns <- names(data)
  first_field <- if (is.data.frame(dic) && "field_name" %in% names(dic) && nrow(dic) > 0) as.character(dic$field_name[1]) else NA_character_

  if ("record_id" %in% columns) {
    "record_id"
  } else if (!is.na(first_field) && first_field %in% columns) {
    first_field
  } else {
    columns[1]
  }
}


## Fill_data ----

#' Fill Rows with Values from One Event
#' @description
#' This function fills all rows in the dataset with the value of a particular variable in a specified event. It is an auxiliary function used in the `rd_rlogic` function.
#' @param which_event String specifying the name of the event.
#' @param which_var String specifying the name of the variable.
#' @param data Dataset containing the REDCap data.
#' @param id_col Name of the column of the record identifier (see `id_field()`).
#' @noRd
fill_data <- function(which_event, which_var, data, id_col = id_field(data)) {

  if (!which_event %in% data$redcap_event_name) {
    rd_abort("The logic can't be evaluated after the translation", call = rlang::caller_env())
  }

  fill_values <- dplyr::tibble(.rd_id = data[[id_col]], .rd_event = data$redcap_event_name, var = data[[which_var]]) |>
    dplyr::group_by(.data$.rd_id) |>
    dplyr::mutate(var = dplyr::case_when(.data$.rd_event != .env$which_event ~ NA, TRUE ~ .data$var)) |>
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
check_proj <- function(project, data = NULL, dic = NULL, event_form = NULL, call = rlang::caller_env()) {
  # Ensure 'project' is a list
  if (!is.list(project)) {
    rd_abort(
      c("{.arg project} must be a list, not {.obj_type_friendly {project}}.", "i" = "Use the output of {.fn redcap_data}."),
      class = "REDCapDM_error_argument", call = call
    )
  }
  
  # Error: a data frame instead of the list of redcap_data()
  if (is.data.frame(project)) {
    rd_abort(
      c("{.arg project} must be the list returned by {.fn redcap_data} (with {.field data} and {.field dictionary}), not a data frame.",
        "i" = "To use a data frame, pass it with {.arg data} and the dictionary with {.arg dic}."),
      class = "REDCapDM_error_argument", call = call
    )
  }

  # Check for `data` duplication
  if (!is.null(data)) {
    rd_warn("Data has been provided twice. The function will ignore the {.arg data} argument.")
  }

  # Check for `dic` duplication
  if (!is.null(dic)) {
    rd_warn("Dictionary has been provided twice. The function will ignore the {.arg dic} argument.")
  }

  # Extract data and dictionary from the project
  data <- if (!is.null(project$data)) project$data else if (!is.null(project$dat)) project$dat else NULL
  dic <- if (!is.null(project$dictionary)) project$dictionary else if (!is.null(project$dic)) project$dic else NULL

  # Handle `event_form` duplication
  if ("event_form" %in% names(project)) {
    if (!is.null(event_form)) {
      rd_warn("Event-form has been provided twice. The function will ignore the {.arg event_form} argument.")
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
#' Rounds as REDCap: half is rounded away from zero (`round(2.5)` is 3 and `round(-2.5)` is -3). The numbers with
#' these decimals are read as R reads the numbers written with them (e.g. "2.675" or "2675e-3", as the data): the value
#' is compared with the half read in this way, so `round(2.675, 2)` is 2.68 (2.675 is stored as 2.67499999999999982),
#' while `round(1.49999999)`, `round(1.499999999999999)` and `round(0.4999999999999999)` are below the half. The
#' decimals are whole numbers (as in REDCap).
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
  digits <- trunc(digits)
  a <- abs(x)
  # (scaled in two steps, so that 10^digits doesn't overflow, e.g. with 309 decimals)
  z <- a * 10^(digits %/% 2) * 10^(digits - digits %/% 2)
  # The number k with these decimals, and the half between k and k + 1 (k and a 5 in the next decimal)
  number <- function(k) suppressWarnings(as.numeric(sprintf("%.0fe%.0f", k, -digits)))
  half <- function(k) suppressWarnings(as.numeric(sprintf("%.0f5e%.0f", k, -digits - 1)))
  # The largest number with these decimals that isn't larger than the value (z can be one unit away)
  n <- floor(z)
  n <- n + (number(n + 1) <= a)
  n <- n - (number(n) > a)
  # (a value that already is the number with these decimals is kept: with many digits, the half can be read as the
  # same number, e.g. -280.634537 with 13 decimals)
  out <- sign(x) * number(n + (a >= half(n) & number(n) != a))
  # A number without decimals at this precision is kept as it is: an integer too large to have decimals (e.g. 1e17),
  # or a number that multiplied by 10^digits is Inf (e.g. 1e308 with one decimal), and so are Inf and NaN. Without
  # digits (e.g. NA), the result is NA
  keep <- (!is.na(digits) & (!is.finite(x) | !is.finite(z) | z >= 2^52)) %in% TRUE
  out[keep] <- rep_len(as.numeric(x), length(out))[keep]
  out
}




## Sort_by_identifier ----

#' Sort a query report by record identifier
#'
#' Uses natural ordering, so `"100-2"` comes before `"100-10"` and `"ABC2"`
#' before `"ABC10"`. `Identifier` is converted to numeric only when every
#' identifier is a plain number without leading zeros.
#'
#' @param queries Data frame with an `Identifier` column.
#' @param by_code Logical. If `TRUE`, ties are broken by the `Code` column.
#' @param numeric Logical. `FALSE` keeps `Identifier` as text. The functions pass
#'   whether all the record identifiers of the project are plain numbers, so that
#'   every report of a project has the same type of `Identifier`, whichever records
#'   have queries.
#' @return The sorted data frame.
#' @noRd
sort_by_identifier <- function(queries, by_code = FALSE, numeric = TRUE) {
  id <- id_text(queries$Identifier)
  keys <- list(natural_rank(id))

  if (by_code && "Code" %in% names(queries)) {
    keys <- c(keys, list(natural_rank(queries$Code)))
  }

  queries <- queries[do.call(order, keys), , drop = FALSE]
  rownames(queries) <- NULL

  # Convert only when every identifier survives the conversion unchanged.
  if (isTRUE(numeric) && ids_are_numbers(id)) {
    queries$Identifier <- as.numeric(id_text(queries$Identifier))
  }

  queries
}

#' Record identifiers as text
#'
#' Numbers are written without scientific notation: `as.character(100000)` is
#' `"1e+05"`, which is not the record `100000` of REDCap.
#'
#' @param x Vector of record identifiers.
#' @return A character vector.
#' @noRd
id_text <- function(x) {
  if (!is.numeric(x)) {
    return(as.character(x))
  }

  # Whole numbers (the usual identifiers) are written at once, without decimals or scientific notation; the other
  # numbers one by one
  out <- rep(NA_character_, length(x))
  whole <- !is.na(x) & is.finite(x) & x == trunc(x) & abs(x) < 1e15
  # (`+ 0` writes -0 as 0)
  out[whole] <- sprintf("%.0f", x[whole] + 0)
  other <- !is.na(x) & !whole
  if (!any(other)) {
    return(out)
  }

  out[other] <- vapply(x[other], function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }

    if (!is.finite(value)) {
      return(as.character(value))
    }

    # Keep ordinary decimals readable, increasing precision when necessary.
    for (digits in 15:17) {
      text <- format(
        value,
        scientific = FALSE,
        trim = TRUE,
        digits = digits,
        decimal.mark = ".",
        big.mark = ""
      )

      if (isTRUE(as.numeric(text) == value)) {
        return(text)
      }
    }

    stop(
      "Cannot format a record identifier without losing precision.",
      call. = FALSE
    )
  }, character(1), USE.NAMES = FALSE)

  out
}

#' Whether all the record identifiers are plain numbers (without leading zeros)
#'
#' @param id Vector of record identifiers.
#' @return `TRUE` or `FALSE`.
#' @noRd
ids_are_numbers <- function(id) {
  id <- id_text(id)
  id <- id[!is.na(id)]

  if (!all(grepl("^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$", id))) {
    return(FALSE)
  }

  number <- suppressWarnings(as.numeric(id))

  all(is.finite(number) & id_text(number) == id)
}

natural_rank <- function(x) {
  x <- as.character(x)
  sorted <- unique(x[stringi::stri_order(x, opts_collator = stringi::stri_opts_collator(numeric = TRUE))])
  match(x, sorted)
}


## Events of the queries ----

#' Unique name and label of the events of the data
#'
#' @param data REDCap dataset.
#' @return A data frame with the `code` (unique event name, `redcap_event_name`) and the `label`
#'   (`redcap_event_name.factor`) of each event of the data. Without one of the two columns, the other one is both.
#' @noRd
event_labels <- function(data) {
  code <- if ("redcap_event_name" %in% names(data)) as.character(data$redcap_event_name) else NULL
  label <- if ("redcap_event_name.factor" %in% names(data)) as.character(data$redcap_event_name.factor) else NULL
  if (is.null(code)) code <- label
  if (is.null(label)) label <- code
  if (is.null(code)) {
    return(data.frame(code = character(0), label = character(0), stringsAsFactors = FALSE))
  }
  events <- unique(data.frame(code = code, label = label, stringsAsFactors = FALSE))
  events <- events[!is.na(events$code), , drop = FALSE]
  events$label[is.na(events$label)] <- events$code[is.na(events$label)]
  # (one label for each event)
  events[!duplicated(events$code), , drop = FALSE]
}

#' Events of a query report: the unique event name in `Event` and its label in `EventLabel`
#'
#' The unique event name is the event of a query (the labels can be renamed, or be the same in several arms), and
#' `EventLabel` shows its label. Each query takes the current label of its event. Older rd_query() reports could
#' contain labels in `Event`; older rd_event() reports already contained unique event names, but used labels in
#' their default `Query` text. Recognised legacy labels and default missing-event query texts are normalised
#' to unique event names.
#'
#' @param queries Queries of a report (with an `Event` column).
#' @param events Unique names and labels of the events (see `event_labels()`).
#' @return `queries` with the unique event names in `Event` and their labels in `EventLabel`, after `Event`.
#' @noRd
query_events <- function(queries, events) {
  if (!"Event" %in% names(queries) ||
      nrow(queries) == 0 && "EventLabel" %in% names(queries)) {
    return(queries)
  }

  event <- as.character(queries$Event)

  previous <- if ("EventLabel" %in% names(queries)) {
    as.character(queries$EventLabel)
  } else {
    rep(NA_character_, nrow(queries))
  }

  legacy <- is.na(previous)

  # Convert unambiguous legacy labels in Event to unique event names.
  shared <- events$label[duplicated(events$label)]

  old <- is.na(previous) &
    !event %in% events$code &
    event %in% setdiff(events$label, shared)

  previous[old] <- event[old]
  event[old] <- events$code[match(event[old], events$label)]

  # Old rd_event() reports already have unique event names in Event.
  # Description contains the old label used in the default Query text.
  missing_event <- if ("Field" %in% names(queries)) {
    as.character(queries$Field) %in% "-"
  } else {
    rep(FALSE, nrow(queries))
  }

  if ("Description" %in% names(queries)) {
    recover <- legacy & missing_event & is.na(previous)
    previous[recover] <- as.character(queries$Description[recover])
  }

  # Normalise only the exact legacy default text.
  # Preserve custom query text and modern reports.
  if ("Query" %in% names(queries)) {
    query <- as.character(queries$Query)

    default <- legacy &
      missing_event &
      !is.na(previous) &
      !is.na(event) &
      !is.na(query) &
      query == paste0("The event '", previous, "' is missing.")

    query[default] <- paste0(
      "The event '", event[default], "' is missing."
    )

    queries$Query <- query
  }

  # Apply current labels; retain previous labels for unknown events.
  label <- ifelse(is.na(previous), event, previous)
  known <- event %in% events$code
  label[known] <- events$label[match(event[known], events$code)]

  if ("Description" %in% names(queries) &&
      "Field" %in% names(queries)) {
    description <- as.character(queries$Description)

    renamed <- known &
      queries$Field %in% "-" &
      !is.na(previous) &
      !is.na(description) &
      description == previous

    description[renamed] <- label[renamed]
    queries$Description <- description
  }

  queries$Event <- event
  queries$EventLabel <- label

  dplyr::relocate(queries, "EventLabel", .after = "Event")
}

#' Labels of the events shown in the summary of the queries
#'
#' @param code Unique event names.
#' @param events Unique names and labels of the events (see `event_labels()`).
#' @return The label of each event; a label of several events has the unique event name after it, e.g.
#'   `"Enrollment (enrollment_arm_2)"`, so that the summary doesn't merge them.
#' @noRd
event_display <- function(code, events) {
  code <- as.character(code)
  events <- events[!duplicated(events$code), , drop = FALSE]
  label <- events$label[match(code, events$code)]
  label[is.na(label)] <- code[is.na(label)]
  shared <- events$label[duplicated(events$label)]
  several <- label %in% shared & code %in% events$code
  label[several] <- paste0(label[several], " (", code[several], ")")
  label
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

#' Plain text of a field label
#'
#' Removes the HTML tags of a REDCap field label and decodes its HTML entities, keeping any other text, such as "<18" or ">90", untouched.
#'
#' @param x Character vector of labels.
#' @return Character vector of labels without HTML and with collapsed white space.
#' @noRd
clean_label <- function(x) {
  x <- as.character(x)
  # Tags inside a line of text (e.g. "Weight (<i>kg</i>)") are removed, and tags that break the line (e.g. <br>) become a space
  inline_tags <- "a|abbr|b|big|cite|code|del|em|font|i|ins|kbd|label|mark|q|s|small|span|strike|strong|sub|sup|tt|u|o:p"
  block_tags <- "blockquote|br|caption|center|col|colgroup|dd|div|dl|dt|h[1-6]|hr|img|li|ol|p|pre|table|tbody|td|tfoot|th|thead|tr|ul"
  attribute <- "\\s+[A-Za-z_:][-A-Za-z0-9_:.]*\\s*=\\s*(\"[^\"]*\"|'[^']*'|[^\\s\"'<>]+)"
  tag <- function(names) paste0("</?(", names, ")(", attribute, ")*\\s*/?>")
  x <- gsub("<!--.*?-->", " ", x, perl = TRUE)
  x <- gsub(tag(inline_tags), "", x, perl = TRUE, ignore.case = TRUE)
  x <- gsub(tag(block_tags), " ", x, perl = TRUE, ignore.case = TRUE)
  entities <- c("&nbsp;" = " ", "&lt;" = "<", "&gt;" = ">", "&quot;" = "\"", "&#39;" = "'", "&apos;" = "'", "&amp;" = "&")
  for (entity in names(entities)) {
    x <- gsub(entity, entities[[entity]], x, fixed = TRUE)
  }
  x <- gsub("[\r\n\t\u200B\u200C\u200D\uFEFF]", " ", x, perl = TRUE)
  ifelse(is.na(x), NA_character_, trimws(gsub("\\s+", " ", x, perl = TRUE)))
}
