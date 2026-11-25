#' Check for Changes Between Two Query Reports
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @description
#' Compare an older query report (`old`) with a newer one (`new`) and classify each
#' query into one of four statuses:
#' \itemize{
#'   \item \strong{Pending} — the same query is present in both reports (no change detected),
#'   \item \strong{Solved} — the query was present in the old report but is absent from the new report,
#'   \item \strong{New} — the query appears in the new report but was not present in the old report,
#'   \item \strong{Miscorrected} — a special case where a query in the new report is marked as \code{New}
#'     but the old report has a query with the same \code{Identifier}, \code{Event}, \code{Repetition} and
#'     \code{Field} (each of them when both reports have it; \code{Description} when they don't have \code{Field}),
#'     suggesting a re-issued or modified query for the same record and field.
#' }
#'
#' The function returns a detailed comparison table of queries with a \code{Modification} factor (one of the four statuses) and an HTML summary table showing counts per status.
#'
#' @param old Data frame containing the previous (older) query report. Must include \code{Identifier} (text, factor or number), \code{Description} and \code{Query} columns (character or factor).
#' @param new Data frame containing the newer query report. Must include \code{Identifier} (text, factor or number), \code{Description} and \code{Query} columns (character or factor). If \code{new} contains a \code{Code} column, it will be removed at the start of processing.
#' @param report_title Optional single string used as the caption for the HTML summary table. With \code{NULL} (the default), the caption is \code{"Comparison report"}; \code{NA} is not accepted.
#' @param return_viewer Logical; if \code{TRUE} (default) an HTML table (knitr/kable + kableExtra) summarizing the counts per state is produced and returned in the \code{results} element of the returned list. If \code{FALSE}, no HTML viewer is produced (useful for non-interactive runs).
#'
#' @return A list of class `REDCapDM_report`, which prints a short summary (see [print.REDCapDM_report()]), with two elements:
#' \describe{
#'   \item{\code{queries}}{A data frame containing all queries present in either \code{old} or \code{new}.
#'     A factor column \code{Modification} indicates the state for each row (levels: \code{Pending},
#'     \code{Solved}, \code{Miscorrected}, \code{New}). The function also reassigns \code{Code}
#'     values so codes are consistent per \code{Identifier}: a code is the number of each query of its record in this
#'     result, not a permanent identifier of the query (the same query can have another code in another comparison).}
#'   \item{\code{results}}{If \code{return_viewer = TRUE}, an HTML \code{knitr::kable} (styled with
#'     \code{kableExtra}) summarising totals per state. If \code{return_viewer = FALSE}, this is \code{NULL}.}
#' }
#'
#' @details
#' Requirements:
#' \itemize{
#'   \item Both \code{old} and \code{new} must be data frames.
#'   \item Both data frames must contain at least the following columns:
#'     \code{Identifier} (text, factor or number), \code{Description}, and \code{Query}.
#'   \item Every query must have an \code{Identifier} (the record identifier of the data).
#'   \item A \code{Code} column is optional; the codes of \code{old} are used to sort the queries, and the
#'     codes of the result are assigned again (see the value). If \code{Code} exists in \code{new}, it is removed at
#'     the beginning of the routine to avoid conflicts with re-assigned codes.
#' }
#'
#' The function merges the two reports, constructs composite keys used for comparison, classifies each row into a modification state, detects and re-labels \code{Miscorrected} cases, reassigns a \code{Code} per \code{Identifier} to keep codes consistent, and returns a detailed dataset plus an optional HTML summary viewer.
#'
#' @section Notes and edge cases:
#' \itemize{
#'   \item \strong{Column types:} If \code{Identifier}, \code{Description} or \code{Query} are
#'     factors, they will be used in the comparison — it is recommended to convert them to
#'     character prior to calling \code{check_queries()} to avoid factor-level mismatches.
#'   \item \strong{Sorting:} The rows are sorted by \code{Identifier} in natural order (e.g. \code{"100-2"}
#'     before \code{"100-10"}). \code{Identifier} is returned as a number only when every identifier is a
#'     plain number without leading zeros (e.g. \code{"007"} is kept as text).
#'   \item \strong{Identity of a query:} a query is the same in both reports when it has the same \code{Identifier},
#'     \code{Event} and \code{Repetition} (when both reports have them), \code{Field} (when both reports have it;
#'     otherwise \code{Description}) and \code{Query}. \code{Event} is the unique event name and \code{Field} the
#'     name of the field: changing the display labels of events, arms or fields doesn't solve their queries. Their labels
#'     (\code{EventLabel} and \code{Description}) and the other columns (e.g. \code{Link} or \code{DAG}) are taken from
#'     the new report, so a query whose label or link changed is still one pending query.
#'   \item \strong{Reports created before \code{EventLabel} existed:} \code{rd_query()} could store event labels
#'     in \code{Event} when factor labels were available. Reports from \code{rd_event()} already stored unique event
#'     names there, but used labels in their default \code{Query} text. Recognised legacy labels and default
#'     missing-event query texts are normalised to unique event names. Shared labels cannot be resolved automatically.
#'   \item \strong{Miscorrected detection:} A \code{Miscorrected} label is assigned to a query that would
#'     otherwise be \code{New} when the old report has a query of the same record (\code{Identifier}), event
#'     (\code{Event}) and repetition (\code{Repetition}), when both reports have these columns, and of the same
#'     variable: the same \code{Field} when both reports have it, or else the same \code{Description}. This signals a
#'     likely re-issued or modified query. New queries without such a query in the old report are
#'     \code{New}, also when there are several of them for the same field.
#' }
#'
#' @examples
#' # Minimal reproducible example
#' old <- data.frame(
#'   Identifier = c("100-1", "100-2", "200-1"),
#'   Description = c("age check", "weight check", "lab miss"),
#'   Query = c("is.na(age)", "is.na(weight)", "missing lab result"),
#'   Code = c("100-1-1", "100-2-1", "200-1-1"),
#'   stringsAsFactors = FALSE
#' )
#'
#' new <- data.frame(
#'   Identifier = c("100-1", "200-1", "300-1"),
#'   Description = c("age check", "lab miss", "new query"),
#'   Query = c("is.na(age)", "missing lab result (clarify)", "is.na(x)"),
#'   stringsAsFactors = FALSE
#' )
#'
#' res <- check_queries(old = old, new = new, report_title = "My Query Comparison")
#' # detailed table
#' head(res$queries)
#' # HTML summary (if in an RMarkdown or interactive viewer)
#' res$results
#'
#' @family query functions
#' @export

check_queries <- function(old, new, report_title = NULL, return_viewer = TRUE) {
  # Ensure both objects provided are dataframes
  if (!is.data.frame(old) || !is.data.frame(new)) {
    rd_abort("The {.arg old} and {.arg new} arguments must be data frames.", class = "REDCapDM_error_argument")
  }
  if (!is.null(report_title) && length(report_title) > 1) {
    rd_abort("There is more than one title for the report, please choose only one.")
  }
  
  # Error: invalid arguments
  check_string(report_title, "report_title", null_ok = TRUE)
  check_flag(return_viewer, "return_viewer")
  
  # Error: both must be query reports
  needed <- c("Identifier", "Description", "Query")
  not_found <- unique(c(setdiff(needed, names(old)), setdiff(needed, names(new))))
  if (length(not_found) > 0) {
    rd_abort(
      c("The {.arg old} and {.arg new} arguments must be query reports (the {.field queries} element of {.fn rd_query} or {.fn rd_event}).", "x" = "Missing columns: {.field {not_found}}."),
      class = "REDCapDM_error_argument"
    )
  }

  # The texts of both reports in UTF-8: the same text in another encoding (e.g. Latin-1) is the same query
  utf8 <- function(x) {
    if (is.character(x)) {
      x[] <- enc2utf8(x)
    } else if (is.factor(x)) {
      levels(x) <- enc2utf8(levels(x))
    }
    x
  }
  old[] <- lapply(old, utf8)
  new[] <- lapply(new, utf8)

  # Error: queries without identifier (records without identifier in the data, or incomplete or edited reports)
  no_id <- c(old = sum(is.na(old$Identifier) | trimws(as.character(old$Identifier)) == ""), new = sum(is.na(new$Identifier) | trimws(as.character(new$Identifier)) == ""))
  if (any(no_id > 0)) {
    rd_abort(
      c("Every query must have an {.field Identifier}.", "x" = "{no_id[['old']]} quer{?y/ies} of {.arg old} and {no_id[['new']]} of {.arg new} don't have it.", "i" = "They come from records without identifier in the data, or from reports that are incomplete or were edited."),
      class = "REDCapDM_error_argument"
    )
  }

  # Events: the unique event name in `Event` and its label in `EventLabel`. Reports created before `EventLabel` existed
  # have the label in `Event`: it is replaced by the unique event name of the new report, when a single event has it
  if (all(c("Event", "EventLabel") %in% names(new)) && "Event" %in% names(old)) {
    events <- data.frame(code = as.character(new$Event), label = as.character(new$EventLabel), stringsAsFactors = FALSE)
    events <- unique(events[!is.na(events$code) & !events$code %in% "-", , drop = FALSE])
    events <- events[!duplicated(events$code), , drop = FALSE]
    old <- query_events(old, events)
    new <- query_events(new, events)
  }

  # Columns that identify a query. Event and Repetition are used when both reports have them, so the same variable in different events or instances of a repeated instrument are different queries.
  # Field too, so two fields with the same description are different queries. The event is its unique name, and the
  # field its name: their labels (EventLabel and Description) can be renamed in REDCap, and are taken from the new
  # report, like the other columns. Without Field in both reports, Description identifies the field.
  both <- intersect(names(old), names(new))
  key_cols <- c("Identifier", intersect(c("Event", "Repetition", "Field"), both), if (!"Field" %in% both) "Description")
  # (each value with its length before it and without carriage returns, which merge() uses to join the columns: two
  # different queries never have the same identity, e.g. "Age\rRule" and "Check" or "Age" and "Rule\rCheck")
  query_key <- function(df, cols) {
    parts <- lapply(unname(as.list(df[cols])), function(x) {
      x <- gsub("\r", "\001r", gsub("\001", "\001\001", id_text(x), fixed = TRUE), fixed = TRUE)
      ifelse(is.na(x), "NA;", paste0(nchar(x, type = "bytes"), ":", x, ";"))
    })
    do.call(paste0, parts)
  }

  # Merge old and new datasets
  new <- new |> dplyr::select(-dplyr::any_of("Code"))
  # (internal columns with names that the reports don't have, e.g. a column `comp` of the reports is kept)
  internal_name <- function(base) utils::tail(make.unique(c(unique(c(names(old), names(new))), base), sep = "_"), 1)
  comp <- internal_name("comp")
  comp2 <- internal_name("comp2")
  old[[comp]] <- query_key(old, c(key_cols, "Query"))
  new[[comp]] <- query_key(new, c(key_cols, "Query"))
  # (the identity of the queries of the old report, without the query: Identifier, Event, Repetition, Field and
  # Description)
  old_keys <- query_key(old, key_cols)

  # The queries of both reports take the other columns of the new report (e.g. Link or DAG): otherwise a query whose
  # link changed would be two rows
  # (a column with a different class in each report, e.g. a factor in one of them, or factors with different levels, is
  # kept as text: the values of the new report can't be copied into a factor that doesn't have them)
  other_cols <- setdiff(intersect(names(old), names(new)), c(key_cols, "Query", comp))
  in_new <- match(old[[comp]], new[[comp]])
  for (col in other_cols) {
    same_class <- identical(class(old[[col]]), class(new[[col]])) && identical(levels(old[[col]]), levels(new[[col]]))
    if (!same_class) {
      old[[col]] <- as.character(old[[col]])
      new[[col]] <- as.character(new[[col]])
    }
    if (any(!is.na(in_new))) {
      old[[col]][!is.na(in_new)] <- new[[col]][in_new[!is.na(in_new)]]
    }
  }

  check <- merge(old, new, by = intersect(names(old), names(new)), all = TRUE)

  # Add columns for comparisons and determine statuses
  check[[comp]] <- query_key(check, c(key_cols, "Query"))
  check[[comp2]] <- query_key(check, key_cols)
  check[["Modification"]] <- rep(NA_character_, nrow(check))
  check[, "Modification"][check[[comp]] %in% old[[comp]] & check[[comp]] %in% new[[comp]]] <- "Pending"
  check[, "Modification"][check[[comp]] %in% old[[comp]] & !check[[comp]] %in% new[[comp]]] <- "Solved"
  check[, "Modification"][!check[[comp]] %in% old[[comp]] & check[[comp]] %in% new[[comp]]] <- "New"
  check <- as.data.frame(check)

  # Identify miscorrected queries: a query that doesn't exist in the old report, when the old report has a query of the
  # same record, event, repetition, field and description (e.g. re-issued with another value). Two new queries of
  # the same field without a query in the old report are new
  miscorrected <- check$Modification %in% "New" & check[[comp2]] %in% old_keys
  if (any(miscorrected)) {
    dups <- check[miscorrected, , drop = FALSE]
    dups[, "Modification"] <- "Miscorrected"
    check <- rbind(check[!miscorrected, , drop = FALSE], dups)
  }

  # Convert the "Modification" column to a factor
  check[, "Modification"] <- factor(check[, "Modification"],
    levels = c("Pending", "Solved", "Miscorrected", "New")
  )

  # Clean up unnecessary columns
  check <- check[, setdiff(names(check), c(comp, comp2)), drop = FALSE]

  # Arrange the dataset by identifier and previous code
  # (identifiers kept as text in a report, e.g. with leading zeros in some of them, are kept as text)
  check <- sort_by_identifier(check, by_code = TRUE, numeric = !is.character(old$Identifier) && !is.character(new$Identifier))

  # Assign new codes to each query to match the old dataset (the number of each query of a record)
  check <- as.data.frame(check)
  number <- if (nrow(check) > 0) stats::ave(seq_len(nrow(check)), id_text(check$Identifier), FUN = seq_along) else integer(0)
  check$Code <- if (nrow(check) > 0) paste0(id_text(check$Identifier), "-", number) else character(0)

  # Summarize query statuses
  report <- check |>
    dplyr::group_by(.data$Modification, .drop = FALSE) |>
    dplyr::summarise("total" = dplyr::n())
  report <- as.data.frame(report)
  report <- report[order(as.numeric(report$total), decreasing = TRUE), ]
  names(report) <- c("State", "Total")
  rownames(report) <- NULL

  # Handle report title
  if (all(is.na(report_title))) {
    report_title <- "Comparison report"
  } else {
    if (length(report_title) > 1) {
      rd_abort("There is more than one title for the report, please choose only one.")
    }
  }

  # Generate styled HTML summary
  viewer <- NULL
  if (isTRUE(return_viewer)) {
    viewer <- knitr::kable(report, align = c("cc"), row.names = FALSE, caption = report_title, format = "html", longtable = TRUE)
    viewer <- style_report(viewer)
  }

  # Return results
  new_report(list(
    queries = check,
    results = viewer
  ))
}
