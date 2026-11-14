## Query reports ----
# rd_query(), rd_event() and check_queries() return a list with the queries (`queries`) and the formatted summary
# (`results`). The class REDCapDM_report only adds a short print() for the console: the list and its elements don't
# change, so `x$queries`, `x$results`, `add_to = x` and `rd_export(x)` work as before.

#' Add the class of the query reports
#'
#' @param x List with the elements `queries` and `results`.
#' @noRd
new_report <- function(x) {
  class(x) <- c("REDCapDM_report", "list")
  x
}

#' Print a query report
#'
#' Shows a short summary of the output of [rd_query()], [rd_event()] or [check_queries()]: the number of queries per
#' variable and event (per state in [check_queries()]). The output is still a list: the queries are in `$queries`
#' and the formatted summary in `$results`.
#'
#' @param x Output of [rd_query()], [rd_event()] or [check_queries()].
#' @param ... Not used.
#' @return `x`, invisibly.
#' @examples
#' report <- rd_query(covican, variables = c("copd", "age"), expression = c("is.na(x)", "x > 70"),
#'                    event = "baseline_visit_arm_1")
#' report
#' head(report$queries)
#' @export
print.REDCapDM_report <- function(x, ...) {
  queries <- x$queries
  # With `by_dag = TRUE` there is one data frame per DAG
  if (is.data.frame(queries)) {
    queries <- list(queries)
  }
  if (!is.list(queries) || !all(vapply(queries, is.data.frame, TRUE))) {
    print(unclass(x), ...)
    return(invisible(x))
  }

  columns <- c("Identifier", "Event", "Field", "Modification")
  queries <- lapply(queries, function(q) as.data.frame(q)[, intersect(columns, names(q)), drop = FALSE])
  queries <- if (length(queries) > 0) do.call(rbind, queries) else data.frame()
  control <- "Modification" %in% names(queries)
  title <- if (control) "REDCapDM query control" else "REDCapDM query report"

  n <- nrow(queries)
  if (n == 0) {
    cat(title, ": no queries.\n", sep = "")
    return(invisible(x))
  }
  records <- length(unique(queries$Identifier))
  cat(sprintf("%s: %d %s in %d %s\n\n", title, n, if (n == 1) "query" else "queries", records, if (records == 1) "record" else "records"))

  if (control) {
    counts <- as.data.frame(table(State = queries$Modification), responseName = "Queries")
  } else {
    # rd_event() has no variable (`Field` is "-"): the queries are counted per event
    by <- if (all(queries$Field %in% c("-", NA))) "Event" else c("Field", "Event")
    counts <- as.data.frame(table(queries[by]), responseName = "Queries")
    counts <- counts[counts$Queries > 0, , drop = FALSE]
    names(counts)[names(counts) == "Field"] <- "Variable"
  }
  table_lines <- utils::capture.output(print(counts, row.names = FALSE, right = FALSE))
  cat(sub("\\s+$", "", table_lines), sep = "\n")

  cat("\nEach query is in `$queries`", if (!is.null(x$results)) " and the summary table in `$results`", ".\n", sep = "")
  cat("`rd_export()` saves the queries in an Excel file.\n")
  invisible(x)
}
