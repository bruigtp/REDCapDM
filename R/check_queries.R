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
#'     but shares the same \code{Identifier} and \code{Description} as an existing record (suggesting a
#'     re-issued or modified query for the same identifier).
#' }
#'
#' The function returns a detailed comparison table of queries with a \code{Modification} factor (one of the four statuses) and an HTML summary table showing counts per status.
#'
#' @param old Data frame containing the previous (older) query report. Must include
#'   \code{Identifier}, \code{Description} and \code{Query} columns (character or factor).
#' @param new Data frame containing the newer query report. Must include
#'   \code{Identifier}, \code{Description} and \code{Query} columns (character or factor).
#'   If \code{new} contains a \code{Code} column, it will be removed at the start of processing.
#' @param report_title Optional single string used as the caption for the HTML summary table.
#'   Defaults to \code{"Comparison report"} when not supplied or when \code{NA}.
#' @param return_viewer Logical; if \code{TRUE} (default) an HTML table (knitr/kable + kableExtra)
#'   summarizing the counts per state is produced and returned in the \code{results} element of the
#'   returned list. If \code{FALSE}, no HTML viewer is produced (useful for non-interactive runs).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{\code{queries}}{A data frame containing all queries present in either \code{old} or \code{new}.
#'     A factor column \code{Modification} indicates the state for each row (levels: \code{Pending},
#'     \code{Solved}, \code{Miscorrected}, \code{New}). The function also reassigns \code{Code}
#'     values so codes are consistent per \code{Identifier}.}
#'   \item{\code{results}}{If \code{return_viewer = TRUE}, an HTML \code{knitr::kable} (styled with
#'     \code{kableExtra}) summarising totals per state. If \code{return_viewer = FALSE}, this is \code{NULL}.}
#' }
#'
#' @details
#' Requirements:
#' \itemize{
#'   \item Both \code{old} and \code{new} must be data frames.
#'   \item Both data frames must contain at least the following character columns:
#'     \code{Identifier}, \code{Description}, and \code{Query}.
#'   \item A \code{Code} column is optional; if present it will be preserved and considered
#'     for sorting and output. If \code{Code} exists in \code{new}, it is removed at the
#'     beginning of the routine to avoid conflicts with re-assigned codes.
#' }
#'
#' The function merges the two reports, constructs composite keys used for comparison, classifies each row into a modification state, detects and re-labels \code{Miscorrected} cases, reassigns a \code{Code} per \code{Identifier} to keep codes consistent, and returns a detailed dataset plus an optional HTML summary viewer.
#'
#' @section Notes and edge cases:
#' \itemize{
#'   \item \strong{Column types:} If \code{Identifier}, \code{Description} or \code{Query} are
#'     factors, they will be used in the comparison — it is recommended to convert them to
#'     character prior to calling \code{check_queries()} to avoid factor-level mismatches.
#'   \item \strong{Sorting:} When \code{Identifier} values contain a dash (e.g. \code{"100-20"}),
#'     the function attempts to split into numeric \code{center} and \code{id} parts for
#'     logical ordering. Otherwise \code{Identifier} is coerced to numeric for ordering.
#'   \item \strong{Miscorrected detection:} A \code{Miscorrected} label is assigned when more
#'     than one row shares the same \code{Identifier + Description} composite and a row is
#'     otherwise classified as \code{New} — this signals a likely re-issued or modified query
#'     for an existing identifier.
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
#' @export

check_queries <- function(old, new, report_title = NULL, return_viewer = TRUE) {
  # Ensure both objects provided are dataframes
  if (!is.data.frame(old) | !is.data.frame(new)) {
    stop("The 'old' and 'new' arguments must be a data frame.", call. = FALSE)
  }
  if (!is.null(report_title) && length(report_title) > 1) {
    stop("There is more than one title for the report, please choose only one.", call. = FALSE)
  }

  # Merge old and new datasets
  new <- new |> dplyr::select(-dplyr::any_of("Code"))
  old[, "comp"] <- paste0(old$Identifier, old$Description, old$Query)
  new[, "comp"] <- paste0(new$Identifier, new$Description, new$Query)
  check <- merge(old, new, by = intersect(names(old), names(new)), all = TRUE)

  # Add columns for comparisons and determine statuses
  check[, "comp"] <- paste0(check$Identifier, check$Description, check$Query)
  check[, "comp2"] <- paste0(check$Identifier, check$Description)
  check[, "Modification"] <- NA
  check[, "Modification"][check$comp %in% old$comp & check$comp %in% new$comp] <- "Pending"
  check[, "Modification"][check$comp %in% old$comp & !check$comp %in% new$comp] <- "Solved"
  check[, "Modification"][!check$comp %in% old$comp & check$comp %in% new$comp] <- "New"

  # Identify miscorrected queries: If a query does not exist in the old report, but there is a new query from the same variable with the same identifier
  check <- check |>
    dplyr::group_by(.data$comp2) |>
    dplyr::mutate(n = dplyr::n())
  check <- as.data.frame(check)

  if (any(check[, "n"] > 1)) {
    dups <- check |>
      dplyr::filter(.data$n > 1 & .data$Modification %in% "New")
    if (nrow(dups) > 0) {
      dups[, "Modification"] <- "Miscorrected"

      check <- check |>
        dplyr::filter(!(.data$n > 1 & .data$Modification %in% "New"))

      check <- rbind(check, dups)
    }
  }

  # Convert the "Modification" column to a factor
  check[, "Modification"] <- factor(check[, "Modification"],
    levels = c("Pending", "Solved", "Miscorrected", "New")
  )

  # Clean up unnecessary columns
  check <- check |>
    dplyr::select(-dplyr::any_of(c("comp", "comp2", "n")))

  # Arrange the dataset by specific fields
  if (any(stringr::str_detect(check$Identifier, "-"))) {
    check <- check |> tidyr::separate("Identifier", c("center", "id"), sep = "([-])", remove = FALSE)
    check[, "center"] <- as.numeric(check[, "center"])
    check[, "id"] <- as.numeric(check[, "id"])
    check <- check[order(check[, "center"], check[, "id"], check[, "Code"], na.last = TRUE), ]
    rownames(check) <- NULL
    check <- check |>
      dplyr::select(-dplyr::any_of(c("center", "id")))
  } else {
    check$Identifier <- as.numeric(check$Identifier)
    check <- check[order(check$Identifier, check$Code), ]
  }

  # Assign new codes to each query to match the old dataset
  check <- data.frame(check |>
    dplyr::group_by(.data$Identifier) |>
    dplyr::mutate(cod = 1:dplyr::n()))
  check$Code <- paste0(as.character(check$Identifier), "-", check$cod)
  check <- check |>
    dplyr::select(-dplyr::any_of("cod"))

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
      stop("There is more than one title for the report, please choose only one.", call. = FALSE)
    }
  }

  # Generate styled HTML summary
  viewer <- NULL
  if (isTRUE(return_viewer)) {
    viewer <- knitr::kable(report, align = c("cc"), row.names = FALSE, caption = report_title, format = "html", longtable = TRUE)
    viewer <- kableExtra::kable_styling(viewer, bootstrap_options = c("striped", "condensed"), full_width = FALSE)
    viewer <- kableExtra::row_spec(viewer, 0, italic = FALSE, extra_css = "border-bottom: 1px solid grey")
  }

  # Return results
  list(
    queries = check,
    results = viewer
  )
}
