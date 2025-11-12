#' Check for Changes Between Two Query Reports
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function compares an old query report with a new one to identify the status of each query.
#' Queries are categorized as `new`, `solved`, `pending`, or `miscorrected`.
#' The function generates a detailed comparison dataframe and a summary report.
#'
#' @param old Dataframe containing the previous version of the query report.
#' @param new Dataframe containing the new version of the query report.\cr
#' This is compared against the `old` report to determine query statuses.
#' @param report_title (Optional) A character string specifying the title for the generated report.\cr
#' If not provided, the default title will be "Comparison report".
#' @param return_viewer logical, whether to return the HTML viewer (default TRUE)
#'
#' @return A list containing:
#' \item{queries}{A dataframe with all individual queries from both reports and a status column (`new`, `solved`, `pending`, or `miscorrected`).}
#' \item{results}{A styled HTML summary table showing the total number of queries in each status category.}
#'
#' @examples
#' # Example of a query
#' data_old <- rd_query(covican,
#'   variables = "copd",
#'   expression = "is.na(x)",
#'   event = "baseline_visit_arm_1"
#' )
#' data_new <- rbind(data_old$queries[1:5, ], c("100-20", rep("abc", 8)))
#'
#' # Compare the two query reports
#' check <- check_queries(
#'   old = data_old$queries,
#'   new = data_new
#' )
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
