#' Check for changes between two reports of the queries
#'
#' This function compares a previous report of queries with a new one and allows you to check which queries are new, which have been modified, and which remain unchanged.
#' @param old Previous version of the queries report.
#' @param new New version of the queries report. The status of each query is determined using this object.
#' @param report_title Character string specifying the title of the report.
#' @return A list consisting of a dataframe with every single query of both reports and a column containing the status of the queries (new, solved, miscorrected or pending) compared to the previous query report. There is also a summary of the total number of queries per category in addition to this dataframe.
#' @examples
#' # Example of a query
#' data_old <- rd_query(covican,
#'                      variables = "copd",
#'                      expression = "is.na(x)",
#'                      event = "baseline_visit_arm_1")
#' data_new <- rbind(data_old$queries[1:5,], c("100-20",rep("abc",8)))
#'
#' # Control of queries
#' check <- check_queries(old = data_old$queries,
#'                        new = data_new)
#' @export

check_queries <-function(old, new, report_title = NULL)
  {

  # Creation of the merged dataset
  new <- new %>% dplyr::select(-"Code")
  old[,"comp"] <- paste0(old$Identifier, old$Description, old$Query)
  new[,"comp"] <- paste0(new$Identifier, new$Description, new$Query)
  check <- merge(old, new, by = intersect(names(old), names(new)), all = TRUE)

  # Checking each type of query
  check[,"comp"] <- paste0(check$Identifier, check$Description, check$Query)
  check[,"comp2"] <- paste0(check$Identifier, check$Description)
  check[,"Modification"] <- NA
  check[,"Modification"][check$comp %in% old$comp & check$comp %in% new$comp] <- "Pending"
  check[,"Modification"][check$comp %in% old$comp & !check$comp %in% new$comp] <- "Solved"
  check[,"Modification"][!check$comp %in% old$comp & check$comp %in% new$comp] <- "New"

  # Adding the new category of miscorrected if a query is not present in the old report but there is a new query from the same variable with the same identifier
  check <- check %>%
              dplyr::group_by(.data$comp2) %>%
              dplyr::mutate(n = dplyr::n())
  check <- as.data.frame(check)

  # The duplicated queries are the ones that were miscorrected
  if (any(check[, "n"] > 1)) {
    dups <- check %>%
              dplyr::filter(.data$n > 1 & .data$Modification %in% "New")
    if (nrow(dups) > 0) {
      dups[, "Modification"] <- "Miscorrected"

      check <- check %>%
        dplyr::filter(!(.data$n > 1 & .data$Modification %in% "New"))

      check <- rbind(check, dups)
    }
  }

  # Convert the column Modification in a factor
  check[, "Modification"] <- factor(check[, "Modification"],
                                   levels = c("Pending", "Solved", "Miscorrected", "New"))

  # Remove exceeding columns
  check <- check %>%
              dplyr::select(-"comp", -"comp2", -"n")

  # Arrange the dataset
  if (any(stringr::str_detect(check$Identifier, "-"))) {

    check <- check %>% tidyr::separate("Identifier", c("center", "id"), sep = "([-])", remove = FALSE)
    check[, "center"] <- as.numeric(check[, "center"])
    check[, "id"] <- as.numeric(check[, "id"])
    check <- check[order(check[, "center"], check[, "id"], check[, "Code"], na.last = TRUE), ]
    rownames(check) <- NULL
    check <- check %>%
              dplyr::select(-"center", -"id")

  } else {

    check$Identifier <- as.numeric(check$Identifier)
    check <- check[order(check$Identifier, check$Code), ]

  }

  # We update the code of each query in order to match the old dataset
  check <- data.frame(check %>%
                          dplyr::group_by(.data$Identifier) %>%
                          dplyr::mutate(cod = 1:dplyr::n()))
  check$Code <- paste0(as.character(check$Identifier), "-", check$cod)
  check <- check %>%
              dplyr::select(-"cod")

  # Creation of the report indicating the variables checked
  report <- check %>%
                dplyr::group_by(.data$Modification, .drop = FALSE) %>%
                dplyr::summarise("total" = dplyr::n())
  report <- as.data.frame(report)
  report <- report[order(as.numeric(report$total), decreasing = TRUE), ]
  names(report) <- c("State", "Total")
  rownames(report) <- NULL

  # Before starting we check if there is more than one report_title and if it isn't the case we stabilish the caption for the report_title
  if (all(is.na(report_title))) {
    report_title <- "Comparison report"
  } else {
    if (length(report_title) > 1) {
      stop("There is more than one title for the report, please choose only one.", call. = FALSE)
    }
  }

  # Adaptation to viewer
  viewer <- knitr::kable(report, align = c("cc"), row.names = FALSE, caption = report_title, format = "html", longtable = TRUE)
  viewer <- kableExtra::kable_styling(viewer, bootstrap_options = c("striped", "condensed"), full_width = FALSE)
  viewer <- kableExtra::row_spec(viewer, 0, italic = FALSE, extra_css = "border-bottom: 1px solid grey")

  # Return the final product
  list(queries = check,
       results = viewer)
}

