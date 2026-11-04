#' Export Queries to an Excel File
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Export a query dataset (e.g., from `rd_query` or `rd_event`) to an `.xlsx` file. The function can optionally convert a column of URLs into Excel hyperlinks and apply password protection to the worksheet.
#'
#' @param project A list containing the dataframe of queries and results (expected `rd_query` or `rd_event` output). Overrides `queries`.
#' @param queries A data frame of identified queries.
#' @param column Name of the column containing URLs to convert into hyperlinks. If `NULL`, hyperlinks are added only if a `Link` column exists.
#' @param sheet_name Name of the Excel sheet in the resulting `.xlsx` file. Default: `"Sheet1"`.
#' @param path File path for saving the `.xlsx` file. If `NULL`, the file is saved as `"example.xlsx"` in the working directory.
#' @param password Optional password to protect the worksheet from edits.
#'
#' @return An `.xlsx` file written to the specified path.
#'
#' @examples
#' \dontrun{
#' rd_export(
#'   queries = my_queries,
#'   column = "Link",
#'   sheet_name = "My Queries",
#'   path = "queries.xlsx"
#' )
#' }
#'
#' @export

rd_export <- function(project = NULL, queries = NULL, column = NULL, sheet_name = NULL, path = NULL, password = NULL) {

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    if (!is.null(queries)) {
      warning("Queries has been provided twice. The function will ignore the `queries` argument.")
    }
    queries <- project$queries
  }

  # Ensure `queries` is a data frame
  queries <- as.data.frame(queries)

  # Warning: links detected but the column argument is not specified.
  if (is.null(column) & !"Link" %in% names(queries) & any(queries |> dplyr::summarise_all(~ any(grepl("https", .))))) {
    warning("Links were detected in the dataset. To convert them into hyperlinks in the Excel file, specify the `column` argument.", call. = FALSE)
  }

  # Create a new workbook
  wb <- openxlsx::createWorkbook()

  # Set the worksheet name
  sheet_name <- if (!is.null(sheet_name)) {
    sheet_name
  } else {
    "Sheet1"
  }
  sheet <- openxlsx::addWorksheet(wb, sheet_name)

  # Handle hyperlink conversion if a column is specified or `Link` is present
  if (!is.null(column) | "Link" %in% names(queries)) {
    # Determine the column name
    column <- if (!is.null(column)) {
      if (column %in% names(queries)) {
        column
      } else {
        # Error: Stop execution if the specified column doesn't exist
        stop("The specified column for hyperlinks does not exist in the dataset. Please review the `column` argument.", call. = FALSE)
      }
    } else {
      "Link"
    }

    # Mark the column as hyperlinks
    class(queries[, column]) <- "hyperlink"
  }

  # Write data to the worksheet
  openxlsx::writeDataTable(
    wb = wb,
    sheet = sheet,
    x = queries,
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    tableStyle = "TableStyleLight11"
  )

  # Center-align cells
  openxlsx::addStyle(
    wb = wb,
    sheet = sheet,
    style = openxlsx::createStyle(halign = "CENTER"),
    rows = 1:(nrow(queries) + 1),
    cols = seq_along(queries),
    gridExpand = TRUE
  )

  # Automatically adjust column widths
  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet,
    cols = seq_along(queries),
    widths = "auto"
  )

  # Determine the file path for saving
  path <- if (!is.null(path)) {
    path
  } else {
    paste0(getwd(), "/example.xlsx")
  }

  # Save the workbook with or without password protection
  if (!is.null(password)) {
    openxlsx::protectWorksheet(wb, sheet = sheet, password = password)
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    message(paste0("The file has been successfully created in '", path, "' with password protection."))
  } else {
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    message(paste0("The file has been successfully created in '", path, "'."))
  }
}
