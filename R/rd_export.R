#' Export Queries to an Excel File
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function exports a query dataset, typically generated using `rd_query` or `rd_event`, into an `.xlsx` file.
#' It supports adding hyperlinks to specified columns and optional password protection for the worksheet.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping, typically the output of the `redcap_data` function. If provided, it overrides individual `data`, `dic`, and `event_form` arguments.
#' @param queries A data frame containing the identified queries. If `...` is provided, this argument is ignored.
#' @param column A string specifying the column in the dataset that contains hyperlinks. If not specified,
#' hyperlinks will not be added unless a column named `Link` is detected.
#' @param sheet_name A string specifying the name of the sheet in the resulting `.xlsx` file. Defaults to `"Sheet1"`.
#' @param path A string specifying the file path to save the `.xlsx` file. If `NULL`, the file is saved in the
#' current working directory with the name `example.xlsx`.
#' @param password An optional string to password-protect the worksheet, preventing unauthorized edits.
#'
#' @return An `.xlsx` file saved to the specified path, containing the query data and hyperlinks if specified.
#'
#' @examples
#' \dontrun{
#' # Export queries to an Excel file
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
