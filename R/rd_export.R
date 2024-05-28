#' Exporting Query Dataset
#'
#' This function exports a query report, generated using the `rd_query` or `rd_event` functions, to an .xlsx file.
#'
#' @param ... List containing the data frame of queries. This list must be the output of the `rd_query` or `rd_event` functions.
#' @param queries Data frame containing the identified queries. If the list is specified, this argument is not required.
#' @param column Character element specifying the column containing the link for each query.
#' @param sheet_name Character element specifying the sheet name of the resulting xlsx file.
#' @param path Character element specifying the file path to save the xlsx file. If `NULL`, the file will be created in the current working directory.
#' @param password String with the password to protect the worksheet and prevent others from making changes.
#' @return An .xlsx file containing all the queries and, if available, hyperlinks to each of them.
#'
#' @export

rd_export <- function(..., queries = NULL, column = NULL, sheet_name = NULL, path = NULL, password = NULL)
{
  # If the entire list resulting from the 'redcap_data' function is used
  project <- c(...)
  if(!is.null(project)){
    if(!is.null(queries)){

      warning("Queries have been specified twice so the function will not use the information in the queries argument.")

    }
    queries <- project$queries
  }

  # Making sure that the object data is a data.frame
  queries <- as.data.frame(queries)

  # Warning: links detected but the column argument is not specified.
  if (is.null(column) & !"Link" %in% names(queries) & any(queries %>% dplyr::summarise_all(~ any(grepl("https", .))))) {

    warning("A column containing links was detected. If you want to convert them into hyperlinks, please specify the column argument.", call. = F)

  }

  # Create a new workbook
  wb <- openxlsx::createWorkbook()

  # Add a new worksheet to the workbook
  sheet_name <- if (!is.null(sheet_name)) {
    sheet_name
  } else {
    "Sheet1"
  }
  sheet <- openxlsx::addWorksheet(wb, sheet_name)

  # Converting the links into hyperlinks
  if (!is.null(column) | "Link" %in% names(queries)) {

    # Stabilish the column name
    column <- if (!is.null(column)) {
      if (column %in% names(queries)) {
        column
      } else {

        # Error: the named column is not present in the dataset
        stop("The column you have specified does not exist in the dataset.", call. = F)

      }
    } else {
      "Link"
    }

    # Add hyperlinks to a cell
    class(queries[, column]) <- "hyperlink"

  }

  # Write the data frame to the worksheet
  openxlsx::writeDataTable(wb = wb, sheet = sheet, x = queries, startRow = 1, startCol = 1, rowNames = F, tableStyle = "TableStyleLight11")

  # Align cells to the center
  openxlsx::addStyle(wb = wb, sheet = sheet, style = openxlsx::createStyle(halign = "CENTER"), rows = 1:(nrow(queries) + 1), cols = 1:length(queries), gridExpand = T)

  # Cell widths
  openxlsx::setColWidths(wb = wb, sheet = sheet, cols = 1:length(queries), widths = "auto")

  # Path to the file
  path <- if (!is.null(path)) {
    path
  } else {
    paste0(getwd(), "/example.xlsx")
  }

  # Save workbook to a file with password
  if (!is.null(password)) {

    openxlsx::protectWorksheet(wb, sheet = sheet, password = password)
    openxlsx::saveWorkbook(wb, path, overwrite = T)
    message(paste0("The file has been successfully created in '", path, "' with password protection."))

  } else {

    # Save workbook to a file
    openxlsx::saveWorkbook(wb, path, overwrite = T)
    message(paste0("The file has been successfully created in '", path, "'."))

  }
}



