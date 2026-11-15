#' Export Queries to an Excel File
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Export a query dataset (e.g., from `rd_query` or `rd_event`) to an `.xlsx` file. The function can optionally convert a column of URLs into Excel hyperlinks and apply password protection to the worksheet.
#'
#' @param project A list containing the dataframe of queries and results (expected `rd_query` or `rd_event` output). Overrides `queries`. A data frame of queries is also accepted (e.g. `rd_export(report$queries)`).
#' @param queries A data frame of identified queries.
#' @param column Name of the column containing URLs to convert into hyperlinks. If `NULL`, hyperlinks are added only if a `Link` column exists.
#' @param sheet_name Name of the Excel sheet in the resulting `.xlsx` file. Default: `"Sheet1"`.
#' @param path File path for saving the `.xlsx` file. If `NULL`, the file is saved as `"example.xlsx"` in the working directory.
#' @param password Optional password to protect the worksheet from edits.
#'
#' @return An `.xlsx` file written to the specified path.
#'
#' @examples
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   result <- rd_query(covican, variables = "age", expression = "x > 80")
#'   rd_export(
#'     result,
#'     sheet_name = "My Queries",
#'     path = file.path(tempdir(), "queries.xlsx")
#'   )
#' }
#'
#' @family query functions
#' @export

rd_export <- function(project = NULL, queries = NULL, column = NULL, sheet_name = NULL, path = NULL, password = NULL) {

  # Handle potential overwriting when both `project` and other arguments are provided
  # A data frame of queries given as the first argument (e.g. rd_export(report$queries))
  if (is.data.frame(project) && is.null(queries)) {
    queries <- project
    project <- NULL
  }

  if (!is.null(project)) {
    # Error: `project` must be the list returned by rd_query() or rd_event()
    if (!is.list(project) || is.data.frame(project)) {
      rd_abort(
        c("{.arg project} must be the list returned by {.fn rd_query} or {.fn rd_event}, not {.obj_type_friendly {project}}.", "i" = "To export a data frame, use {.arg queries}."),
        class = "REDCapDM_error_argument"
      )
    }
    if (!is.null(queries)) {
      rd_warn("Queries has been provided twice. The function will ignore the {.arg queries} argument.")
    }
    queries <- project$queries
  }
  
  # Error: nothing to export
  if (is.null(queries)) {
    rd_abort("There are no queries to export: provide the output of rd_query() or rd_event() in {.arg project}, or a data frame in {.arg queries}.")
  }
  
  # Error: invalid arguments
  if (!is.data.frame(queries)) {
    rd_abort("{.arg queries} must be a data frame. With {.code rd_query(by_dag = TRUE)} the queries are a list with one data frame per DAG: export each of them.")
  }
  if (ncol(queries) == 0) {
    rd_abort("{.arg queries} has no columns: there is nothing to export.")
  }
  for (arg in c("column", "sheet_name", "path", "password")) {
    check_string(get(arg), arg, null_ok = TRUE)
  }

  # Error: Excel sheet names have up to 31 characters and can't have / \ ? * : [ ]
  if (!is.null(sheet_name) && (nchar(sheet_name) > 31 || grepl("[/\\\\?*:\\[\\]]", sheet_name, perl = TRUE))) {
    rd_abort(
      c("{.arg sheet_name} is not a valid name for an Excel sheet: {.val {sheet_name}}.", "i" = "It can have up to 31 characters and none of {.code / \\ ? * : [ ]}."),
      class = "REDCapDM_error_argument"
    )
  }

  # Error: `path` is a folder, not a file
  if (!is.null(path) && dir.exists(path)) {
    rd_abort(
      c("{.arg path} is a folder: {.path {path}}.", "i" = "Give the name of the file, e.g. {.code file.path(path, \"queries.xlsx\")}."),
      class = "REDCapDM_error_file"
    )
  }

  # Ensure `queries` is a data frame
  queries <- as.data.frame(queries)

  # Warning: links detected but the column argument is not specified.
  if (is.null(column) && !"Link" %in% names(queries) && any(queries |> dplyr::summarise_all(~ any(grepl("https", .))))) {
    rd_warn("Links were detected in the dataset. To convert them into hyperlinks in the Excel file, specify the {.arg column} argument.")
  }

  rlang::check_installed("openxlsx", reason = "to export the queries to an .xlsx file.")

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
  if (!is.null(column) || "Link" %in% names(queries)) {
    # Determine the column name
    column <- if (!is.null(column)) {
      if (column %in% names(queries)) {
        column
      } else {
        # Error: Stop execution if the specified column doesn't exist
        rd_abort("The specified column for hyperlinks does not exist in the dataset. Please review the {.arg column} argument.")
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
  
  # Error: the folder of the file must exist
  if (!dir.exists(dirname(path))) {
    rd_abort(c("The folder {.path {dirname(path)}} does not exist.", "i" = "Please create it or review the {.arg path} argument."), class = "REDCapDM_error_file")
  }

  # Save the workbook with or without password protection
  if (!is.null(password)) {
    openxlsx::protectWorksheet(wb, sheet = sheet, password = password)
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    rd_inform(c("v" = "The file has been successfully created in {.file {path}}, with its sheet protected from edits by a password."))
  } else {
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    rd_inform(c("v" = "The file has been successfully created in {.file {path}}."))
  }
}
