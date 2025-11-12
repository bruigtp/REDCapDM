#' @name REDCapDM-package
#' @keywords internal
#' @aliases REDCapDM
#'
#' @title Managing REDCap Data: The R package REDCapDM / REDCapDM: A Toolkit for Managing REDCap Data in R
#'
#' @description
#' The **REDCapDM** package provides tools to import, process, and manage REDCap data within R.
#' It supports data retrieval through the REDCap API or directly from exported files and includes a robust
#' set of functions for data transformation, validation, and discrepancy management. Designed for efficient
#' workflow integration, **REDCapDM** simplifies the handling of REDCap datasets, making it easier to ensure
#' data quality and consistency.
#'
#' Key Features:
#'
#' - **Flexible Data Import**: Import data directly from REDCap using API connections or process exported REDCap files.
#' - **Data Transformation**: Streamline the cleaning and preparation of raw datasets for analysis.
#' - **Query Management**: Identify and track data discrepancies, missing events, and manage resolution reports.
#'
#' Core Functions:
#'
#' - `redcap_data`: Reads data exported from REDCap or retrieved through the REDCap API.
#' - `rd_transform`: Processes raw REDCap datasets into a structured and analyzable format.
#'    - `transform_dates`: Transform dates and datetimes variables.
#'    - `recalculate`: Recalculates REDCap calculated fields, compares them to originals, and reports discrepancies.
#'    - `to_factor`: Converts variables to factors and updates the dictionary's branching logic.
#'    - `rd_delete_vars`: Deletes specified or pattern-matched variables from the data and dictionary.
#'    - `transform_checkbox`: Transforms the names of REDCap checkbox variables and updates the branching logic in the dictionary.
#'    - `transform_dic`: Evaluates and transforms branching logic in the REDCap dictionary into R logic.
#'    - `rd_split`: Splits a REDCap dataset by form or event.
#' - `rd_rlogic`: Converts REDCap branching and conditional logic into R-compatible expressions.
#' - `rd_insert_na`: Inserts missing values into specified variables based on filters.
#' - `rd_query`: Identifies discrepancies (queries) in the dataset for validation.
#' - `rd_event`: Detects missing events in longitudinal datasets.
#' - `check_queries`: Compares historical and current query reports to track changes and additions.
#' - `rd_export`: Exports a summary report of identified queries to an Excel (.xlsx) file.
#'
#'
#' @examples
#' \dontrun{
#' # Install REDCapDM from CRAN:
#' install.packages("REDCapDM")
#'
#' # Install the latest version of REDCapDM from GitHub:
#' remotes::install_github("bruigtp/REDCapDM")
#' }
#'
#' @importFrom lifecycle deprecated
NULL
