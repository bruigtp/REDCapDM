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
#' - `redcap_data`: Reads data exported from REDCap or retrieved through the REDCap API into R.
#' - `rd_transform`: One-step pipeline to clean and preprocess the raw REDCap data.
#'    - `rd_dates`: Standardize date and datetime fields.
#'    - `rd_delete_vars`: Remove specified variables (by name or pattern).
#'    - `rd_recalculate`: Recompute calculated fields and compare with REDCap values.
#'    - `rd_factor`: Replace numeric multiple-choice columns with their factor version.
#'    - `rd_checkbox`: Expand checkbox responses with custom labels and rename 'var___1' columns (REDCap style) to 'var_option'.
#'    - `rd_split`: Splits a REDCap dataset by form or event.
#'    - `rd_insert_na`: Manually set specified variables to missing based on a logical filter.
#'    - `rd_rlogic`: Translate REDCap branching or calculation logic into R syntax.
#'    - `rd_dictionary`: Update dictionary (translation of REDCap logic into R syntax) to reflect transformed data and logic.
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
