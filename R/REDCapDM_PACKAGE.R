#' @name REDCapDM-package
#' @keywords internal
#' @aliases REDCapDM
#'
#' @title Managing REDCap Data: The R package REDCapDM
#'
#' @description
#'
#' The REDCapDM package facilitates the importing of data from REDCap into R through either an API connection or directly from exported files. It includes a range of data processing and transformation functions, and supports the creation and management of queries to address any discrepancies or uncertainties within the dataset.
#'
#' REDCapDM functions:
#'
#' - `redcap_data`: reads data exported directly from REDCap or via an API connection.
#'
#' - `rd_transform`: processes the raw dataset.
#'
#' - `rd_rlogic`: translates REDCap logic into R logic.
#'
#' - `rd_insert_na`: manually inserts a missing value for specified variables using a filter.
#'
#' - `rd_query`: identifies discrepancies in the dataset (queries).
#'
#' - `rd_event`: identifies missing events in each record of the dataset.
#'
#' - `check_queries`: compares a current report of queries with an older one to determine which queries have been modified, which remain unchanged, and which are new.
#'
#' - `rd_export`: exports a report of identified queries to an .xlsx file.
#'
#' @examples
#' \dontrun{
#' # Install REDCapDM from CRAN:
#' install.packages('REDCapR')
#'
#' # Install REDCapDM from GitHub:
#' remotes::install_github('bruigtp/REDCapDM')
#' }
NULL
