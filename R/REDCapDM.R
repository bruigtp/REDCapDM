#' REDCapDM: A package to create query reports on a given REDCap dataset.
#'
#' @description
#' The REDCapDM package provides three main functions that allow us to read a dataset exported from REDCap, to perform a pre-processing of the data, to identify a set of queries and to check, over time, which of the old queries have been resolved. This package has been built and tested on databases created with REDCap v12.4.17.
#'
#' @details
#' REDCapDM functions:
#'
#' - redcap_data: used to read data exported from REDCap or through an API connection.
#'
#' - rd_transform: pre-processing a dataset.
#'
#' - rd_rlogic: transcribes redcap logic to R logic.
#'
#' - rd_insert_na: manually input a missing value for specified variables using a filter.
#'
#' - rd_query: identification of queries.
#'
#' - rd_event: identification of missing events per record identifier.
#'
#' - check_queries: used to compare current queries with an old report to determine which ones have been modified, which remain unchanged, and if there are any new queries.
#'
#' - rd_export: used to export all identified queries into a xlsx file.
#'
#' @docType package
#' @name REDCapDM
NULL
