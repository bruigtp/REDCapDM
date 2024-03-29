% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rd_event.R
\name{rd_event}
\alias{rd_event}
\title{Identification of missing event(s)}
\usage{
rd_event(
  ...,
  data = NULL,
  dic = NULL,
  event,
  filter = NA,
  query_name = NA,
  addTo = NA,
  report_title = NA,
  report_zeros = FALSE,
  link = list()
)
}
\arguments{
\item{...}{List containing the data, the dictionary and the event (if required). It may be the output of the `redcap_data` function.}

\item{data}{Data frame containing the data read from REDCap. If the list is given, this argument is not required.}

\item{dic}{Data frame containing the dictionary read from REDCap. If the list is given, this argument is not required.}

\item{event}{Character vector with the name of the REDCap event(s) to be analyzed.}

\item{filter}{A filter to be applied to the dataset. This argument can be used to identify the missing events on a subset of the dataset.}

\item{query_name}{Description of the query. It can be defined as the same one for all the variables, or you can define a different one for each variable. By default, the function defines it as `The event [event] is missing' for each event`.}

\item{addTo}{Data frame corresponding to a previous query data frame to which you can add the new query data frame. By default, the function always generates a new data frame without taking into account previous reports.}

\item{report_title}{Character string specifying the title of the report.}

\item{report_zeros}{Logical. If `TRUE`, the function returns a report containing variables with zero queries.}

\item{link}{List containing project information used to create a web link to each missing event.}
}
\value{
A list with a data frame of 9 columns (10 columns, if the link argument is specified) meant to help the user identify each missing event and a table with the total number of missing events per event analyzed.
}
\description{
When working with a longitudinal REDCap project, the exported data has a structure where each row represents one event per record. However, by default, REDCap does not export events for which there is no information available.
This function allows you to identify which record identifiers do not contain information about a particular event.
}
\examples{
example <- rd_event(covican,
                    event = "follow_up_visit_da_arm_1")
example
}
