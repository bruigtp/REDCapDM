% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rd_insert_na.R
\name{rd_insert_na}
\alias{rd_insert_na}
\title{Insert missing using a filter}
\usage{
rd_insert_na(..., data = NULL, dic = NULL, event_form = NULL, vars, filter)
}
\arguments{
\item{...}{List containing the data and the dictionary and the event if it's needed. Can be the output of the function `redcap_data`.}

\item{data}{Data frame containing data from REDCap. If the list is specified this argument is not needed.}

\item{dic}{Data frame  containing the dictionary read from REDCap. If the list is specified this argument is not needed.}

\item{event_form}{Data frame  containing the correspondence of each event with each form. If the list is specified this argument is not needed.}

\item{vars}{Character vector containing the names of those variables to transform.}

\item{filter}{Character vector containing the logic to be directly evaluated. When each logic is TRUE the corresponding variable specified in `vars` will be put to missing.}
}
\value{
transformed data with the specified variables converted.
}
\description{
Function that allows you to manually input a missing to some variables (`vars`) when some filters (`filter`) are satisfied. Useful for checkboxes without a gatekeeper question in the branching logic. Take in account that the variable will be transformed only in the events where both the variable and the filter evaluation are present, so they need to have at least one event in common.
}
\examples{
table(is.na(covican$data$potassium))
data <- rd_insert_na(covican,
             vars = "potassium",
             filter = "age < 65")
table(data$potassium)
}
