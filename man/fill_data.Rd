% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-transform.R
\name{fill_data}
\alias{fill_data}
\title{Fill Rows with Values from One Event}
\usage{
fill_data(which_event, which_var, data)
}
\arguments{
\item{which_event}{String specifying the name of the event.}

\item{which_var}{String specifying the name of the variable.}

\item{data}{Dataset containing the REDCap data.}
}
\description{
This function fills all rows in the dataset with the value of a particular variable in a specified event. It is an auxiliary function used in the `rd_rlogic` function.
}
