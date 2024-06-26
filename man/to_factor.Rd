% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-transform.R
\name{to_factor}
\alias{to_factor}
\title{Convert Variables to Factors}
\usage{
to_factor(data, dic, exclude = NULL)
}
\arguments{
\item{data}{Data frame containing the REDCap data.}

\item{dic}{Data frame containing the REDCap dictionary.}

\item{exclude}{Character vector specifying the names of variables that should not be converted to factors. If `NULL`, all variables will be converted.}
}
\description{
This function converts all variables in the dataset to factors, except those specified in the `exclude` parameter.
}
