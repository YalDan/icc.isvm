% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_ICC_data.R
\name{make_ICC_data}
\alias{make_ICC_data}
\title{Make ICC data}
\usage{
make_ICC_data(
  DATA,
  N_complete,
  value_var = "mark_iv",
  verbose = FALSE,
  impute = FALSE,
  threshold = 0.66
)
}
\arguments{
\item{DATA}{A data.table with structure as provided in the example.}

\item{N_complete}{Maximum possible number of observations during the observed timeframe. This value is used for determining which instruments are above the specified threshold.}

\item{value_var}{The variable for which we want to generate a clustering data set.}

\item{verbose}{Boolean value. Should the function be verbose?}

\item{impute}{Boolean value. Should missing observations be imputed? Defaults to TRUE.}

\item{threshold}{How many percent of observations are allowed to be missing for each individual instrument over the observed timeframe? E.g. if set to 0.9, we allow 10\% of observations to be missing. Defaults to 0.66.}
}
\value{
Output: a data.table that can be used for ICC.
}
\description{
Generates a dataset for ICC computation.
}
