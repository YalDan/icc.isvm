% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_and_save_test_data.R
\name{make_and_save_test_data}
\alias{make_and_save_test_data}
\title{Make and save test data}
\usage{
make_and_save_test_data(
  PATH,
  filename,
  convert_from_epoch_date = TRUE,
  scale_iv = FALSE
)
}
\arguments{
\item{PATH}{An input data path.}

\item{filename}{The name of the file that will be saved.}

\item{convert_from_epoch_date}{Should the data be converted to POSIXct from Epoch/UNIX timestamp? Defaults to TRUE.}

\item{scale_iv}{Should IV be scaled down? Do this when IV values are e.g. 80 instead of 0.8. Defaults to FALSE.}
}
\value{
Outputs pre-processed data.table.
}
\description{
Wrapper for load_and_preprocess function that also saves the data on the local machine.
}
