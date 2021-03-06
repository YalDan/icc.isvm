% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_test_result_list.R
\name{get_test_result_list}
\alias{get_test_result_list}
\title{Get test result list}
\usage{
get_test_result_list(
  DATA,
  K = 2,
  gamma = 0,
  w_max = 3,
  money_range,
  impute,
  subset_maturities,
  save_plot = F,
  data_only = F,
  parallel = T,
  n_cores = detectCores() * 0.5
)
}
\arguments{
\item{DATA}{Output list generated from "make_subset" function.}

\item{K}{Integer value. Number of clusters. Defaults to 2.}

\item{gamma}{Numeric value for the switching penalty. Defaults to 0.}

\item{w_max}{Integer value. The number of maximum iterations for ICC. Defaults to 3.}

\item{money_range}{Vector with lower and upper limit of moneyness values to consider. The natural boundary would be c(0,Inf).}

\item{impute}{Boolean value. Should missing observations be imputed?}

\item{subset_maturities}{Boolean value. Should only a subset of the closest maturities be considered?}

\item{save_plot}{Boolean. Should the plots be saved?}

\item{data_only}{Boolean. If set to TRUE, only the data is returned and plotting is skipped. Defaults to FALSE.}

\item{parallel}{Boolean. Should the results be computed in parallel? Won't work on Windows machines. Defaults to TRUE.}

\item{n_cores}{Number of cores. This only applies if parallel = TRUE. Defaults to 50\% of detected cores.}
}
\value{
DATA: The input data.

date: The date of the last observation.

symbols: List of all symbols that were used for clustering.

price_plot: Plot of the clustered price of the underlying index.

vola_plot: Plot of the clustered input data.

states: Time series with the states at each point in time.

mean_impl_vola: Table with mean and sd of the detected clusters.

w_max: Number of maximum iterations specified.

w: Number of actual iterations.

K: The input number of clusters .

scale_gamma: Indicates if the penalty gamma got scaled down.

gamma_init: Initial value provided for gamma.

gamma_value: Actual value for gamma. In case gamma hasn't been scaled down this will be identical to 'gamma_init'.
}
\description{
ICC clustering for multiple datasets. List wrapper for "get_test_result" function.
}
