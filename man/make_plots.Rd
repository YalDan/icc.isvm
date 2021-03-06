% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_plots.R
\name{make_plots}
\alias{make_plots}
\title{Make plots}
\usage{
make_plots(
  final_list_DT_subset,
  PRICE_PLOT = TRUE,
  ICC_PLOT = TRUE,
  ISVM_PLOT = TRUE,
  SAVE_PLOTS = TRUE,
  RM_CI = FALSE
)
}
\arguments{
\item{final_list_DT_subset}{A list with the result of the ICC and ISVM experiments generated from function "fit_ICC_ISVM".}

\item{PRICE_PLOT}{Boolean value. Should the price of the underlying index be plotted?}

\item{ICC_PLOT}{Boolean value. Should the ICC result be plotted?}

\item{ISVM_PLOT}{Boolean value. Should the ISVM fit be plotted?}

\item{SAVE_PLOTS}{Boolean value. Should the plots be saved?}

\item{RM_CI}{Boolean value. Should the confidence interval be removed from the plot for the ISVM model fitted on the whole dataset?}
}
\value{
Returns a data.table with the price of the underlying index.
}
\description{
Generate plot for the final experiment result
}
