#' Make directories
#'
#' Creates directories for saving plots and experiment results with parameters for reproducibility
#'
#' @param N_subs_date Number of dates the experiment data was subsetted from.
#' @param K Integer value. Number of clusters in the experiment.
#' @param gamma Numeric value for the switching penalty in the experiment.
#' @param impute Boolean value. Were missing observations imputedin the experiment?
#' @param money_range Vector with lower and upper limit of moneyness values that were considered in the experiment.
#' @param subset_maturities Boolean value. Was only a subset of the closest maturities considered in the experiment?
#'
#' @return dir_name_vola: The name of the directory for the ICC plots.
#' @return dir_name_price:  The name of the directory for the price plots.

#' @export
make_experiment_directory <- function(N_subs_date, K, gamma, impute, money_range, subset_maturities) {
  dir_name <- paste("./plots/",N_subs_date, "d_",
                    K, "K_",
                    gamma, "gamma_",
                    "impute", impute, "_",
                    money_range[1], "_", money_range[2],  "moneyrange_",
                    "subsmat", subset_maturities,
                    sep = "")
  dir_name_vola <- paste(dir_name, "/volatility/", sep = "")
  dir_name_price <- paste(dir_name, "/price/", sep = "")

  if (!dir.exists(dir_name)) {
    print(paste("Created directory", dir_name))
    dir.create(dir_name, recursive = TRUE)
  }

  if (!dir.exists(dir_name_vola)) {
    print(paste("Created directory", dir_name_vola))
    dir.create(dir_name_vola, recursive = TRUE)
  }

  if (!dir.exists(dir_name_price)) {
    print(paste("Created directory", dir_name_price))
    dir.create(dir_name_price, recursive = TRUE)
  }

  return(list("dir_name_vola" = dir_name_vola, "dir_name_price" = dir_name_price))
}

#' @export
make_plot_directory <- function() {

  dir_name_ICC <- "./plots/ICC/"
  dir_name_ISVM <- "./plots/ISVM/"
  dir_name_data <- "./plots/data/"

  if (!dir.exists("./plots")) {
    print("Created directory './plots'")
    dir.create("./plots")
  }

  if (!dir.exists(dir_name_ICC)) {
    print(paste("Created directory", dir_name_ICC))
    dir.create(dir_name_ICC, recursive = TRUE)
  }

  if (!dir.exists(dir_name_ISVM)) {
    print(paste("Created directory", dir_name_ISVM))
    dir.create(dir_name_ISVM, recursive = TRUE)
  }

  if (!dir.exists(dir_name_data)) {
    print(paste("Created directory", dir_name_data))
    dir.create(dir_name_data, recursive = TRUE)
  }
}
