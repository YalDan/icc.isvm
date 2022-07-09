#' Make plots
#'
#' Generate plot for the final experiment result
#'
#' @import data.table
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import scales
#'
#' @param final_list_DT_subset A list with the result of the ICC and ISVM experiments generated from function "fit_ICC_ISVM".
#' @param PRICE_PLOT Boolean value. Should the price of the underlying index be plotted?
#' @param ICC_PLOT Boolean value. Should the ICC result be plotted?
#' @param ISVM_PLOT Boolean value. Should the ISVM fit be plotted?
#' @param SAVE_PLOTS Boolean value. Should the plots be saved?
#' @param RM_CI Boolean value. Should the confidence interval be removed from the plot for the ISVM model fitted on the whole dataset?
#'
#' @return Returns a data.table with the price of the underlying index.

#' @export
make_plots <- function(final_list_DT_subset,
                       PRICE_PLOT = TRUE,
                       ICC_PLOT = TRUE,
                       ISVM_PLOT = TRUE,
                       SAVE_PLOTS = TRUE,
                       RM_CI = FALSE)
{
  make_plot_directory()
  plot_theme <- make_plot_theme()
  last_date <- final_list_DT_subset$ICC$date
  K <- final_list_DT_subset$ICC$K
  gamma <- final_list_DT_subset$ICC$gamma_value

  if (PRICE_PLOT) {
    price <- ggplot(final_list_DT_subset$ICC$price_plot$data, aes(x = t,y = close)) + geom_point() + # do a scatterplot, e.g. geom_line() would make it a lineplot
      labs(x = "Date", y = "BTC / USD", title = "") + # rename axis labels and title
      plot_theme + # add the customized plot theme that makes the background transparent and removes the gridlines
      coord_cartesian(xlim = c(min(final_list_DT_subset$ICC$price_plot$data[,t]) - 3600*6,max(final_list_DT_subset$ICC$price_plot$data[,t]) + 3600*6))

    vola <- ggplot(final_list_DT_subset$ICC$vola_plot$data, aes(x = t,y = value,color = variable,group = variable)) + geom_line() +
      labs(x = "Date", y = "Implied Volatility", title = "") + # rename axis labels and title
      coord_cartesian(xlim = c(min(final_list_DT_subset$ICC$vola_plot$data[,t]) - 3600*6,max(final_list_DT_subset$ICC$vola_plot$data[,t]) + 3600*6)) +
      plot_theme

    if (SAVE_PLOTS) {
      ggsave(plot = price, file = paste("./plots/data/", last_date, "_price.png", sep = ""), bg = "transparent",
             width = 11.69, height = 8.27, dpi = 300)
      ggsave(plot = vola, file = paste("./plots/data/", last_date, "_vola.png", sep = ""),  bg = "transparent",
             width = 11.69, height = 8.27, dpi = 300)
    }
  }

  if (ICC_PLOT) {
    print(final_list_DT_subset$ICC$price_plot)
    print(final_list_DT_subset$ICC$vola_plot)

    if (SAVE_PLOTS) {
      ggsave(plot = final_list_DT_subset$ICC$price_plot + labs(title = ""), file = paste("./plots/ICC/", last_date, "_K", K, "_gamma", gamma, "_price_cluster.png", sep = ""), bg = "transparent",
             width = 11.69, height = 8.27, dpi = 300)
      ggsave(plot = final_list_DT_subset$ICC$vola_plot + labs(title = ""), file = paste("./plots/ICC/", last_date, "_K", K, "_gamma", gamma, "_vola_cluster.png", sep = ""), bg = "transparent",
             width = 11.69, height = 8.27, dpi = 300)
    }
  }
  if (ISVM_PLOT) {
    if (RM_CI) {
      ISVM_all_states_no_CI <- lapply(final_list_DT_subset$ISVM[[1]], function(x) {
        x$layers[[4]] <- NULL
        x$layers[[3]] <- NULL
        return(x)
      })
      ISVM_all_states <- do.call("grid.arrange", c(ISVM_all_states_no_CI, nrow = 2))
      ggsave(plot = ISVM_all_states, file = paste("./plots/ISVM/", last_date, "_ISVM_all_states_no_ci.png", sep = ""), bg = "transparent",
             width = 11.69, height = 8.27, dpi = 300)
    } else {
      ISVM_all_states <- do.call("grid.arrange", c(final_list_DT_subset$ISVM[[1]], nrow = 2))
      ggsave(plot = ISVM_all_states, file = paste("./plots/ISVM/", last_date, "_ISVM_all_states.png", sep = ""), bg = "transparent",
             width = 11.69, height = 8.27, dpi = 300)
    }

    ## with original model

    gamma_list_clusters <- make_ISVM_grid_list(final_list_DT_subset, "gamma")
    eta2_list_clusters <- make_ISVM_grid_list(final_list_DT_subset, "eta2")
    eta_list_clusters <- make_ISVM_grid_list(final_list_DT_subset, "eta")
    mu_list_clusters <- make_ISVM_grid_list(final_list_DT_subset, "mu")
    rho_list_clusters <- make_ISVM_grid_list(final_list_DT_subset, "rho")

    gamma_plot <- do.call("grid.arrange", c(gamma_list_clusters, nrow = 1))
    eta2_plot <- do.call("grid.arrange", c(eta2_list_clusters, nrow = 1))
    eta_plot <- do.call("grid.arrange", c(eta_list_clusters, nrow = 1))
    mu_plot <- do.call("grid.arrange", c(mu_list_clusters, nrow = 1))
    rho_plot <- do.call("grid.arrange", c(rho_list_clusters, nrow = 1))

    if (SAVE_PLOTS) {
      ggsave(plot = gamma_plot, file = paste("./plots/ISVM/", last_date, "_K", K, "_ISVM_gamma_cluster_states_comp.png", sep = ""), bg = "transparent",
             width = 24, height = 24/5, dpi = 300)
      ggsave(plot = eta2_plot, file = paste("./plots/ISVM/", last_date, "_K", K, "_ISVM_eta2_cluster_states_comp.png", sep = ""), bg = "transparent",
             width = 12, height = 12/5, dpi = 300)
      ggsave(plot = eta_plot, file = paste("./plots/ISVM/", last_date, "_K", K, "_ISVM_eta_cluster_states_comp.png", sep = ""), bg = "transparent",
             width = 12, height = 12/5, dpi = 300)
      ggsave(plot = mu_plot, file = paste("./plots/ISVM/", last_date, "_K", K, "_ISVM_mu_cluster_states_comp.png", sep = ""), bg = "transparent",
             width = 12, height = 12/5, dpi = 300)
      ggsave(plot = rho_plot, file = paste("./plots/ISVM/", last_date, "_K", K, "_ISVM_rho_cluster_states_comp.png", sep = ""), bg = "transparent",
             width = 12, height = 12/5, dpi = 300)
    }
    ##
  }
}
