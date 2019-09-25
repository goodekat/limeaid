#' Plot of LIME Comparison Metrics
#'
#' Plots the specified comparison metrics by LIME
#' tunning parameters
#'
#' @param explanations explain dataframe from apply_lime
#' @param metrics
#'
#' @importFrom forcats fct_recode
#' @importFrom tidyr gather
#'
#' @export plot_compare

plot_compare <- function(explanations, metrics = NULL){

  # If metrics is not specified
  if (is.null(metrics)) metrics = c("ave_r2", "msee")

  # Obtain the comparison metrics
  metric_data <- compare_limes(explanations)

  # Prepare the data for the plot
  plot_data <- metric_data %>%
    tidyr::gather(key = metric, value = value, ave_r2:msee) %>%
    filter(metric %in% metrics) %>%
    mutate(metric = factor(metric),
           nbins = factor(nbins),
           metric = forcats::fct_recode(
             metric,
             "Average R2" = "ave_r2",
             "MSEE" = "msee"),
           sim_method = forcats::fct_recode(
             sim_method,
             "Quantile Bins" = "quantile_bins",
             "Equal Bins" = "equal_bins",
             "Kernel" = "kernel_density",
             "Normal" = "normal_approx"),
           sim_method_plot = factor(ifelse(sim_method %in% c("Kernel", "Normal"),
                                           "Density",
                                           as.character(sim_method))),
           nbins_plot = factor(ifelse(is.na(nbins),
                                      as.character(sim_method),
                                      as.character(nbins))))

  # Create the comparison plot
  ggplot(plot_data, aes(x = nbins_plot, y = value, color = nbins_plot)) +
    geom_point() +
    facet_grid(metric ~ sim_method_plot, scales = "free", space = "free_x") +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "Number of Bins",
         y = "Metric Value",
         color = "")

}

