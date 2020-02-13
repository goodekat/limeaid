#' Plot of LIME Comparison Metrics
#'
#' Plots the specified comparison metrics by LIME
#' tunning parameters
#'
#' @param explanations explain dataframe from apply_lime
#' @param metrics
#'
#' @importFrom tidyr gather
#'
#' @export compare_metrics

compare_metrics <- function(explanations, metrics = NULL){

  # If metrics is not specified
  if (is.null(metrics)) metrics = c("ave_r2", "msee")

  # Obtain the comparison metrics
  metric_data <- compare_limes(explanations)

  # Prepare the data for the plot
  plot_data <- metric_data %>%
    tidyr::pivot_longer(names_to = "metric", values_to = "value", ave_r2:msee) %>%
    filter(metric %in% metrics) %>%
    mutate(metric = factor(metric),
           nbins = factor(nbins),
           metric = ifelse(metric == "ave_r2", "Average R2", "MSEE"),
           sim_method =
             ifelse(sim_method == "quantile_bins", "Quantile Bins",
                    ifelse(sim_method == "equal_bins", "Equal Bins",
                           ifelse(sim_method == "kernel_density", "Kernel",
                                  "Normal"))) %>% factor(),
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

