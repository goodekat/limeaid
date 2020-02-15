#' Plot of LIME Comparison Metrics
#'
#' Plots the specified comparison metrics versus LIME tunning parameters.
#'
#' @param explanations Explain dataframe from the list returned by apply_lime.
#' @param metrics Vector specifying metrics to compute. Default is 'all'. See details for metrics available.
#'
#' @details The metrics available are listed below.
#'
#' \itemize{
#'   \item \code{ave_r2}: Average explainer model R2 value computed over all explanations in the test set.
#'   \item \code{msee}: Mean square explanation error computed over all explanations in the test set.
#'   \item \code{ave_fidelity}: Average fidelity metric (Ribeiro et. al. 2016) computed over all explanations in the test set.
#' }
#'
#' @references Ribeiro, M. T., S. Singh, and C. Guestrin, 2016:
#'   "why should I trust you?": Explaining the predictions of any classifier.
#'   Proceedings of the 22nd ACM SIGKDD Inter- national Conference on
#'   Knowledge Discovery and Data Mining, San Francisco, CA, USA, August
#'   13-17, 2016, 1135–1144.
#'
#' @importFrom tidyr gather
#'
#' @export metric_plot
#'
#' @examples
#'
#' # Create Random Forest model on the sine data
#' rfsine <- caret::train(x = sine_data_train[c("x1", "x2", "x3")],
#'                        y = sine_data_train$y,
#'                        method = "rf")
#'
#' # Apply lime several implementations of LIME
#' sine_lime_explain <-
#'    apply_lime(train = sine_data_train[c("x1", "x2", "x3")],
#'               test = sine_data_test[c("x1", "x2", "x3")],
#'               model = rfsine,
#'               label = "1",
#'               n_features = 2,
#'               sim_method = c('quantile_bins', 'kernel_density'),
#'               nbins = c(3, 4),
#'               seed = 20190914)
#'
#' # Plot metrics to compare LIME implementations
#' metric_plot(sine_lime_explain$explain)
#'
#' # Return a plot with only the MSEE values
#' metric_plot(sine_lime_explain$explain, metrics = "msee")

metric_plot <- function(explanations, metrics = 'all'){

  # Checks
  checkmate::expect_data_frame(explanations)
  checkmate::expect_character(metrics)

  # If metrics is not specified
  if ("all" %in% metrics) metrics = c("ave_r2", "msee", "ave_fidelity")

  # Obtain the comparison metrics
  metric_data <- compute_metrics(explanations)

  # Prepare the data for the plot
  plot_data <- metric_data %>%
    tidyr::pivot_longer(names_to = "metric", values_to = "value", ave_r2:ave_fidelity) %>%
    filter(metric %in% metrics) %>%
    mutate(metric = factor(metric),
           nbins = factor(nbins),
           metric = ifelse(metric == "ave_r2", "Average R2",
                    ifelse(metric == "msee", "MSEE",
                           "Average Fidelity")),
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
                                      as.character(nbins)))) %>%
    mutate(ranking_value = ifelse(metric == "Average R2", -value, value)) %>%
    group_by(metric) %>%
    mutate(rank = factor(rank(ranking_value))) %>%
    arrange(metric, value)

  # Create color palette
  colors <- scales::seq_gradient_pal("blue", "lightblue", "Lab")(seq(0, 1 , length.out = length(unique(plot_data$rank))))

  # Create the comparison plot
  ggplot(plot_data, aes(x = nbins_plot, y = value, color = rank)) +
    geom_point() +
    facet_grid(metric ~ sim_method_plot, scales = "free", space = "free_x") +
    theme_bw() +
    labs(x = "Number of Bins",
         y = "Metric Value",
         color = "Rank") +
    scale_colour_manual(values = colors)

}

