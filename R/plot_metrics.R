#' Plot of LIME Comparison Metrics
#'
#' Plots the specified comparison metrics versus LIME tunning parameters.
#'
#' @param explanations Explain dataframe from the list returned by apply_lime.
#' @param metrics Vector specifying metrics to compute. Default is 'all'. See details for metrics available.
#' @param add_lines Draw lines between tuning parameters with the same gower power.
#' @param rank_legend Specifies whether the legend for rank is treated as 'continuous' or 'discrete'.
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
#'   "Why should I trust you?": Explaining the predictions of any classifier.
#'   Proceedings of the 22nd ACM SIGKDD International Conference on
#'   Knowledge Discovery and Data Mining, San Francisco, CA, USA, August
#'   13-17, 2016, 1135–1144.
#'
#' @importFrom ggplot2 element_blank scale_color_gradient scale_colour_grey geom_line
#' @importFrom tidyr gather
#' @importFrom scales seq_gradient_pal
#'
#' @export plot_metrics
#'
#' @examples
#'
#' # Prepare training and testing data
#' x_train = sine_data_train[c("x1", "x2", "x3")]
#' y_train = factor(sine_data_train$y)
#' x_test = sine_data_test[1:5, c("x1", "x2", "x3")]
#' 
#' # Fit a random forest model
#' rf <- randomForest::randomForest(x = x_train, y = y_train) 
#' 
#' # Run apply_lime
#' res <- apply_lime(train = x_train, 
#'                   test = x_test, 
#'                   model = rf,
#'                   label = "1",
#'                   n_features = 2,
#'                   sim_method = 'quantile_bins',
#'                   nbins = 2:3, 
#'                   gower_pow = c(1, 5))
#'
#' # Plot metrics to compare LIME implementations
#' plot_metrics(res$explain)
#'
#' # Return a plot with only the MSEE values
#' plot_metrics(res$explain, metrics = "msee")


plot_metrics <- function(explanations, metrics = 'all', add_lines = FALSE, rank_legend = 'continuous'){

  # Checks
  checkmate::expect_data_frame(explanations)
  checkmate::expect_character(metrics)
  if (!all(metrics %in% c("all", "ave_r2", "msee", "ave_fidelity"))) {
    stop("metrics specified incorrectly. Must be a character vector with options of 'ave_r2', 'msee', 'ave_fidelity'.")
  } 
  
  # If metrics is not specified
  if ("all" %in% metrics) metrics = c("ave_r2", "msee", "ave_fidelity")

  # Obtain the comparison metrics
  metric_data <- compute_metrics(explanations, metrics)

  # Prepare the data for the plot
  plot_data <- metric_data %>%
    tidyr::pivot_longer(names_to = "metric", 
                        values_to = "value", 
                        metrics) %>%
    filter(.data$metric %in% metrics) %>%
    mutate(metric = factor(.data$metric),
           nbins = factor(.data$nbins),
           metric = ifelse(.data$metric == "ave_r2", "Average R2",
                    ifelse(.data$metric == "msee", "MSEE",
                           "Average Fidelity")),
           sim_method =
             ifelse(.data$sim_method == "quantile_bins", "Quantile Bins",
                    ifelse(.data$sim_method == "equal_bins", "Equal Bins",
                           ifelse(.data$sim_method == "kernel_density", "Kernel",
                                  "Normal"))) %>% factor(),
           sim_method_plot = factor(ifelse(.data$sim_method %in% c("Kernel", "Normal"),
                                           "Density",
                                           as.character(.data$sim_method))),
           nbins_plot = factor(ifelse(is.na(.data$nbins),
                                      as.character(.data$sim_method),
                                      as.character(.data$nbins)))) %>%
    mutate(metric = factor(.data$metric, levels = c("Average R2", "Average Fidelity", "MSEE"))) %>%
    mutate(ranking_value = ifelse(.data$metric == "Average R2", -.data$value, .data$value)) %>%
    group_by(.data$metric) %>%
    mutate(rank = rank(.data$ranking_value),
           gower_pow = factor(.data$gower_pow)) %>%
    arrange(.data$metric, .data$value)

  # Create the comparison plot based on whether 1 or more gower power 
  # was specified
  if (length(unique(plot_data$gower_pow)) == 1) {
    plot <- ggplot(plot_data, 
                   aes(x = .data$nbins_plot, y = .data$value))
  } else {
    plot <- ggplot(plot_data, 
                   aes(x = .data$nbins_plot, y = .data$value, shape = .data$gower_pow)) + 
      labs(shape = "Gower \nPower")
  }
  
  # Add lines to the plot if requested
  if (add_lines == TRUE) {
    plot <- plot + geom_line(aes(group = factor(.data$gower_pow)), size = 0.1)
  }
  
  # Add points and color based on the number of ranks
  if (rank_legend == "continuous") {
    plot <- plot + geom_point(aes(color = .data$rank)) + 
      scale_color_gradient(low = "black", high = "grey85")
  } else if (rank_legend == "discrete") {
    plot <- plot + geom_point(aes(color = factor(.data$rank))) + 
      scale_colour_grey()
  } else {
    stop("'rank_legend' specified incorrectly. Must by 'continuous' or 'discrete'.")
  }
  
  # Add the additional layers to the plot
  plot +
    facet_grid(.data$metric ~ .data$sim_method_plot, 
               scales = "free", space = "free_x", switch = "y") +
    theme_grey() +
    theme(strip.placement = "outside", 
          strip.background = element_blank()) +
    labs(x = "Number of Bins",
         y = "",
         color = "Rank \n(within \na metric)")

}

