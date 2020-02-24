#' Heatmap of LIME Selected Features
#'
#' Creates a heatmap of the features selected by lime for all
#' observations in the test set across all of the different
#' LIME implementations.
#'
#' @param explanations Explain dataframe from the list returned by apply_lime.
#' @param feature_nums A vector of integer values from 1 to \code{nfeatures} (specified in \code{apply_lime}) to determine which features selected by LIME should be included in the plot.
#'
#' @importFrom checkmate expect_data_frame expect_character
#' @importFrom ggplot2 aes facet_grid geom_point geom_tile ggplot labs scale_color_manual theme theme_bw
#'
#' @export feature_heatmap
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
#' # Plot heatmap of selected features across LIME implementations
#' feature_heatmap(sine_lime_explain$explain)
#'
#' # Return a heatmap with only the features selected first by LIME
#' feature_heatmap(sine_lime_explain$explain, feature_num = 1)

feature_heatmap <- function(explanations, feature_nums = NULL){

  # Checks
  checkmate::expect_data_frame(explanations)
  if (!is.null(feature_nums)) checkmate::expect_double(feature_nums)

  # Organize the explanation data for plotting
  heatmap_data <- explanations %>%
    select(sim_method, nbins, gower_pow, case, feature, feature_weight) %>%
    mutate(feature_magnitude = abs(feature_weight)) %>%
    group_by(sim_method, nbins, gower_pow, case) %>%
    arrange(sim_method, nbins, gower_pow, case, desc(feature_magnitude)) %>%
    mutate(feature_num = 1:n()) %>%
    ungroup() %>%
    mutate(nbins = factor(nbins),
           gower_pow = factor(paste("Gower Power of", gower_pow)),
           case = factor(case),
           feature = factor(feature),
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

  # Subset the data to only keep the requested features
  if (!(is.null(feature_nums))) {
    heatmap_data <- heatmap_data %>%
      filter(feature_num %in% feature_nums) %>%
      mutate(feature_num = factor(feature_num),
             feature_num = paste("Feature", feature_num))
  } else {
    heatmap_data <- heatmap_data %>%
      mutate(feature_num = factor(feature_num),
             feature_num = paste("Feature", feature_num))
  }

  # Create the heatmap
  ggplot(heatmap_data, aes(x = nbins_plot, y = case, fill = feature)) +
    geom_tile() +
    facet_grid(feature_num ~ sim_method_plot + gower_pow, scales = "free", space = "free") +
    theme_bw() +
    labs(x = "Number of Bins",
         y = "Prediction Number",
         fill = "Feature")

}

