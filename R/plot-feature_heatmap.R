#' Heatmap of LIME Selected Features
#'
#' Creates a heatmap of the features selected by lime for each
#' of the tunning parameters
#'
#' @param explanations explain data frame from the apply_lime list
#' @param feature_nums accepts a vector of integer values from 1 to \code{nfeatures} specified in \code{apply_lime} to specify which features selected by LIME should be included in the plot
#'
#' @importFrom checkmate expect_data_frame
#' @importFrom ggplot2 aes facet_grid geom_tile ggplot labs theme theme_bw
#'
#' @export feature_heatmap
#'

feature_heatmap <- function(explanations, feature_nums = NULL){

  # Checks
  checkmate::expect_data_frame(explanations)

  # Organize the explanation data for plotting
  heatmap_data <- explanations %>%
    select(sim_method, nbins, case, feature, feature_weight) %>%
    mutate(feature_magnitude = abs(feature_weight)) %>%
    group_by(sim_method, nbins, case) %>%
    arrange(sim_method, nbins, case, desc(feature_magnitude)) %>%
    mutate(feature_num = 1:n()) %>%
    ungroup() %>%
    mutate(nbins = factor(nbins),
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
    facet_grid(feature_num ~ sim_method_plot, scales = "free", space = "free") +
    theme_bw() +
    labs(x = "Number of Bins",
         y = "Prediction Number",
         fill = "Feature")

}

