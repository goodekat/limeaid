#' @importFrom checkmate expect_data_frame
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
           case = as.numeric(case),
           feature = factor(feature))

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
  ggplot(heatmap_data, aes(x = nbins, y = case, fill = feature)) +
    geom_tile() +
    facet_grid(feature_num ~ sim_method, scales = "free", space = "free") +
    theme_bw() +
    labs(x = "Number of Bins",
         y = "Prediction Number",
         fill = "Feature")

}

