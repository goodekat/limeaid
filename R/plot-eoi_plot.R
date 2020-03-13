#' Visualize the explanation of interest (eoi) and LIME simulated data
#' 
#' @param explanation Rows from the LIME explanations associated 
#'        with one prediction of interest
#' @param alpha Value to use for alpha blending of the points
#'        
#' @importFrom dplyr mutate_at slice
#' @export eoi_plot
#' 
#' @examples 
#' # Create Random Forest model on the sine data
#' rfsine <- caret::train(x = sine_data_train[c("x1", "x2", "x3")],
#'  y = sine_data_train$y,
#'  method = "rf")
#'                        
#' # Apply lime such that the simulated values are returned
#' sine_lime_explain <-
#' apply_lime(train = sine_data_train[c("x1", "x2", "x3")],
#'  test = sine_data_test[c("x1", "x2", "x3")],
#'  model = rfsine,
#'  label = "1",
#'  n_features = 2,
#'  sim_method = c('quantile_bins', 'kernel_density'),
#'  nbins = c(3, 4),
#'  return_perms = TRUE,
#'  seed = 20190914)
#'  
#' # Extract the rows associtaed with the explanation 
#' # of interest (the first observation in the test data) 
#' eoi <- sine_lime_explain$explain[1:2,]
#' 
#' # Plot the simulated data
#' eoi_plot(eoi)

eoi_plot <- function(explanation, alpha = 1) {
  
  # Extract the label associated with the explanation
  eoi_label = explanation$label[1]
  
  # Extract the predictions from the complex model associated
  # with the simulated data values from 
  complex_pred = explanation %>%
    slice(1) %>%
    pull(perms_pred_complex) %>%
    as.data.frame()
  
  # If label was a number, then extract the column differently
  if (sum(names(complex_pred) == eoi_label) == 0) {
    detected = stringr::str_detect(names(complex_pred), eoi_label)
    eoi_label = names(complex_pred)[detected]
  }
  
  # Extract the features selected by LIME
  eoi_features = explanation$feature
  
  # Create a dataset with the simulated features and their 
  # predictions from the complex model
  sim_data <- explanation %>% 
    slice(1) %>%
    pull(perms_raw) %>%
    as.data.frame() %>%
    select(eoi_features) %>%
    mutate(complex_pred = complex_pred %>% pull(all_of(eoi_label)),
           obs = 1:n())
  
  # Create pairs of features to use in plot
  feature_pairs <- t(combn(eoi_features, 2))
  
  # Create data for plotting
  sim_data_plot <- purrr::map_df(
    .x = 1:nrow(feature_pairs),
    .f = function(row) {
      data.frame(f1 = as.character(feature_pairs[row,1]),
                 f2 = as.character(feature_pairs[row,2]),
                 v1 = sim_data %>% pull(feature_pairs[row,1]),
                 v2 = sim_data %>% pull(feature_pairs[row,2]),
                 complex_pred = sim_data$complex_pred) %>%
        mutate_at(.vars = c("f1", "f2"), .funs = as.character)
    })
  
  # Create a dataframe with the prediction of interest's 
  # observed feature values
  poi_data <- data.frame(feature = explanation$feature,
                         value = explanation$feature_value) %>%
    pivot_wider(names_from = "feature", values_from = "value")
  
  # Create data for plotting
  poi_data_plot <- purrr::map_df(
    .x = 1:nrow(feature_pairs),
    .f = function(row) {
      data.frame(f1 = as.character(feature_pairs[row,1]),
                 f2 = as.character(feature_pairs[row,2]),
                 v1 = poi_data %>% pull(feature_pairs[row,1]),
                 v2 = poi_data %>% pull(feature_pairs[row,2]),
                 complex_pred = explanation$label_prob[1]) %>%
        mutate_at(.vars = c("f1", "f2"), .funs = as.character)
    })
  
  # Create the plot
  ggplot() + 
    geom_point(data = sim_data_plot,
               mapping =  aes(x = v2, y = v1, color = complex_pred),
               alpha = alpha) + 
    geom_point(data = poi_data_plot %>% 
                 mutate(shape = "Prediction \nof Interest"), 
               mapping = aes(x = v1, 
                             y = v2, 
                             fill = complex_pred, 
                             shape = shape),
               color = "black",
               size = 5,
               alpha = 0.8) +
    facet_grid(f1 ~ f2, scales = "free", switch = "both") + 
    scale_color_gradient2(low = "firebrick", 
                          high = "steelblue", 
                          midpoint = 0.5, 
                          limits = c(0, 1)) + 
    scale_shape_manual(values = 23) +
    theme(strip.placement = "outside",
          strip.background = element_rect(color = "white", 
                                          fill = "white")) + 
    labs(x = "", 
         y = "", 
         color = "Complex \nModel \nPrediction",
         fill = "Complex \nModel \nPrediction", 
         shape = "") + 
    guides(shape = guide_legend(order = 1),
           fill = FALSE)

}
