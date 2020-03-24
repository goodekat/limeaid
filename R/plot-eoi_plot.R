#' Visualize the explanation of interest (eoi) and LIME simulated data
#' 
#' @param explanation Rows from the LIME explanations associated 
#'        with one prediction of interest
#' @param bins Should lines indicating the bins used by LIME be included? 
#'        Only applicable is sim_method is equal to "quantile_bins" or 
#'        "equal_bins". (Default is TRUE.)
#' @param weights Should the size of the points represent the weight 
#'        assigned by LIME? (Default is TRUE.)
#' @param alpha Value to use for alpha blending of the points
#' @param title.opt Should a title be included that lists the 
#'        simulation method and Gower exponent? (Default is TRUE.)
#'        
#' @importFrom dplyr mutate_at slice
#' @importFrom ggplot2 element_rect geom_hline geom_vline guides guide_legend scale_color_gradient2 scale_fill_gradient2 scale_shape_manual scale_size theme_grey
#' @importFrom utils combn
#' @export eoi_plot
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
#' \dontrun{
#' res <- apply_lime(train = x_train, 
#'                   test = x_test, 
#'                   model = rf,
#'                   label = "1",
#'                   n_features = 2,
#'                   sim_method = c('quantile_bins',
#'                                  'kernel_density'),
#'                   nbins = 2:3, 
#'                   return_perms = TRUE)
#'  
#' # Extract the rows associtaed with an explanation 
#' # of interest (the first observation in the test data) 
#' eoi <- res$explain[1:2,]
#' 
#' # Plot the explanation of interest
#' eoi_plot(eoi)
#' }

eoi_plot <- function(explanation, bins = TRUE, weights = TRUE, alpha = 1, title.opt = TRUE) {
  
  # Extract the label associated with the explanation
  eoi_label = explanation$label[1]
  
  # Extract the predictions from the complex model associated
  # with the simulated data values
  complex_pred = explanation %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data$perms_pred_complex) %>%
    as.data.frame()
  
  # Extract the weights associated with the simulated values
  sim_weights = explanation %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data$weights)
  
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
    dplyr::slice(1) %>%
    dplyr::pull(.data$perms_raw) %>%
    as.data.frame() %>%
    dplyr::select(all_of(eoi_features)) %>%
    dplyr::mutate(complex_pred = complex_pred %>% 
                    dplyr::pull(tidyselect::all_of(eoi_label)),
                  weights = sim_weights[[1]],
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
                 complex_pred = sim_data$complex_pred,
                 weights = sim_data$weights) %>%
        dplyr::mutate_at(.vars = c("f1", "f2"), .funs = as.character)
    }) %>%
    dplyr::mutate(f1 = factor(.data$f1, levels = unique(feature_pairs[,1])),
                  f2 = factor(.data$f2, levels = unique(feature_pairs[,2])))
  
  # Create a dataframe with the prediction of interest's 
  # observed feature values
  poi_data <- data.frame(feature = explanation$feature,
                         value = explanation$feature_value) %>%
    tidyr::pivot_wider(names_from = "feature", values_from = "value")
  
  # Create data for plotting
  poi_data_plot <- purrr::map_df(
    .x = 1:nrow(feature_pairs),
    .f = function(row) {
      data.frame(f1 = as.character(feature_pairs[row,1]),
                 f2 = as.character(feature_pairs[row,2]),
                 v1 = poi_data %>% pull(feature_pairs[row,1]),
                 v2 = poi_data %>% pull(feature_pairs[row,2]),
                 complex_pred = explanation$label_prob[1]) %>%
        dplyr::mutate_at(.vars = c("f1", "f2"), .funs = as.character)
    }) %>%
    dplyr::mutate(f1 = factor(.data$f1, levels = unique(feature_pairs[,1])),
                  f2 = factor(.data$f2, levels = unique(feature_pairs[,2])))
  
  # Create bin data if requested and appropriate
  if (explanation$sim_method[1] %in% c("quantile_bins", "equal_bins") & bins == TRUE) {
    
    # Extract the bin bounds
    bin_bounds <- suppressWarnings(
      extract_bounds(feature = explanation$feature, 
                     feature_desc = explanation$feature_desc)) %>%
      dplyr::mutate_at(.vars = c("lower", "upper"), 
                       .funs = as.character) %>%
      tidyr::pivot_longer(names_to = "bound", 
                          values_to = "value", 
                          .data$lower:.data$upper) %>%
      tidyr::pivot_wider(names_from = "feature",
                         values_from = "value")
    
    # Create bin data for plotting
    bin_data_plot <- purrr::map_df(
      .x = 1:nrow(feature_pairs),
      .f = function(row) {
        data.frame(f1 = as.character(feature_pairs[row,1]),
                   f2 = as.character(feature_pairs[row,2]),
                   y = bin_bounds %>% pull(feature_pairs[row,1]),
                   x = bin_bounds %>% pull(feature_pairs[row,2])) %>%
          dplyr::mutate_at(.vars = c("f1", "f2"), 
                           .funs = as.character) %>%
          dplyr::mutate_at(.vars = c("y", "x"), 
                           .funs = function(val) as.numeric(as.character(val))) %>%
          dplyr::mutate(y = ifelse(.data$y == -Inf | .data$y == Inf, NA, .data$y),
                        x = ifelse(.data$x == -Inf | .data$x == Inf, NA, .data$x))
      }) %>%
      dplyr::mutate(f1 = factor(.data$f1, levels = unique(feature_pairs[,1])),
                    f2 = factor(.data$f2, levels = unique(feature_pairs[,2])))
    
  }
  
  
  # Start the creation of the plot (including weights based on option specified)
  if (weights == TRUE) {
    plot <- ggplot() + 
      geom_point(data = sim_data_plot,
                 mapping =  aes(x = .data$v2, 
                                y = .data$v1, 
                                color = complex_pred, 
                                size = .data$weights),
                 alpha = alpha)
  } else {
    plot <- ggplot() + 
      geom_point(data = sim_data_plot,
                 mapping =  aes(x = .data$v2, 
                                y = .data$v1, 
                                color = complex_pred),
                 alpha = alpha)
  }
  
  # Add poi data and additional structure to the plot
  plot <- plot + 
    geom_point(data = poi_data_plot %>% 
                 mutate(shape = "Prediction \nof Interest"), 
               mapping = aes(x = .data$v2, 
                             y = .data$v1, 
                             fill = complex_pred, 
                             shape = .data$shape),
               color = "black",
               size = 5,
               alpha = 0.8) +
    facet_grid(.data$f1 ~ .data$f2, scales = "free", switch = "both") + 
    scale_color_gradient2(low = "firebrick", 
                          high = "steelblue", 
                          midpoint = 0.5, 
                          limits = c(0, 1)) + 
    scale_fill_gradient2(low = "firebrick", 
                         high = "steelblue", 
                          midpoint = 0.5, 
                          limits = c(0, 1)) + 
    scale_shape_manual(values = 23) +
    scale_size(range = c(0, 2)) +
    theme_grey() +
    theme(strip.placement = "outside",
          strip.background = element_rect(color = "white", 
                                          fill = "white")) + 
    labs(x = "", 
         y = "", 
         color = "Complex \nModel \nPrediction",
         fill = "Complex \nModel \nPrediction", 
         shape = "",
         size = "Weight") + 
    guides(shape = guide_legend(order = 1),
           fill = FALSE)
  
  # Add the bins to the plot if requested
  if (explanation$sim_method[1] %in% c("quantile_bins", "equal_bins") & bins == TRUE) {
    plot <- plot + 
      geom_vline(data = bin_data_plot %>% select(-.data$y) %>% stats::na.omit(),
                 mapping = aes(xintercept = .data$x)) +
      geom_hline(data = bin_data_plot %>% select(-.data$x) %>% stats::na.omit(),
                 mapping = aes(yintercept = .data$y))
  }
  
  # Add a title to the plot if requested
  if (title.opt == TRUE) {
    plot + 
      labs(title = ifelse(is.na(explanation$nbins),
                          paste0("Case: " ,
                                 explanation$case[1],
                                 "\nSimulation Method: ", 
                                 explanation$sim_method[1], 
                                 "\nGower Exponent:", 
                                 explanation$gower_pow[1]), 
                          paste0("Case: " ,
                                 explanation$case[1],
                                 "\nSimulation Method: ", 
                                 explanation$nbins[1], 
                                 " ", 
                                 explanation$sim_method[1], 
                                 "\nGower Exponent: ", 
                                 explanation$gower_pow[1])))
  } else {
    plot
  }

}
