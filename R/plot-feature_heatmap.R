#' Heatmap of LIME Selected Features
#'
#' Creates a heatmap of the features selected by lime for all
#' observations in the test set across all of the different
#' LIME implementations.
#'
#' @param explanations Explain dataframe from the list returned by apply_lime.
#' @param feature_nums A vector of integer values from 1 to
#'        \code{nfeatures} (specified in \code{apply_lime}) to 
#'        determine which features selected by LIME should be 
#'        included in the plot.
#' @param facet_var A categorical variable that is the same length as
#'        the data input to apply_lime for the test argument that 
#'        will be used to facet the heatmap. (NULL by default) 
#' @param order_method Method for ordering the predictions: either
#'        "obs_num" which uses the order from the explanation
#'        dataframe (default), "feature_arrange" which sorts by 
#'        the factors within a feature, or one of the options from 
#'        the package seriation (methods currently implemented are 
#'        "ARSA", "GW", "HC", "OLO", "Random", "TSP") 
#'
#' @importFrom checkmate expect_data_frame expect_character
#' @importFrom cluster daisy
#' @importFrom dplyr arrange_at left_join pull
#' @importFrom ggplot2 aes facet_grid geom_point geom_tile ggplot labs scale_color_manual theme theme_bw
#' @importFrom seriation seriate
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of
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

feature_heatmap <- function(explanations, feature_nums = NULL,
                            facet_var = NULL, 
                            order_method = "obs_num"){

  # Checks
  checkmate::expect_data_frame(explanations)
  if (!is.null(feature_nums)) checkmate::expect_double(feature_nums)

  # Prepare the explanation data for plotting
  heatmap_data <- explanations %>%
    select(sim_method, nbins, gower_pow, case, feature, feature_weight) %>%
    mutate(feature_magnitude = abs(feature_weight)) %>%
    group_by(sim_method, nbins, gower_pow, case) %>%
    arrange(sim_method, nbins, gower_pow, case, desc(feature_magnitude)) %>%
    mutate(feature_num = 1:n()) %>%
    ungroup() %>%
    mutate(nbins = factor(nbins),
           gower_pow = factor(paste("Gower Power of", gower_pow)),
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
                                      as.character(nbins)))) %>%
    mutate(case = as.character(case))
  
  # If requested add faceting variable
  if (!is.null(facet_var)) {
    heatmap_data <- heatmap_data %>%
      left_join(data.frame(case = unique(explanations$case), 
                           facet_var = facet_var) %>%
                  mutate(case = as.character(case)),
                by = "case")
  }
  
  # Subset the data to only keep the requested features
  if (!(is.null(feature_nums))) {
    min_feat_num <- min(feature_nums)
    heatmap_data <- heatmap_data %>%
      filter(feature_num %in% feature_nums) %>%
      mutate(feature_num = factor(feature_num),
             feature_num = paste("Feature", feature_num))
  } else {
    min_feat_num <- min(heatmap_data$feature_num)
    heatmap_data <- heatmap_data %>%
      mutate(feature_num = factor(feature_num),
             feature_num = paste("Feature", feature_num))
  }
  
  # If requested, determine an order for the cases using seriation 
  if (order_method == "obs_num") {
    
    # Turn case into a factor and order the levels numerically
    cases_order = sort(as.numeric(as.character(unique(heatmap_data$case))))
    heatmap_data <- heatmap_data %>% 
      mutate(case = factor(case, levels = cases_order))
    
  } else if (order_method == "feature_arrange") {
    
    # Prepare features for distance calculation
    # Columns: simulation method and feature 
    # Row: case in data
    # Cell: feature selected by lime
    sim_features <- heatmap_data %>%
      filter(feature_num == paste("Feature", min_feat_num)) %>%
      mutate(method = paste(sim_method, nbins, gower_pow)) %>%
      select(-feature_weight, -feature_magnitude, -sim_method, 
             -nbins, -gower_pow, -sim_method_plot,
             -nbins_plot, -facet_var) %>%
      tidyr::pivot_wider(names_from = "method", values_from = "feature")
    
    # Determine the order of the cases
    cases_order <- sim_features %>%
      select(-feature_num) %>%
      arrange_at(vars(-case)) %>%
      pull(case)
    
    # Add the order to the heatmap data
    heatmap_data <- heatmap_data %>%
      mutate(case = as.character(case)) %>%
      mutate(case = factor(case, levels = cases_order))
    
  } else {
    
    # Prepare features for distance calculation
    # Columns: simulation method and feature 
    # Row: case in data
    # Cell: feature selected by lime
    sim_features <- heatmap_data %>%
      filter(feature_num == paste("Feature", min_feat_num)) %>%
      mutate(method = paste(sim_method, nbins, gower_pow)) %>%
      select(-feature_weight, -feature_magnitude, -sim_method, 
             -nbins, -gower_pow, -sim_method_plot,
             -nbins_plot, -facet_var) %>%
      tidyr::pivot_wider(names_from = "method", values_from = "feature")
    
    # Calculate the distances between colums and use seriate
    # to order the cases
    features_dist <- cluster::daisy(sim_features %>% select(-case, -feature_num))
    features_ord <- seriation::seriate(features_dist, method = order_method)
    if (order_method %in% c("ARSA", "Random", "TSP")) {
      sim_features$order <- features_ord[[1]][]
    } else if (order_method %in% c("GW", "HC", "OLO")) {
      sim_features$order <- features_ord[[1]][]$order
    }
    
    # Determine the order of the cases
    cases_order <- sim_features %>% 
      select(case, order) %>% 
      arrange(order) %>%
      pull(case)

    # Add the order to the heatmap data
    heatmap_data <- heatmap_data %>%
      mutate(case = as.character(case)) %>%
      mutate(case = factor(case, levels = cases_order))
    
  }
    
  # Create the heatmap
  plot <- 
    ggplot(heatmap_data, 
           aes(x = nbins_plot, y = case, fill = feature)) +
    geom_tile() +
    theme_bw() +
    labs(x = "Number of Bins",
         y = "Prediction Number",
         fill = "Feature")

  # Facet and return the plot
  if (!is.null(facet_var)) {
    plot + 
      facet_grid(feature_num + facet_var ~ 
                   sim_method_plot + gower_pow, 
                 scales = "free", space = "free")
  } else {
    plot + 
      facet_grid(feature_num ~ sim_method_plot + gower_pow, 
                 scales = "free", space = "free") 
  }
    
}

