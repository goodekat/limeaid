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
#'        dataframe (default), "sort_features" which sorts by 
#'        the factors within a feature using the dplyr "arrange" function, 
#'        or one of the options from the package seriation for matrices 
#'        (see seriation::list_seriation_methods("matrix") for the options
#'        available.) 
#'
#' @importFrom checkmate expect_data_frame expect_character
#' @importFrom cluster daisy
#' @importFrom dplyr arrange_at desc left_join pull vars
#' @importFrom ggplot2 aes facet_grid geom_point geom_tile ggplot labs scale_color_grey scale_color_manual scale_fill_grey theme theme_bw
#' @importFrom seriation list_seriation_methods seriate
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of
#'
#' @export feature_heatmap
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
#'                   sim_method = c('quantile_bins',
#'                                  'kernel_density'),
#'                   nbins = 2:4)
#'                   
#' # Plot heatmap of selected features across LIME implementations
#' feature_heatmap(res$explain)
#'
#' # Return a heatmap with only the features selected first by LIME
#' feature_heatmap(res$explain, feature_num = 1)

feature_heatmap <- function(explanations, feature_nums = NULL,
                            facet_var = NULL, order_method = "obs_num"){

  # Checks
  checkmate::expect_data_frame(explanations)
  if (!is.null(feature_nums)) checkmate::expect_double(feature_nums)

  # Prepare the explanation data for plotting
  heatmap_data <- explanations %>%
    select(.data$sim_method, .data$nbins, .data$gower_pow, 
           .data$case, .data$feature, .data$feature_weight) %>%
    mutate(feature_magnitude = abs(.data$feature_weight)) %>%
    group_by(.data$sim_method, .data$nbins, .data$gower_pow, .data$case) %>%
    arrange(.data$sim_method, .data$nbins, .data$gower_pow, 
            .data$case, desc(.data$feature_magnitude)) %>%
    mutate(feature_num = 1:n()) %>%
    ungroup() %>%
    mutate(nbins = factor(.data$nbins),
           gower_pow = factor(paste("Gower \nPower of", .data$gower_pow)),
           feature = factor(.data$feature),
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
    mutate(case = as.character(.data$case))
  
  # If requested add faceting variable
  if (!is.null(facet_var)) {
    heatmap_data <- heatmap_data %>%
      left_join(data.frame(case = unique(explanations$case), 
                           facet_var = facet_var) %>%
                  mutate(case = as.character(.data$case)),
                by = "case")
  }
  
  # Subset the data to only keep the requested features
  if (!(is.null(feature_nums))) {
    min_feat_num <- min(feature_nums)
    heatmap_data <- heatmap_data %>%
      filter(.data$feature_num %in% feature_nums) %>%
      mutate(feature_num = factor(.data$feature_num),
             feature_num = paste("Feature", .data$feature_num))
  } else {
    min_feat_num <- min(heatmap_data$feature_num)
    heatmap_data <- heatmap_data %>%
      mutate(feature_num = factor(.data$feature_num),
             feature_num = paste("Feature", .data$feature_num))
  }
  
  # If requested, determine an order for the cases using seriation 
  if (order_method == "obs_num") {
    
    # Turn case into a factor and order the levels numerically
    cases_order = sort(as.numeric(as.character(unique(heatmap_data$case))))
    heatmap_data <- heatmap_data %>% 
      mutate(case = factor(.data$case, levels = cases_order))
    
  } else if (order_method == "sort_features") {
    
    # Prepare features for ordering
    sim_features <- heatmap_data %>%
      filter(.data$feature_num == paste("Feature", min_feat_num)) %>%
      mutate(method = paste(.data$sim_method, .data$nbins, .data$gower_pow)) %>%
      select(-.data$feature_weight, -.data$feature_magnitude, -.data$sim_method, 
             -.data$nbins, -.data$gower_pow, -.data$sim_method_plot,
             -.data$nbins_plot, -facet_var) %>%
      tidyr::pivot_wider(names_from = "method", values_from = "feature")
    
    # Determine the order of the cases
    cases_order <- sim_features %>%
      select(-.data$feature_num) %>%
      arrange_at(vars(-.data$case)) %>%
      pull(.data$case)
    
    # Add the order to the heatmap data
    heatmap_data <- heatmap_data %>%
      mutate(case = as.character(.data$case)) %>%
      mutate(case = factor(.data$case, levels = cases_order))
    
  } else if (order_method %in% seriation::list_seriation_methods("matrix")) {
    
    # Arrange the data by case and extract the unique cases
    heatmap_data <- heatmap_data %>% arrange(.data$case) 
    cases <- unique(heatmap_data$case)
    
    # Prepare features for ordering
    feature_matrix <- heatmap_data %>%
      dplyr::filter(.data$feature_num == paste("Feature", min_feat_num)) %>%
      dplyr::mutate(method = paste(.data$sim_method, .data$nbins, .data$gower_pow)) %>%
      dplyr::select(.data$method, .data$feature, .data$case) %>% 
      mutate(feature = as.numeric(.data$feature)) %>%
      tidyr::pivot_wider(names_from = "method", values_from = "feature") %>% 
      select(-.data$case) %>%
      as.matrix()
    
    # Determine the order of the cases using seriation
    # default method is PCA, 
    # seriation::list_seriation_methods("matrix")
    # lists all
    features_ordered <- seriate(feature_matrix, method = order_method)
    
    # Add the order to the heatmap data
    heatmap_data <- heatmap_data %>% 
      mutate(case = factor(.data$case, levels = cases[features_ordered[[1]]]))
    
  } else {
    stop("order_method not specified correctly. See ?feature_heatmap for available options.")
  }
    
  # Create the heatmap
  plot <- 
    ggplot(heatmap_data, 
           aes(x = .data$nbins_plot, y = .data$case, fill = .data$feature, color = .data$feature)) +
    geom_tile() +
    theme_grey() +
    labs(x = "Number of Bins",
         y = "Prediction Number",
         fill = "Feature",
         color = "Feature")

  # Facet, add grey scale colors, and return the plot
  if (!is.null(facet_var)) {
    plot + 
      facet_grid(.data$feature_num + .data$facet_var ~ 
                   .data$sim_method_plot + .data$gower_pow, 
                 scales = "free", space = "free") + 
      scale_fill_grey() +
      scale_color_grey()
  } else {
    plot + 
      facet_grid(.data$feature_num ~ .data$sim_method_plot + .data$gower_pow, 
                 scales = "free", space = "free") + 
      scale_fill_grey() + 
      scale_color_grey()
  }
    
}

