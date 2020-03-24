#' Apply Different Implementations of LIME
#'
#' Applies LIME with the specified tunning parameter options.
#'
#' @param train Dataframe of training data features.
#' @param test Dataframe of testing data features.
#' @param model Complex model to be explained.
#' @param sim_method Vector of methods to use for creating the simulated
#'        data. Options are 'quantile_bins', 'equal_bins',
#'        'kernel_density', and 'normal_approx'.
#' @param nbins Vector of number of bins to use with bin based
#'        simulation methods.
#' @param label Response category to use in the explanations. Current
#'        implementation only accepts 1 label.
#' @param n_features Number of features to return in the explanations.
#' @param n_permutations Number of permutations to use when simulating
#'        data for each explanation. Default is 5000.
#' @param feature_select Feature selection method. Options are 'auto',
#'        'none', 'forward_selection', 'highest_weights', 'lasso_path',
#'        and 'tree'.
#' @param dist_fun Distance function to use when computing weights for
#'        the simulated data. Default is 'gower'. Otherwise,
#'        \code{stats::dist()} will be used.
#' @param kernel_width Kernel width to use if \code{dist_fun} is not
#'        'gower'.
#' @param gower_pow Numeric vector of powers to use when computing 
#'        the Gower distance.
#' @param return_perms Should the simulated dataset (permutations) be 
#'        returned for all of the observations in the test datatset and
#'        LIME implementations? Default is FALSE.
#' @param all_fs Indicates whether all feature selection methods
#'        should be applied for an implementation of LIME to see how
#'        the features selected varies within a LIME implemenation.
#'        Note that the LIME results returned will correspond to the 
#'        method specified in the \code{feature_selection} option.
#' @param parallel Indicates whether to perform the application of
#'        LIME using parallel computation (with furrr) or without
#'        (with purrr). Default is FALSE. Setting parallel = TRUE may 
#'        help with computation time with very large test datasets or
#'        many different sets of tuning parameters.
#' @param seed Number to be used as a seed (if desired).
#'
#' @importFrom checkmate expect_character expect_data_frame expect_double      
#' @importFrom dplyr arrange bind_cols everything filter group_by mutate n select summarise ungroup %>%
#' @importFrom future multisession plan
#' @importFrom furrr future_pmap
#' @importFrom lime lime explain
#' @importFrom purrr map map_df pmap pmap_dbl
#' @importFrom rlang .data
#'
#' @export apply_lime
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
#'                   nbins = 2:3)

apply_lime <- function(train, test, model, sim_method, nbins = 4,
                       label, n_features, n_permutations = 5000,
                       feature_select = "auto", dist_fun = "gower",
                       kernel_width = NULL, gower_pow = 1,
                       all_fs = FALSE, return_perms = FALSE,
                       parallel = FALSE, seed = NULL){

  # Checks
  checkmate::expect_data_frame(train)
  checkmate::expect_data_frame(test)
  if (!all(sim_method %in% c("quantile_bins", "equal_bins", "kernel_density", "normal_approx"))) {
    stop("sim_method specified incorrectly. Must be a character vector with options of 'quantile_bins', 'equal_bins', 'kernel_density', or 'normal_approx'.")
  } 
  checkmate::expect_numeric(nbins)
  checkmate::expect_numeric(n_features, len = 1)
  checkmate::expect_numeric(n_permutations, len = 1)
  if (!(feature_select %in% c('auto', 'none', 'forward_selection', 'highest_weights', 'lasso_path', 'tree'))) {
    stop("feature_select specified incorrectly. Options are 'auto', 'none', 'forward_selection', 'highest_weights', 'lasso_path', or 'tree'.")
  }
  checkmate::expect_numeric(gower_pow)
  checkmate::expect_logical(all_fs)
  
  # Put the input options into a list
  inputs <- organize_inputs(sim_method, nbins, gower_pow) # helper
  
  # Apply the lime and explain functions for all specified inputs
  if (parallel == TRUE) {
    
    # Tell R to run the upcoming code in parallel
    future::plan(future::multisession)
    
    # Apply lime (with furrr)
    results <- furrr::future_pmap(.l = inputs,
                                  .f = lime_explain, # helper
                                  train = train,
                                  test = test,
                                  model = model,
                                  label = label,
                                  n_features = n_features,
                                  n_permutations = n_permutations,
                                  feature_select = feature_select,
                                  dist_fun = "gower",
                                  kernel_width = kernel_width,
                                  return_perms = return_perms,
                                  all_fs = all_fs,
                                  label_fs = label, 
                                  seed = seed)
  } else {
    
    # Apply lime (with purrr)
    results <- purrr::pmap(.l = inputs,
                           .f = lime_explain, # helper
                           train = train,
                           test = test,
                           model = model,
                           label = label,
                           n_features = n_features,
                           n_permutations = n_permutations,
                           feature_select = feature_select,
                           dist_fun = "gower",
                           kernel_width = kernel_width,
                           return_perms = return_perms,
                           all_fs = all_fs,
                           label_fs = label, 
                           seed = seed)
  }
  
  # Separate the lime and explain function results
  results <- list(lime = purrr::map(results, function(list) list$lime),
                  explain = purrr::map_df(results,
                                          function(list) list$explain,
                                          .id = "implementation"))

  # Specify the order of the factors of sim_method
  sim_method_levels <- sim_method
  results$explain <- results$explain %>%
    dplyr::mutate(sim_method = factor(.data$sim_method, levels = sim_method_levels))

  # Name the items in the lime list
  names(results$lime) <- 
    purrr::map_chr(.x = 1:length(results$lime), 
                   .f = function(case) {
                     method = 
                       inputs2method(bin_continuous = inputs$bin_continuous[case], 
                                     use_density = inputs$use_density[case], 
                                     quantile_bins = inputs$quantile_bins[case])
                     sprintf("case: %s %s, gower_pow = %s",
                             ifelse(method %in% c("quantile_bins", "equal_bins"), inputs$nbins[case], ""),
                             method, 
                             inputs$gower_pow[case])
                     }
                   )
      
  # Return the results from lime
  return(results)

}
