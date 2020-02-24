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
#' @param all_fs Indicates whether all feature selection methods
#'        should be applied for an implementation of LIME to see how
#'        the features selected varies within a LIME implemenation.
#'        Note that the LIME results returned will correspond to the 
#'        method specified in the \code{feature_selection} option. 
#'        This option must be used with  the \code{label_fs} option 
#'        to specify the label to use for feature selection. (Only 
#'        one label is allowed for now.)
#' @param label_fs The response label to use when all feature
#'        selection methods are implemented.
#' @param seed Number to be used as a seed (if desired).
#' @param apply_method The package that should be used to apply LIME
#'        multiple times. Options are 'purrr' (default), 'furrr', or
#'        'future_furrr'.
#'        
#' @importFrom dplyr arrange bind_cols everything filter group_by mutate n select summarise ungroup %>%
#' @importFrom future multisession plan
#' @importFrom furrr future_pmap
#' @importFrom lime lime explain
#' @importFrom purrr map map_df pmap pmap_dbl
#'
#' @export apply_lime
#'
#' @examples
#'
#' # Create Random Forest model on the sine data
#' rfsine <- caret::train(x = sine_data_train[c("x1", "x2", "x3")],
#'                        y = sine_data_train$y,
#'                        method = "rf")
#'
#' # Apply lime
#' sine_lime_explain <-
#'    apply_lime(train = sine_data_train[c("x1", "x2", "x3")],
#'               test = sine_data_test[c("x1", "x2", "x3")],
#'               model = rfsine,
#'               label = "1",
#'               n_features = 2,
#'               sim_method = c('quantile_bins', 'kernel_density'),
#'               nbins = c(3, 4),
#'               seed = 20190914)

apply_lime <- function(train, test, model, sim_method, nbins,
                       label, n_features, n_permutations = 5000,
                       feature_select = "auto", dist_fun = "gower",
                       kernel_width = NULL, gower_pow = 1,
                       all_fs = FALSE, label_fs = NULL,
                       seed = NULL, apply_method = "purrr"){

  # Put the input options into a list
  inputs <- organize_inputs(sim_method, nbins, gower_pow) # helper

  # Apply the lime and explain functions for all specified inputs
  if (apply_method == "purrr") {
    if (!is.null(seed)) set.seed(seed)
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
                           all_fs = all_fs,
                           label_fs = label_fs)
  } else if (apply_method == "furrr") {
    if (!is.null(seed)) set.seed(seed)
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
                                  all_fs = all_fs,
                                  label_fs = label_fs)
  } else if (apply_method == "future_furrr") {
    # Tell R to run the upcoming code in parallel
    future::plan(future::multisession)
    if (!is.null(seed)) set.seed(seed)
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
                                  all_fs = all_fs,
                                  label_fs = label_fs)
  }


  # Separate the lime and explain function results
  results <- list(lime = purrr::map(results, function(list) list$lime),
                  explain = purrr::map_df(results,
                                          function(list) list$explain,
                                          .id = "implementation"))

  # Specify the order of the factors of sim_method
  sim_method_levels <- sim_method
  results$explain <- results$explain %>%
    mutate(sim_method = factor(sim_method, levels = sim_method_levels))

  # Name the items in the lime list
  names(results$lime) <- purrr::map_chr(1:length(results$lime), function(case)
      sprintf("case: %s %s",
              ifelse(sim_method[case] %in% c("quantile_bins", "equal_bins"), nbins, ""),
              sim_method[case]))

  # Return the results from lime
  return(results)

}
