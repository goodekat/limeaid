#' Apply LIME
#'
#' Applies LIME with the specified tunning parameter options
#'
#' @param train Dataframe of training data featues
#' @param test Dataframe of testing data features
#' @param model Complex model to be explained
#' @param label Vector of response category or categories to use in the explanations
#' @param nfeatures Number of features to return in the explanations
#' @param sim_method Vector of methods to use for creating the simulated
#'        data. Options are 'quantile_bins', 'equal_bins', 'kernel_density',
#'        and 'normal_approx'
#' @param nbins Vector of number of bins to use with bin based simulation methods
#' @param feature_select Feature selection method
#' @param dist_fun Distance function
#' @param kernel_width Kernel width
#' @param gower_pow Power to use when computing the Gower distance
#' @param nreps Number of times to apply LIME for each set of input options
#' @param seed Seed number if specifying a seed is desired
#'
#' @importFrom dplyr bind_cols mutate select %>%
#' @importFrom future multisession plan
#' @importFrom furrr future_pmap
#' @importFrom lime as_classifier lime explain
#' @importFrom purrr map map_df
#'
#' @export apply_lime
#'
#' @examples
#'
#' library(caret)
#'
#' # Split up the data set
#' iris_test <- iris[1:5, 1:4]
#' iris_train <- iris[-(1:5), 1:4]
#' iris_lab <- iris[[5]][-(1:5)]
#'
#' # Create Random Forest model on iris data
#' model <- train(iris_train, iris_lab, method = 'rf')
#'
#' iris_lime_explain <- apply_lime(train = iris_train,
#'                                 test = iris_test,
#'                                 model = model,
#'                                 label = "virginica",
#'                                 n_features = 2,
#'                                 sim_method = c('quantile_bins', 'equal_bins', 'kernel_density', 'normal_approx'),
#'                                 nbins = 2,
#'                                 seed = 20190914)

apply_lime <- function(train, test, model, label, n_features,
                       sim_method, nbins, feature_select = "auto",
                       dist_fun = "gower", kernel_width = NULL,
                       gower_pow = 1, nreps = 1, seed = NULL){

  # Set a seed if requested
  if (!is.null(seed)) set.seed(seed)

  # Put the input options into a list
  inputs <- organize_inputs(sim_method, nbins)

  # Tell R to run the upcoming code in parallel
  future::plan(future::multisession)

  # Apply the lime and explain functions for all specified inputs
  results <- furrr::future_pmap(.l = inputs,
                                .f = lime_explain, # lime_explain is a helper function in limeaid
                                train = train,
                                test = test,
                                model = model,
                                label = label,
                                n_features = n_features)

  # Separate the lime and explain function results
  results <- list(lime = map(results, function(list) list$lime),
                  explain = map_df(results,
                                   function(list) list$explain,
                                   .id = "implementation"))

  # Specify the order of the factors of sim_method
  sim_method_levels <- sim_method
  results$explain <- results$explain %>%
    mutate(sim_method = factor(sim_method, levels = sim_method_levels))

  # Name the items in the lime list
  # names(hamby224_lime) <- map_chr(1:22, function(case)
  #     sprintf("case: bin_continuous = %s, quantile_bins = %s, nbins = %0.f, use_density = %s, bin_method = %s, response = %s",
  #             hamby224_lime_inputs$bin_continuous[case],
  #             hamby224_lime_inputs$quantile_bins[case],
  #             hamby224_lime_inputs$nbins[case],
  #             hamby224_lime_inputs$use_density[case],
  #             hamby224_lime_inputs$bin_method[case],
  #             hamby224_lime_inputs$response[case]))

  # Return the results from lime
  return(results)

}

# Organize the simulation methods in a way to be used by LIME
organize_inputs <- function(sim_method, nbins){

  # Create a dataframe with the bin based simulation methods
  bin_method = sim_method[!(sim_method %in% c("kernel_density", "normal_approx"))]
  bin_inputs = expand.grid(nbins, bin_method)
  colnames(bin_inputs) = c("nbins", "sim_method")

  # Create a dataframe with the density based simulation methods
  density_method = sim_method[!(sim_method %in% c("quantile_bins", "equal_bins"))]
  density_inputs = data.frame(nbins = rep(4, length(density_method)),
                              sim_method = density_method)

  # Join the simulation methods dataframes
  inputs <- rbind(bin_inputs, density_inputs)

  # Add lime input variables
  inputs <- inputs %>%
    bind_cols(purrr::map_df(.x = inputs$sim_method, .f = method2inputs)) %>%
    select(bin_continuous, quantile_bins, nbins, use_density)

  # Return the inputs as a list
  return(as.list(inputs))

}

# Helper function for applying the lime and explain functions for
# one set of input options
lime_explain <- function(bin_continuous, quantile_bins, nbins,
                         use_density, train, test, model, label,
                         n_features){

  # Apply the lime function
  lime <- lime::lime(x = train,
                     model = model,
                     bin_continuous = bin_continuous,
                     n_bins = nbins,
                     quantile_bins = quantile_bins,
                     use_density = use_density)

  # Apply the explain function and add some additional variables
  explain <- lime::explain(x = test,
                            explainer = lime,
                            labels = label,
                            n_features = n_features) %>%
    mutate(sim_method = inputs2method(bin_continuous = bin_continuous,
                                      quantile_bins = quantile_bins,
                                      use_density = use_density),
           nbins = ifelse(sim_method %in% c("quantile_bins", "equal_bins"), nbins, NA)) %>%
    select(sim_method, nbins, model_type:prediction)

  return(list(lime = lime, explain = explain))

}

join_test_explain <- function(test, explain){

}

# Join the data and the explanations and edit and add additional variables
# Create the feature bin labels using my function "bin_labeller"
  # hamby224_test_explain <- hamby224_test %>%
  #   mutate(case = as.character(case)) %>%
  #   full_join(hamby224_explain, by = "case") %>%
  #   mutate(case = factor(case),
  #          feature_desc = factor(feature_desc),
  #          feature_bin = pmap_chr(list(feature = feature,
  #                                 feature_value = feature_value,
  #                                 b_c = bin_continuous,
  #                                 q_b = quantile_bins,
  #                                 n_b = nbins,
  #                                 u_d = use_density,
  #                                 b_m = bin_method,
  #                                 r_v = response),
  #                           .f = bin_labeller, # bin_labeller is one of my helper functions
  #                           bin_data = hamby224_bin_boundaries,
  #                           case_info = hamby224_lime_inputs)) %>%
  #   mutate(feature = factor(feature),
  #          nbins = factor(nbins),
  #          feature_number = readr::parse_number(as.character(feature_desc)),
  #          strictly_less = FALSE) %>%
  #   arrange(nbins)
  #
  # # Finish creating the strictly less than variable
  # hamby224_test_explain$strictly_less[grep("< ", hamby224_test_explain$feature_desc)] <- TRUE
  #
  # # Reorder the variables of feature_desc and feature_bin for plotting purposes and
  # # create new variables of situation and bin_situation
  # hamby224_test_explain <- hamby224_test_explain %>%
  #   mutate(feature_desc = reorder(feature_desc, strictly_less),
  #          feature_desc = reorder(feature_desc, feature_number),
  #          feature_desc = reorder(feature_desc, as.numeric(feature))) %>%
  #   mutate(nbins = as.numeric(as.character(nbins)),
  #          situation = ifelse(bin_continuous == TRUE & bin_method == "quantile_bins",
  #                             sprintf("%.0f quantile", nbins),
  #                             ifelse(bin_continuous == TRUE & bin_method == "equally_spaced",
  #                                    sprintf("%.0f equally spaced", nbins),
  #                                    ifelse(bin_continuous == TRUE & bin_method == "tree" &
  #                                             response == "samesource",
  #                                           sprintf("%.0f samesource tree", nbins),
  #                                           ifelse(bin_continuous == TRUE & bin_method == "tree" &
  #                                             response == "rfscore",
  #                                             sprintf("%.0f rfscore tree", nbins),
  #                                             ifelse(bin_continuous == FALSE &
  #                                                    use_density == TRUE,
  #                                                    "kernel density",
  #                                                    "normal approximation"))))) %>%
  #            fct_relevel("2 quantile", "3 quantile", "4 quantile",
  #                        "5 quantile", "6 quantile", "2 equally spaced",
  #                        "3 equally spaced", "4 equally spaced",
  #                        "5 equally spaced", "6 equally spaced",
  #                        "2 samesource tree", "3 samesource tree",
  #                        "4 samesource tree", "5 samesource tree",
  #                        "6 samesource tree")) %>%
  #   mutate(bin_situation = ifelse(bin_method == "quantile_bins" &
  #                                 bin_continuous == TRUE,
  #                                 "quantile",
  #                                 ifelse(bin_method == "equally_spaced" &
  #                                        bin_continuous == TRUE,
  #                                        "equally spaced",
  #                                        ifelse(bin_method == "tree" &
  #                                               bin_continuous == TRUE &
  #                                               response == "samesource",
  #                                               "samesource tree",
  #                                               ifelse(bin_method == "tree" &
  #                                                      bin_continuous == TRUE &
  #                                                      response == "rfscore",
  #                                                      "rfscore tree",
  #                                                      ifelse(bin_continuous == FALSE &
  #                                                      use_density == TRUE,
  #                                                      "kernel density",
  #                                                      "normal approximation")))))) %>%
  #   mutate(bin_situation = factor(bin_situation)) %>%
  #   select(situation, bin_situation, bin_continuous:response, case:feature_desc,
  #          feature_bin:strictly_less, data, prediction)
