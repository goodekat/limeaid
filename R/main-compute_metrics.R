#' Compute assessment metrics for LIME implementations
#'
#' Computes various metrics that can be used to assess LIME
#' explainer models for different LIME implementations.
#'
#' @param explanations Explain dataframe from the list returned by apply_lime.
#' @param metrics Vector specifying metrics to compute. Default is 'all'. See details for metrics available.
#'
#' @details The metrics available are listed below.
#'
#' \itemize{
#'   \item \code{ave_r2}: Average explainer model R2 value computed over all explanations in the test set.
#'   \item \code{msee}: Mean square explanation error computed over all explanations in the test set.
#'   \item \code{ave_fidelity}: Average fidelity metric (Ribeiro et. al. 2016) computed over all explanations in the test set.
#' }
#'
#' @references Ribeiro, M. T., S. Singh, and C. Guestrin, 2016:
#'   "why should I trust you?": Explaining the predictions of any classifier.
#'   Proceedings of the 22nd ACM SIGKDD Inter- national Conference on
#'   Knowledge Discovery and Data Mining, San Francisco, CA, USA, August
#'   13-17, 2016, 1135–1144.
#'
#' @export compute_metrics
#' @importFrom dplyr arrange
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
#' # Compute metrics to compare lime implementations
#' compute_metrics(sine_lime_explain$explain)
#'
#' # Return a table with only the MSEE values
#' compute_metrics(sine_lime_explain$explain, metrics = "msee")

compute_metrics <- function(explanations, metrics = "all"){

  # Checks
  checkmate::expect_data_frame(explanations)
  checkmate::expect_character(metrics)
  if (!all(metrics %in% c("all", "ave_r2", "msee", "ave_fidelity"))) {
    stop("metrics specified incorrectly. Must be a character vector with options of 'ave_r2', 'msee', 'ave_fidelity'.")
  } 

  # If metrics is not specified
  if ("all" %in% metrics) metrics = c("ave_r2", "msee", "ave_fidelity")

  # Compute the metrics
  res <- explanations %>%
    group_by(implementation, sim_method, nbins, gower_pow) %>%
    summarise(ave_r2 = mean(model_r2),
              msee = sum((label_prob - model_prediction)^2) /
                sqrt(n() - 1),
              ave_fidelity = mean(fidelity)) %>%
    ungroup() %>%
    as.data.frame() %>% 
    mutate(implemenation = as.numeric(implementation)) %>% 
    arrange(implemenation) %>% 
    mutate(implemenation = as.character(implemenation))

  # Return a dataframe with the desired metrics
  return(res %>% select(implementation, sim_method, nbins, gower_pow, metrics))

}




