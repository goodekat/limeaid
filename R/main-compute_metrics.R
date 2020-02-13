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
#' }
#'
#' @export compute_metrics
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

  # If metrics is not specified
  if (metrics == "all") metrics = c("ave_r2", "msee")

  # Compute the metrics
  res <- explanations %>%
    group_by(implementation, sim_method, nbins) %>%
    summarise(ave_r2 = mean(model_r2),
              msee = sum((label_prob - model_prediction)^2) / sqrt(n() - 1)) %>%
    ungroup() %>%
    as.data.frame()

  # Return a dataframe with the desired metrics
  return(res %>% select(implementation, sim_method, nbins, metrics))

}


# Would need the access the simulated data to compute this
# iris_lime_explain$explain$perms_numerified[[1]]
# fidelity_metric <- function(perms, weights){
#
#   p_complex <-
#   p_explainer <-
#   pi
#   L <- sum(pi * ((p_complex - p_explainer)^2))
#   return(L)
#
# }




