#' Compute assessment metrics for LIME implementations
#'
#' Computes various metrics that can be used to assess LIME
#' explainer models for different LIME implementations.
#'
#' @param explanations Explain dataframe from the list returned by apply_lime.
#' @param metrics Vector specifying metrics to compute. Default is 'all'. See details for metrics available.
#' @param include_sd Indicator for whether standard deviation of R2 and/or fidelity should also be returned. (Default is FALSE.)
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
#'                   
#' # Compute metrics to compare lime implementations
#' compute_metrics(res$explain)
#'
#' # Return a table with only the MSEE values
#' compute_metrics(res$explain, metrics = "msee")

compute_metrics <- function(explanations, metrics = "all", include_sd = FALSE){

  # Checks
  checkmate::expect_data_frame(explanations)
  checkmate::expect_character(metrics)
  if (!all(metrics %in% c("all", "ave_r2", "msee", "ave_fidelity"))) {
    stop("metrics specified incorrectly. Must be a character vector with options of 'ave_r2', 'msee', 'ave_fidelity'.")
  } 

  # If metrics is not specified
  if ("all" %in% metrics) metrics = c("ave_r2", "msee", "ave_fidelity")
  
  # If include_sd is TRUE, then add necessary standard deviations to metrics
  if (include_sd == TRUE & "ave_r2" %in% metrics) metrics = c(metrics, "sd_r2")
  if (include_sd == TRUE & "ave_fidelity" %in% metrics) metrics = c(metrics, "sd_fidelity")
  
  # Compute the metrics
  if (include_sd != TRUE) {
    res <- explanations %>%
      group_by(.data$implementation, .data$sim_method, .data$nbins, .data$gower_pow) %>%
      summarise(ave_r2 = mean(.data$model_r2),
                msee = sum((.data$label_prob - .data$model_prediction)^2) /
                  sqrt(n() - 1),
                ave_fidelity = mean(.data$fidelity)) %>%
      ungroup() %>%
      as.data.frame() %>% 
      mutate(implemenation = as.numeric(.data$implementation)) %>% 
      arrange(.data$implemenation) %>% 
      mutate(implemenation = as.character(.data$implemenation)) %>%
      select(.data$implementation, .data$sim_method, .data$nbins, .data$gower_pow, metrics)
  } else {
    res <- explanations %>%
      group_by(.data$implementation, .data$sim_method, .data$nbins, .data$gower_pow) %>%
      summarise(ave_r2 = mean(.data$model_r2),
                sd_r2 = sd(.data$model_r2),
                msee = sum((.data$label_prob - .data$model_prediction)^2) /
                  sqrt(n() - 1),
                ave_fidelity = mean(.data$fidelity),
                sd_fidelity = sd(.data$fidelity)) %>%
      ungroup() %>%
      as.data.frame() %>% 
      mutate(implemenation = as.numeric(.data$implementation)) %>% 
      arrange(.data$implemenation) %>% 
      mutate(implemenation = as.character(.data$implemenation)) %>%
      select(.data$implementation, .data$sim_method, .data$nbins, .data$gower_pow, metrics)
  }

  # Return a dataframe with the desired metrics
  return(res)

}




