#' Compute the locality metric for LIME implementations
#'
#' Computes the locality metric measuring how well the explainer
#' models approximate the complex model for different LIME
#' implementations.
#'
#' @export compare_limes

# Right now these metrics are being computed on the
# LIME explanations

compare_limes <- function(explanations){

  res <- explanations %>%
    group_by(implementation, sim_method, nbins) %>%
    summarise(ave_r2 = mean(model_r2),
              msee = sum((label_prob - model_prediction)^2) / sqrt(n() - 1)) %>%
    ungroup() %>%
    as.data.frame()

  return(res)

}


# Would need the access the simulated data to compute this
# locality_metric <- function(explanations){
#
#   p_complex <- explanations$label_prob
#   p_explainer <- explanations$model_prediction
#
#   L <- sum(pi * ((p_complex - p_explainer)^2))
#   return(L)
#
# }




