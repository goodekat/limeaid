## Functions for computing the fidelity metric on a set of observations

# Computes fidelity metrics for multiple observations (each
# with a set of perturbations)
compute_fidelities <- function(explanations){
  exp_sub <- explanations %>%
    select(.data$label, .data$perms_pred_complex, .data$perms_pred_simple, .data$weights)
  purrr::pmap_dbl(exp_sub, fidelity_metric)
}

# Computes fidelity metric for one observation (with a set of
# perturbations)
fidelity_metric <- function(label, perms_pred_complex,
                            perms_pred_simple, weights){
  p_complex <- perms_pred_complex %>% select(.data$label)
  p_explainer <- perms_pred_simple
  sum(weights * ((p_complex - p_explainer)^2))
}
