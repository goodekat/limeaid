# Function for applying the lime and explain functions for
# one set of input options

lime_explain <- function(bin_continuous, quantile_bins, nbins,
                         use_density, gower_pow, train, test, model, 
                         label, n_features, feature_select,
                         n_permutations, dist_fun, kernel_width,
                         return_perms, all_fs, label_fs, seed){

  # Apply the lime function
  set.seed(seed)
  lime <- lime::lime(x = train,
                     model = model,
                     bin_continuous = bin_continuous,
                     n_bins = nbins,
                     quantile_bins = quantile_bins,
                     use_density = use_density)

  # Apply the explain function and add some additional variables
  set.seed(seed)
  explain <- lime::explain(x = test,
                           explainer = lime,
                           labels = label,
                           n_features = n_features,
                           n_permutations = n_permutations,
                           feature_select = feature_select,
                           dist_fun = dist_fun,
                           kernel_width = kernel_width,
                           gower_pow = gower_pow,
                           all_fs = all_fs,
                           label_fs = label_fs) %>%
    mutate(sim_method = inputs2method(bin_continuous = bin_continuous,
                                      quantile_bins = quantile_bins,
                                      use_density = use_density),
           nbins = ifelse(sim_method %in% c("quantile_bins", "equal_bins"), nbins, NA),
           gower_pow = gower_pow) 
  
  # Compute the fidelity for each observation in the explanations dataset
  explain$fidelity <- compute_fidelities(explain)

  # Reorder variables and determine which to return based on return_perms
  if (return_perms == TRUE) {
    explain <- explain %>% select(sim_method, nbins, gower_pow, everything())
  } else if (return_perms == FALSE) {
    explain <- explain %>% 
      select(sim_method, nbins, gower_pow, everything()) %>%
      select(-perms_raw, -perms_numerified, -perms_pred_complex, 
             -perms_pred_simple, -weights)
  }
  
  # Return the output in a list
  return(list(lime = lime, explain = explain))

}
