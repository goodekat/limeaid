# Function for applying the lime and explain functions for
# one set of input options
lime_explain <- function(bin_continuous, quantile_bins, n_bins,
                         use_density, train, test, model, label,
                         n_features, feature_select, seed,
                         n_permutations, dist_fun,
                         kernel_width, gower_pow){

  # Set a seed if requested
  if (!is.null(seed)) set.seed(seed)

  # Apply the lime function
  lime <- lime::lime(x = train,
                     model = model,
                     bin_continuous = bin_continuous,
                     n_bins = n_bins,
                     quantile_bins = quantile_bins,
                     use_density = use_density)

  # Apply the explain function and add some additional variables
  explain <- lime::explain(x = test,
                           explainer = lime,
                           labels = label,
                           n_features = n_features,
                           n_permutations = n_permutations,
                           feature_select = feature_select,
                           dist_fun = dist_fun,
                           kernel_width = kernel_width,
                           gower_pow = gower_pow) %>%
    mutate(sim_method = inputs2method(bin_continuous = bin_continuous,
                                      quantile_bins = quantile_bins,
                                      use_density = use_density),
           n_bins = ifelse(sim_method %in% c("quantile_bins", "equal_bins"), n_bins, NA)) %>%
    select(sim_method, n_bins, everything())

  return(list(lime = lime, explain = explain))

}
