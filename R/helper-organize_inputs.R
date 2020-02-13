# Function for organizing the simulation methods in a way to be used by LIME
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
