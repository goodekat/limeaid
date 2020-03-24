# Functions for cleaning the inputs used by apply_limes and
# lime/explain functions (listed alphabetically)
#
# - inputs2method:   Given the LIME input values for simulation
#                    return the concise "simulation method"
# - methods2inputs:  Given a "simulation method" return the LIME
#                    input values for simulation
# - organize_inputs: Function for organizing the simulation
#                    methods in a way to be used by LIME

inputs2method <- function(bin_continuous, quantile_bins, use_density){

  # Bin cases
  if (bin_continuous == TRUE) {
    if (quantile_bins == TRUE) {
      sim_method = "quantile_bins"
    } else if (quantile_bins == FALSE) {
      sim_method = "equal_bins"
    }

    # Density cases
  } else if (bin_continuous == FALSE){
    if (use_density == TRUE) {
      sim_method = "kernel_density"
    } else if (use_density == FALSE) {
      sim_method = "normal_approx"
    }
  }

  # Return the simulation method
  return(sim_method)

}

method2inputs <- function(sim_method){

  # Quantile bins case
  if (sim_method == "quantile_bins") {
    bin_continuous = TRUE
    quantile_bins = TRUE
    use_density = TRUE

    # Equal bins case
  } else if (sim_method == "equal_bins") {
    bin_continuous = TRUE
    quantile_bins = FALSE
    use_density = TRUE

    # Kernel density case
  } else if (sim_method == "kernel_density") {
    bin_continuous = FALSE
    quantile_bins = TRUE
    use_density = TRUE

    # Normal approximation case
  } else if (sim_method == "normal_approx") {
    bin_continuous = FALSE
    quantile_bins = TRUE
    use_density = FALSE

  }

  # Return the input parameters in a dataframe
  return(data.frame(bin_continuous, quantile_bins, use_density))

}
               
organize_inputs <- function(sim_method, nbins, gower_pow){
  
  # Create a dataframe with the bin based simulation methods
  bin_method = sim_method[!(sim_method %in% c("kernel_density", "normal_approx"))]
  bin_inputs = expand.grid(nbins, bin_method, gower_pow)
  colnames(bin_inputs) = c("nbins", "sim_method", "gower_pow")

  # Create a dataframe with the density based simulation methods
  density_method = sim_method[!(sim_method %in% c("quantile_bins", "equal_bins"))]
  density_inputs = expand.grid(density_method, gower_pow)
  colnames(density_inputs) = c("sim_method", "gower_pow")
  density_inputs <- density_inputs %>%
    mutate(nbins = rep(4, n()))
  
  # Join the simulation methods dataframes
  inputs <- rbind(bin_inputs, density_inputs)
  
  # Add lime input variables (and add a perm_seed if needed)
  if (length(gower_pow) > 1) {
    inputs <- inputs %>%
      dplyr::group_by(.data$nbins, .data$sim_method) %>%
      dplyr::mutate(perm_seed = sample(100000:999999, 1)) %>%
      dplyr::ungroup() %>%
      dplyr::bind_cols(purrr::map_df(.x = inputs$sim_method, .f = method2inputs)) %>%
      dplyr::select(.data$bin_continuous, .data$quantile_bins, 
                    .data$nbins, .data$use_density, .data$gower_pow,
                    .data$perm_seed)
  } else {
    inputs <- inputs %>%
      dplyr::bind_cols(purrr::map_df(.x = inputs$sim_method, .f = method2inputs)) %>%
      dplyr::select(.data$bin_continuous, .data$quantile_bins, 
                    .data$nbins, .data$use_density, .data$gower_pow)
  }
  
  # Return the inputs as a list
  return(as.list(inputs))

}

