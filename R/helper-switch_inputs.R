# Functions for switching between apply_lime input values
# and lime input values

# Given a "simulation method" return the LIME input values for simulation
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

# Given the LIME input values for simulation return the concise "simulation method"
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
