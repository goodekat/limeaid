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

organize_inputs <- function(sim_method, n_bins){

  # Create a dataframe with the bin based simulation methods
  bin_method = sim_method[!(sim_method %in% c("kernel_density", "normal_approx"))]
  bin_inputs = expand.grid(n_bins, bin_method)
  colnames(bin_inputs) = c("n_bins", "sim_method")

  # Create a dataframe with the density based simulation methods
  density_method = sim_method[!(sim_method %in% c("quantile_bins", "equal_bins"))]
  density_inputs = data.frame(n_bins = rep(4, length(density_method)),
                              sim_method = density_method)

  # Join the simulation methods dataframes
  inputs <- rbind(bin_inputs, density_inputs)

  # Add lime input variables
  inputs <- inputs %>%
    bind_cols(purrr::map_df(.x = inputs$sim_method, .f = method2inputs)) %>%
    select(bin_continuous, quantile_bins, n_bins, use_density)

  # Return the inputs as a list
  return(as.list(inputs))

}

