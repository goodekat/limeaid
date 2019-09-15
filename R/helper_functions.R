## Helper Functions for the Paper

## ---------------------------------------------------------------
## Create Bins using a Tree
## ---------------------------------------------------------------

# Function to get from a tree to k bins:
# Inputs: x = feature, y = response variable, k = number of bins
# Outputs: k-1 values for breakpoints (does not include minimum and maximum)

library(tidyverse)
library(tree)

treebink <- function (y, x, k, minsize = 10) {
  
  # Set the mindev: "The within-node deviance must be at least this times 
  # that of the root node for the node to be split." 0.01 is the default
  # value in the tree package.
  dev <- 0.01
  
  # Fit a tree with starting specifications
  tree <- tree::tree(y ~ x, control = tree::tree.control(nobs = length(y), 
                                                         mindev = dev, 
                                                         minsize = minsize))
  while ((length(grep("<leaf>", tree$frame$var)) < k) & (dev > 1e-9)) {
    dev <- dev/10
    tree <- tree::tree(y ~ x, control = tree::tree.control(nobs = length(y), mindev=dev,  minsize = minsize))
  }
  
  tree <- tree::prune.tree(tree, best = k)
  breaks <- na.omit(unique(parse_number(tree$frame$splits[,1])))
  if (length(breaks) == k-1) return(sort(breaks)) # and we're happy because we got all the values we need
  
  
  if (length(breaks) > k-1) {
    # we have too many breaks, now reduce the number
    tree$frame$id <- as.numeric(row.names(tree$frame))
    # tree$frame %>%  ggplot(aes(x = id, y = dev, colour=var)) + geom_point()
    
    tree$frame$splits <- tree$frame$splits[,1]
    subtree <- tree$frame %>% arrange(id) %>% filter(var=="x")
    breaks <- sort(parse_number(subtree$splits[1:(k-1)]))
  }
  if (length(breaks) < k-1) {
    warning("Not enough intervals found, consider decreasing minsize (default is 10)")
  }
  breaks
  
}

# Examples:
# treebink(car.test.frame$Mileage, car.test.frame$Weight, k = 5)
# 
# # gives warning
# treebink(car.test.frame$Mileage, car.test.frame$Weight, k = 10)
# treebink(car.test.frame$Mileage, car.test.frame$Weight, k = 10, minsplit = 5)
# 
# # gives warning
# treebink(car.test.frame$Mileage, car.test.frame$Weight, k = 5, minsplit = 40)

## ---------------------------------------------------------------
## My Version of the LIME Function (includes tree binning option)
## ---------------------------------------------------------------

# x: The features from the training data used for training the model that should be explained.
# y: The resopnse variable from the training dataset
# model: The model whose output should be explained
# preprocess: Function to transform a character vector to the format expected from the model.
# tokenization: function used to tokenize text for the permutations.
# keep_word_position: set to TRUE if to keep order of words. Warning: each word will be replaced by word_position.
# ...: Arguments passed on to methods
# bin_continuous: Should continuous variables be binned when making the explanation
# n_bins: The number of bins for continuous variables if bin_continuous = TRUE
# quantile_bins: Should the bins be based on n_bins quantiles or spread evenly over the range of the training data
# use_density: If bin_continuous = FALSE should continuous data be sampled using a kernel density estimation. If not, 
#              continuous features are expected to follow a normal distribution.
# bin_method: Binning method that should be used ("quantile_bins", "equally_spaced", or "tree")

library(assertthat)

mylime <- function(x, y, model, preprocess = NULL, bin_continuous = TRUE, n_bins = 4, 
                   quantile_bins = TRUE, use_density = TRUE, bin_method = "quantile_bins", ...) {
  
  # Transform a character vector to the format expected from the model.
  if (is.null(preprocess)) preprocess <- function(x) x
  assert_that(is.function(preprocess))
  
  # Create the explaniner 
  explainer <- c(as.list(environment()), list(...))
  
  # Set the explainer values of x and y to NULL
  explainer$x <- NULL
  explainer$y <- NULL
  
  # Determines the type of each variable in the data
  explainer$feature_type <- setNames(sapply(x, function(f) {
    if (is.integer(f)) {
      if (length(unique(f)) == 1) 'constant' else 'integer'
    } else if (is.numeric(f)) {
      if (length(unique(f)) == 1) 'constant' else 'numeric'
    } else if (is.character(f)) {
      'character'
    } else if (is.factor(f)) {
      'factor'
    } else if (is.logical(f)) {
      'logical'
    } else if (inherits(f, 'Date') || inherits(f, 'POSIXt')) {
      'date_time'
    } else {
      stop('Unknown feature type', call. = FALSE)
    }
  }), names(x))
  
  # Return a warning if any variable is a constant
  if (any(explainer$feature_type == 'constant')) {
    warning('Data contains numeric columns with zero variance', call. = FALSE)
  }
  
  # Obtain the bin cuts using the specified method
  bin_method <- as.character(bin_method)
  explainer$bin_cuts <- setNames(lapply(seq_along(x), function(i) {
    if (explainer$feature_type[i] %in% c('numeric', 'integer')) {
      if (bin_method == "quantile_bins") {
        bins <- quantile(x[[i]], seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
        bins[!duplicated(bins)]
      } else if (bin_method == "equally_spaced") {
        d_range <- range(x[[i]], na.rm = TRUE)
        seq(d_range[1], d_range[2], length.out = n_bins + 1)
      } else if (bin_method == "tree"){
        inner <- treebink(y = y[[1]], x[[i]], k = n_bins)
        min <- min(x[[i]])
        max <- max(x[[i]])
        bins <- c(min, inner, max)
        return(bins)
      }
    }
  }), names(x))
  
  # Determine the proportion of values between each of the bin cuts
  explainer$feature_distribution <- setNames(lapply(seq_along(x), function(i) {
    switch(
      explainer$feature_type[i],
      integer = ,
      numeric = if (bin_continuous) {
        table(cut(x[[i]], unique(explainer$bin_cuts[[i]]), labels = FALSE, include.lowest = TRUE))/nrow(x)
      } else if (use_density) {
        density(x[[i]])
      } else {
        c(mean = mean(x[[i]], na.rm = TRUE), sd = sd(x[[i]], na.rm = TRUE))
      },
      character = ,
      logical = ,
      factor = table(x[[i]])/nrow(x),
      NA
    )
  }), names(x))
  
  # Attach a structure to the explainer
  structure(explainer, class = c('data_frame_explainer', 'explainer', 'list'))
  
}

## ---------------------------------------------------------------
## Run the Lime Functions
## ---------------------------------------------------------------

# Function for running the lime functions which runs the lime and explain 
# objects in a list
run_lime <- function(bin_continuous, quantile_bins, nbins, use_density, bin_method,
                     response, features, train, test, rfmodel, label, nfeatures, seed = TRUE){
  
  # Set a seed if requested
  if (seed == TRUE) set.seed(20181128)
  
  # Run my lime function
  lime <- mylime(x = train %>% select(features), 
                 y = ifelse(is.na(response), NA, train %>% select(response)), 
                 model = rfmodel, 
                 bin_continuous = bin_continuous, 
                 n_bins = nbins, 
                 quantile_bins = quantile_bins,
                 use_density = use_density,
                 bin_method = bin_method)
  
  # Run the explain function and add a variable for the number of bins
  explain <- explain(x = test, explainer = lime, labels = label, n_features = nfeatures) %>%
    mutate(bin_continuous = bin_continuous,
           quantile_bins = quantile_bins,
           nbins = nbins,
           use_density = use_density,
           bin_method = bin_method,
           response = response)
  
  return(list(lime = lime, explain = explain))
  
}

## ---------------------------------------------------------------
## Write the Bins
## ---------------------------------------------------------------

# Function for writing bins given the bin boundaries
write_bins <- function(bin_data){
  
  # Compute the number of bins based the input dataframe
  nbins = length(bin_data) - 2
  
  # Create an empty matrix to store the bins in
  bin_matrix <- matrix(NA, nrow = 1, ncol = nbins + 1)
  
  # Put the feature name in the first column
  bin_matrix[,1] <- as.character(bin_data[[1]])
  
  # Put the 1st through 2nd to last bins in the 2nd through 2nd to last columns
  bin_matrix[,2:nbins] <- sapply(2:nbins, 
                          FUN = function(number) sprintf("[%.2f, %.2f)", 
                                                         as.numeric(bin_data[number]),
                                                         as.numeric(bin_data[number + 1])))
  
  # Fill in the last column with the upper bin (based on whether it should be infinite or not)
  bin_matrix[,nbins + 1] <- ifelse(is.na(bin_data[nbins + 2]),
                            sprintf("(%.2f, %s)", as.numeric(bin_data[nbins + 1]), "\U221E"),
                            sprintf("(%.2f, %.2f]", as.numeric(bin_data[nbins + 1]),
                                    as.numeric(bin_data[nbins + 2])))
  
  return(bin_matrix)
  
}

## ---------------------------------------------------------------
## Create a Data Frame with the Bins
## ---------------------------------------------------------------

create_bin_data <- function(lime_object){
  
  # If bin_continuous = TRUE, create a list with data frames on bin information
  if (lime_object$bin_continuous == TRUE){
    
    # Determine the nubmer of bins
    nbins <- length(lime_object$bin_cuts[[1]]) - 1
    
    # Create a dataframe of bin boundaries
    bin_boundaries <- data.frame(Feature = rf_features,
                                 Lower = c(0, -1, rep(0, 7)),
                                 matrix(unlist(lime_object$bin_cuts), nrow = 9, 
                                        byrow = TRUE)[,2:nbins],
                                 Upper = c(1, 1, rep(NA, 7)))
    
    # Use my function "write_bins" to create a dataframe with the bins
    bins <- data.frame(t(apply(bin_boundaries, 1, write_bins)))
    
    # Assign appropriate names to the bin columns
    if (nbins == 2){
      names(bins) <- c("Feature", "Lower Bin", "Upper Bin")
    } else if (nbins == 3) {
      names(bins) <- c("Feature", "Lower Bin", "Middle Bin", "Upper Bin")
    } else {
      names(bins) <- c("Feature", 
                       "Lower Bin",
                       sapply(1:(nbins - 2), function(bin) sprintf("Middle Bin %.0f", bin)),
                       "Upper Bin")
    }
    
    return(list(boundaries = bin_boundaries, bins = bins))
  
  } else {
    
    # If bin_continuous = FALSE, return the following message
    return(list(boundaries = paste("No bins available since bin_continuous = FALSE was selected."),
                bins = paste("No bins available since bin_continuous = FALSE was selected.")))
    
  }
  
}

## ---------------------------------------------------------------
## Label Feature Bins
## ---------------------------------------------------------------

# Function to use for creating bin labels in the test_explain dataset
bin_labeller <- function(feature, feature_value, b_c, q_b, n_b, u_d, b_m, r_v, bin_data, case_info){
  
  if (is.na(feature) | b_c == FALSE) {
    
    # Set feature_bin to NA if feature is NA
    feature_bin <- NA
    
  } else {
    
    # Determine which item in the list of bin divisions to grab 
    # based on the case info
    if (is.na(r_v)) {
      selected_list_item <- case_info %>%
        filter(bin_continuous == b_c, 
               quantile_bins == q_b,
               nbins == n_b, 
               use_density == u_d,
               bin_method == b_m) %>%
        pull(case)
    } else {
      selected_list_item <- case_info %>%
        filter(bin_continuous == b_c, 
               quantile_bins == q_b,
               nbins == n_b, 
               use_density == u_d,
               bin_method == b_m,
               response == r_v) %>%
        pull(case)      
    }
    
    # Subset the bin cuts table to the selected feature
    feature_bin_data <- bin_data[[selected_list_item]] %>%
      filter(Feature == as.character(feature)) %>%
      select(-Feature, -Lower, -Upper)
    
    # Determine which bin the case falls in
    if(feature_value <= feature_bin_data[1]){
      feature_bin <- paste(feature, "(lower bin)")
    } else if (feature_bin_data[n_b - 1] < feature_value){
      feature_bin <- paste(feature, "(upper bin)")
    } else {
      middle_bin_checks <- data.frame(middle_bin_number = 1:(n_b - 2),
                                      contained = sapply(1:(n_b - 2),
                                                         FUN = function(number) {
                                                           feature_bin_data[number] < feature_value &
                                                             feature_value <= feature_bin_data[number + 1]}))
      
      if (n_b == 3){
        feature_bin = sprintf("%s (middle bin)", feature)
      } else {
        feature_bin <- sprintf("%s (middle bin %.0f)", 
                               feature,
                               middle_bin_checks %>% filter(contained == TRUE) %>% pull(middle_bin_number))
      }
      
    }
    
  }
  
  # Return the bin
  return(feature_bin)
  
}

