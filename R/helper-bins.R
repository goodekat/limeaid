## Helper functions for working with bins

# Function for extracting the numeric bounds from the 
# feature_desc variable in the LIME explanations
extract_bounds <- function(feature, feature_desc) {
  data.frame(feature = feature, 
             feature_desc = feature_desc) %>%
    tidyr::separate(feature_desc, c("other", "upper"), sep = " <= ") %>%
    tidyr::separate(other, c("lower", "feature2"), sep = " < ") %>%
    mutate(upper = ifelse(is.na(upper), "Inf", upper)) %>%
    select(-feature2) %>%
    mutate_at(.vars = c("lower", "upper"), .funs = as.numeric) %>%
    mutate(lower = ifelse(is.na(lower), "-Inf", lower))
}