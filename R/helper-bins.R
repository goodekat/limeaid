## Helper functions for working with bins

# Function for extracting the numeric bounds from the 
# feature_desc variable in the LIME explanations
extract_bounds <- function(feature, feature_desc) {
  data.frame(feature = feature, 
             feature_desc = feature_desc) %>%
    tidyr::separate(.data$feature_desc, c("other", "upper"), sep = " <= ") %>%
    tidyr::separate(.data$other, c("lower", "feature2"), sep = " < ") %>%
    dplyr::mutate(upper = ifelse(is.na(.data$upper), "Inf", .data$upper)) %>%
    dplyr::select(-.data$feature2) %>%
    dplyr::mutate_at(.vars = c("lower", "upper"), .funs = as.numeric) %>%
    dplyr::mutate(lower = ifelse(is.na(.data$lower), "-Inf", .data$lower))
}