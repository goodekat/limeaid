# Join the data and the explanations and edit and add additional variables
# Create the feature bin labels using my function "bin_labeller"
# hamby224_test_explain <- hamby224_test %>%
#   mutate(case = as.character(case)) %>%
#   full_join(hamby224_explain, by = "case") %>%
#   mutate(case = factor(case),
#          feature_desc = factor(feature_desc),
#          feature_bin = pmap_chr(list(feature = feature,
#                                 feature_value = feature_value,
#                                 b_c = bin_continuous,
#                                 q_b = quantile_bins,
#                                 n_b = nbins,
#                                 u_d = use_density,
#                                 b_m = bin_method,
#                                 r_v = response),
#                           .f = bin_labeller, # bin_labeller is one of my helper functions
#                           bin_data = hamby224_bin_boundaries,
#                           case_info = hamby224_lime_inputs)) %>%
#   mutate(feature = factor(feature),
#          nbins = factor(nbins),
#          feature_number = readr::parse_number(as.character(feature_desc)),
#          strictly_less = FALSE) %>%
#   arrange(nbins)
#
# # Finish creating the strictly less than variable
# hamby224_test_explain$strictly_less[grep("< ", hamby224_test_explain$feature_desc)] <- TRUE
#
# # Reorder the variables of feature_desc and feature_bin for plotting purposes and
# # create new variables of situation and bin_situation
# hamby224_test_explain <- hamby224_test_explain %>%
#   mutate(feature_desc = reorder(feature_desc, strictly_less),
#          feature_desc = reorder(feature_desc, feature_number),
#          feature_desc = reorder(feature_desc, as.numeric(feature))) %>%
#   mutate(nbins = as.numeric(as.character(nbins)),
#          situation = ifelse(bin_continuous == TRUE & bin_method == "quantile_bins",
#                             sprintf("%.0f quantile", nbins),
#                             ifelse(bin_continuous == TRUE & bin_method == "equally_spaced",
#                                    sprintf("%.0f equally spaced", nbins),
#                                    ifelse(bin_continuous == TRUE & bin_method == "tree" &
#                                             response == "samesource",
#                                           sprintf("%.0f samesource tree", nbins),
#                                           ifelse(bin_continuous == TRUE & bin_method == "tree" &
#                                             response == "rfscore",
#                                             sprintf("%.0f rfscore tree", nbins),
#                                             ifelse(bin_continuous == FALSE &
#                                                    use_density == TRUE,
#                                                    "kernel density",
#                                                    "normal approximation"))))) %>%
#            fct_relevel("2 quantile", "3 quantile", "4 quantile",
#                        "5 quantile", "6 quantile", "2 equally spaced",
#                        "3 equally spaced", "4 equally spaced",
#                        "5 equally spaced", "6 equally spaced",
#                        "2 samesource tree", "3 samesource tree",
#                        "4 samesource tree", "5 samesource tree",
#                        "6 samesource tree")) %>%
#   mutate(bin_situation = ifelse(bin_method == "quantile_bins" &
#                                 bin_continuous == TRUE,
#                                 "quantile",
#                                 ifelse(bin_method == "equally_spaced" &
#                                        bin_continuous == TRUE,
#                                        "equally spaced",
#                                        ifelse(bin_method == "tree" &
#                                               bin_continuous == TRUE &
#                                               response == "samesource",
#                                               "samesource tree",
#                                               ifelse(bin_method == "tree" &
#                                                      bin_continuous == TRUE &
#                                                      response == "rfscore",
#                                                      "rfscore tree",
#                                                      ifelse(bin_continuous == FALSE &
#                                                      use_density == TRUE,
#                                                      "kernel density",
#                                                      "normal approximation")))))) %>%
#   mutate(bin_situation = factor(bin_situation)) %>%
#   select(situation, bin_situation, bin_continuous:response, case:feature_desc,
#          feature_bin:strictly_less, data, prediction)
