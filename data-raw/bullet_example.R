# Load the hamby data
bullet_train <- read.csv("../data/hamby173and252_train.csv")
bullet_test <- read.csv("../data/hamby224_test.csv")

# Extract the features and order them based on feature importance
bullet_features <- rownames(bulletxtrctr::rtrees$importance)

# Apply LIME to two cases with the permutations returned
bullet_lime_explain_perms <- apply_lime(
  train = bullet_train %>% select(all_of(bullet_features)),
  test = bullet_test %>% 
    filter(case %in% c(16, 20)) %>%
    select(all_of(bullet_features)),
  model = bulletxtrctr::rtrees,
  label = as.character(TRUE),
  n_features = 3,
  sim_method = c('quantile_bins', 'equal_bins', 'kernel_density', 'normal_approx'),
  nbins = 3,
  feature_select = "auto",
  dist_fun = "gower",
  kernel_width = NULL,
  gower_pow = 0.5,
  return_perms = TRUE,
  all_fs = FALSE,
  seed = 20190914)

# Separate the lime and explain parts of the results
bullet_lime_perms <- bullet_lime_explain_perms$lime
bullet_explain_perms <- bullet_lime_explain_perms$explain

eoi_plot(bullet_explain_perms[1:3,])
eoi_plot(bullet_explain_perms[4:6,])
eoi_plot(bullet_explain_perms[7:9,])
eoi_plot(bullet_explain_perms[10:12,])
eoi_plot(bullet_explain_perms[13:15,])
eoi_plot(bullet_explain_perms[16:18,])
eoi_plot(bullet_explain_perms[19:21,])
eoi_plot(bullet_explain_perms[22:24,])

feature_desc = bullet_explain_perms$feature_desc[1:11]

bin_extrctr <- function(feature_desc) {
  data.frame(feature_desc = feature_desc) %>%
    tidyr::separate(feature_desc, c("other", "upper"), sep = " <= ") %>%
    tidyr::separate(other, c("lower", "feature2"), sep = " < ") %>%
    mutate(upper = ifelse(is.na(upper), "Inf", upper)) %>%
    select(-feature2) %>%
    mutate_at(.vars = c("lower", "upper"), .funs = as.numeric) %>%
    mutate(lower = ifelse(is.na(lower), "-Inf", lower))
}
  








