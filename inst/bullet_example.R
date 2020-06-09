# Load the hamby data
bullet_train <- read.csv("../data/hamby173and252_train.csv")
bullet_test <- read.csv("../data/hamby224_test.csv")

# Extract the features and order them based on feature importance
bullet_features <- rownames(bulletxtrctr::rtrees$importance)

# Apply LIME to two cases with the permutations returned
bullet_lime_explain_perms <- apply_lime(
  train = bullet_train %>% select(all_of(bullet_features)),
  test = bullet_test %>% 
    filter(case %in% c(325, 20)) %>%
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

plot_expl_scatter(bullet_explain_perms[1:3,])
lime::plot_features(bullet_explain_perms[4:6,])
plot_expl_scatter(bullet_explain_perms[4:6,])
plot_expl_scatter(bullet_explain_perms[7:9,])

explanation = bullet_explain_perms %>% 
  filter(case == 1, sim_method == "equal_bins", nbins == 3)

plot_expl_scatter(explanation)
lime::plot_features(explanation)



