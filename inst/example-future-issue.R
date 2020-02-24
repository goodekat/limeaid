# random forest model
set.seed(202020)
model <- 
  randomForest::randomForest(y ~ x1 + x2 + x3,
                             data = sine_data_train)

# apply lime with purrr: runs fine!
sine_lime_explain_purrr <-
  apply_lime(train = sine_data_train[c("x1", "x2", "x3")],
             test = sine_data_test[c("x1", "x2", "x3")],
             model = model,
             label = "1",
             n_features = 2,
             sim_method = 'quantile_bins',
             nbins = 4,
             seed = 20190914, 
             gower_pow = 1,
             apply_method = "purrr")

# apply lime with furrr: runs fine!
sine_lime_explain_furrr <-
  apply_lime(train = sine_data_train[c("x1", "x2", "x3")],
             test = sine_data_test[c("x1", "x2", "x3")],
             model = model,
             label = "1",
             n_features = 2,
             sim_method = 'quantile_bins',
             nbins = 4,
             seed = 20190914, 
             gower_pow = 1,
             apply_method = "furrr")

# apply lime with future and furrr: returns an error
sine_lime_explain_future_furrr <-
  apply_lime(train = sine_data_train[c("x1", "x2", "x3")],
             test = sine_data_test[c("x1", "x2", "x3")],
             model = model,
             label = "1",
             n_features = 2,
             sim_method = 'quantile_bins',
             nbins = 4,
             seed = 20190914, 
             gower_pow = 1,
             apply_method = "future_furrr")

# apply lime with purrr (after future has been called): runs fine!
sine_lime_explain_purrr <-
  apply_lime(train = sine_data_train[c("x1", "x2", "x3")],
             test = sine_data_test[c("x1", "x2", "x3")],
             model = model,
             label = "1",
             n_features = 2,
             sim_method = 'quantile_bins',
             nbins = 4,
             seed = 20190914, 
             gower_pow = 1,
             apply_method = "purrr")

# apply lime with furrr (after future has been called): returns an error
sine_lime_explain_furrr <-
  apply_lime(train = sine_data_train[c("x1", "x2", "x3")],
             test = sine_data_test[c("x1", "x2", "x3")],
             model = model,
             label = "1",
             n_features = 2,
             sim_method = 'quantile_bins',
             nbins = 4,
             seed = 20190914, 
             gower_pow = 1,
             apply_method = "furrr")
