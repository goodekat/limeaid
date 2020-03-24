context("test-main_functions")

## Data and Model -----------------------------------------------------

# Prepare training and testing data
x_train = sine_data_train[c("x1", "x2", "x3")]
y_train = factor(sine_data_train$y)
x_test = sine_data_test[1:5, c("x1", "x2", "x3")]
y_test = factor(sine_data_test$y)[1:5]

# Fit a random forest to the sine training data
rf <- randomForest::randomForest(x = x_train, y = y_train)

## Applications of LIME -----------------------------------------------

# Run apply_lime
le <- apply_lime(train = x_train,
                 test = x_test,
                 model = rf,
                 label = "1",
                 n_features = 2,
                 sim_method = c('quantile_bins',
                                'kernel_density'),
                 nbins = 2:3,
                 gower_pow = c(0.5, 1),
                 seed = 20190914)

# Run apply_lime again
le_copy <- apply_lime(train = x_train,
                      test = x_test,
                      model = rf,
                      label = "1",
                      n_features = 2,
                      sim_method = c('quantile_bins',
                                     'kernel_density'),
                      nbins = 2:3,
                      gower_pow = c(0.5, 1),
                      seed = 20190914)

# Run apply_lime with extra options specified 
le_extra <- apply_lime(train = x_train,
                       test = x_test,
                       model = rf,
                       label = "1",
                       n_features = 2,
                       sim_method = c('quantile_bins',
                                      'kernel_density'),
                       nbins = 2:3,
                       gower_pow = c(0.5, 1),
                       return_perms = TRUE,
                       all_fs = TRUE,
                       seed = 20190914)

## Comparisons of LIME ------------------------------------------------

# Compute metrics
metrics <- compute_metrics(le$explain)
metrics_sub <- 
  compute_metrics(le$explain, metrics = c("msee", "ave_fidelity"))

## Tests of Main Functions --------------------------------------------

# Test that apply_lime produces output with the correct structure
test_that("apply_lime", {

  # Check the structures are correct
  testthat::expect_type(le$lime, "list")
  testthat::expect_true(tibble::is_tibble(le$explain))
  testthat::expect_type(le_extra$lime, "list")
  testthat::expect_true(tibble::is_tibble(le_extra$explain))
  
  # Check that the lengths and dimensions are correct
  testthat::expect_equal(length(le$lime), 6)
  testthat::expect_equal(dim(le$explain), c(60, 18))
  testthat::expect_equal(length(le_extra$lime), 6)
  testthat::expect_equal(dim(le_extra$explain), c(60, 27))
  
  # Check that the seed is working
  testthat::expect_true(all.equal(le$lime, 
                                  le_copy$lime))
  testthat::expect_true(identical(le$explain$prediction, 
                                  le_copy$explain$prediction))
  
  # Extract the explanations for one case of interest (same simulation
  # method but different gower powers)
  coi0.5 <- le_extra$explain %>% 
    dplyr::filter(case == sine_data_test$case[1], 
                  sim_method == 'quantile_bins', 
                  nbins == 3, 
                  gower_pow == 0.5)
  coi1 <- le_extra$explain %>% 
    dplyr::filter(case == sine_data_test$case[1], 
                  sim_method == 'quantile_bins', 
                  nbins == 3, 
                  gower_pow == 1)
  
  # Check to make sure the simulated dataset match between the 
  # two different gower powers
  testthat::expect_true(identical(coi0.5$perms_raw[[1]], coi1$perms_raw[[1]]))
  
})

# Test that compute_metrics produces output with the correct structure
test_that("compute_metrics", {
  
  # Check the type of output
  testthat::expect_true(is.data.frame(metrics))
  testthat::expect_true(is.data.frame(metrics_sub))
  
  # Check the dimensions
  testthat::expect_equal(dim(metrics), c(6, 7))
  testthat::expect_equal(dim(metrics_sub), c(6, 6))
  
  # Check that the correct metrics are output
  testthat::expect_true("ave_r2" %in% names(metrics))
  testthat::expect_true("msee" %in% names(metrics))
  testthat::expect_true("ave_fidelity" %in% names(metrics))
  testthat::expect_false("ave_r2" %in% names(metrics_sub))
  testthat::expect_true("msee" %in% names(metrics_sub))
  testthat::expect_true("ave_fidelity" %in% names(metrics_sub))
  
})
