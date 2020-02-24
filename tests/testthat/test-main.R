context("test-main_functions")

# Load packages
library(dplyr)

## Iris Data and Model ------------------------------------------------

# Iris training and testing
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Two versions of a random forest model
model_caret <- caret::train(x = iris_train, y = iris_lab, method = "rf")
model_randomForest <- 
  randomForest::randomForest(Species ~ .,
                             data = cbind(iris_train, 
                                          Species = iris_lab))

## Applications of LIME -----------------------------------------------

# Run apply_lime on the iris data
iris_lime_explain <- apply_lime(train = iris_train,
                                test = iris_test,
                                model = model_caret,
                                label = "virginica",
                                n_features = 2,
                                sim_method = c('quantile_bins',
                                               'equal_bins',
                                               'kernel_density',
                                               'normal_approx'),
                                nbins = 2:3,
                                gower_pow = c(0.5, 1),
                                seed = 20190914)

# Run apply_lime on the iris data with the randomForest model
iris_lime_explain_randomForest <- apply_lime(train = iris_train,
                                test = iris_test,
                                model = model_randomForest,
                                label = "virginica",
                                n_features = 2,
                                sim_method = c('quantile_bins',
                                               'equal_bins',
                                               'kernel_density',
                                               'normal_approx'),
                                nbins = 2:3,
                                seed = 20190914)

# Run apply_lime on the iris data again to check see
iris_lime_explain2 <- apply_lime(train = iris_train,
                                test = iris_test,
                                model = model_caret,
                                label = "virginica",
                                n_features = 2,
                                sim_method = c('quantile_bins',
                                               'equal_bins',
                                               'kernel_density',
                                               'normal_approx'),
                                nbins = 2:3,
                                gower_pow = c(0.5, 1),
                                seed = 20190914)

# Run apply_lime on the iris data with all feature selection methods
iris_lime_explain_fs <- apply_lime(train = iris_train,
                                   test = iris_test,
                                   model = model_caret,
                                   label = "virginica",
                                   n_features = 2,
                                   sim_method = c('quantile_bins',
                                                  'equal_bins',
                                                  'kernel_density',
                                                  'normal_approx'),
                                   nbins = 2:3,
                                   all_fs = TRUE,
                                   label_fs = "versicolor",
                                   seed = 20190914)

## Comparisons of LIME ------------------------------------------------

# Compute metrics
metrics <- compute_metrics(iris_lime_explain$explain)
metrics_sub <- compute_metrics(iris_lime_explain$explain,
                               metrics = c("msee", "ave_fidelity"))

## Tests of Main Functions --------------------------------------------

# Test that apply_lime produces output with the correct structure
test_that("apply_lime", {

  # Check the structure is correct
  testthat::expect_type(iris_lime_explain$lime, "list")
  testthat::expect_true(tibble::is_tibble(iris_lime_explain$explain))
  testthat::expect_type(iris_lime_explain_fs$lime, "list")
  testthat::expect_true(tibble::is_tibble(iris_lime_explain_fs$explain))
  
  # Check that the implementation with the randomForest model ran
  testthat::expect_type(iris_lime_explain_randomForest, "list")

  # Check that the lengths and dimensions are correct
  testthat::expect_equal(length(iris_lime_explain$lime), 12)
  testthat::expect_equal(length(iris_lime_explain_fs$lime), 6)
  testthat::expect_equal(dim(iris_lime_explain$explain), c(120, 22))
  testthat::expect_equal(dim(iris_lime_explain_fs$explain), c(60, 26))

  # Check that the seed is working
  testthat::expect_true(all.equal(iris_lime_explain$lime, 
                                  iris_lime_explain2$lime))
  testthat::expect_true(identical(iris_lime_explain$explain, 
                                  iris_lime_explain2$explain))
  
})

# Test that compute_metrics produces output with the correct structure
test_that("compute_metrics", {
  
  # Check the type of output
  testthat::expect_true(is.data.frame(metrics))
  testthat::expect_true(is.data.frame(metrics_sub))
  
  # Check the dimensions
  testthat::expect_equal(dim(metrics), c(12, 7))
  testthat::expect_equal(dim(metrics_sub), c(12, 6))
  
  # Check that the correct metrics are output
  testthat::expect_true("ave_r2" %in% names(metrics))
  testthat::expect_true("msee" %in% names(metrics))
  testthat::expect_true("ave_fidelity" %in% names(metrics))
  testthat::expect_false("ave_r2" %in% names(metrics_sub))
  testthat::expect_true("msee" %in% names(metrics_sub))
  testthat::expect_true("ave_fidelity" %in% names(metrics_sub))
  
})
