context("test-apply_lime")

# Load packages
library(dplyr)

## Iris Data Example ----------------------------------------------------

# Iris training and testing
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Iris random forest
model <- caret::train(x = iris_train, y = iris_lab, method = "rf")

# Run apply_lime on the iris data
iris_lime_explain <- apply_lime(train = iris_train,
                                test = iris_test,
                                model = model,
                                label = "virginica",
                                n_features = 2,
                                sim_method = c('quantile_bins',
                                               'equal_bins',
                                               'kernel_density',
                                               'normal_approx'),
                                n_bins = 2:3,
                                seed = 20190914)

# Test that the output has not changed
test_that("apply_lime-output", {

  testthat::expect_type(iris_lime_explain$lime, "list")
  testthat::expect_type(iris_lime_explain$explain, "list")

})
