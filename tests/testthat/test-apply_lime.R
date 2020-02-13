context("test-apply_lime")

# Load packages
library(dplyr)
library(randomForest)

## Iris Data Example --------------------------------------------------------

# Iris training and testing
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Iris random forest
set.seed(20190913)
model <- randomForest(iris_train, iris_lab)

# Run apply_lime on the iris data
iris_lime_explain <- apply_limes(train = iris_train,
                                test = iris_test,
                                model = lime::as_classifier(model),
                                label = "virginica",
                                n_features = 2,
                                sim_method = c('quantile_bins',
                                               'equal_bins',
                                               'kernel_density',
                                               'normal_approx'),
                                nbins = 2:3,
                                seed = 20190914)

# Saved on Sept 20, 2019
#saveRDS(iris_lime_explain, "./inst/extdata/iris_lime_explain.rds")

## Generated Sine Data Example ----------------------------------------------

# Generate the data
set.seed(20190913)
sine_data <- data.frame(x1 = runif(n = 600, min = 0, max = 20),
                        x2 = runif(n = 600, min = -2, max = 2),
                        x3 = rnorm(n = 600)) %>%
  mutate(y = factor(ifelse(x2 > sin(x1), 1, 0)))

# Separte the data into training and testing parts
sine_data_train <- sine_data[1:500,]
sine_data_test <- sine_data[501:600,]

# Fit a random forest
set.seed(20190913)
rfsine <- randomForest(x = sine_data_train %>% select(x1, x2, x3),
                       y = sine_data_train %>% pull(y))

# Obtain predictions on the training and testing data
sine_data_train$rfpred <- predict(rfsine)
sine_data_test$rfpred <- predict(rfsine,
                                 sine_data_test %>% select(x1, x2, x3))

# Apply lime with various input options
sine_lime_explain <- apply_limes(
  train = sine_data_train %>% select(x1, x2, x3),
  test = sine_data_test %>% select(x1, x2, x3),
  model = rfsine,
  label = "1",
  n_features = 2,
  sim_method = c('quantile_bins',
                 'kernel_density'),
  nbins = 3:4,
  seed = 20190914)

# Saved on Sept 20, 2019
#saveRDS(sine_lime_explain, "./inst/extdata/sine_lime_explain.rds")

## apply_lime Output Test ---------------------------------------------------

# Test that the output has not changed
test_that("apply_lime-output", {

  # Load in the saved explanations
  saved_iris_lime_explain =
    readRDS(system.file("extdata", "iris_lime_explain.rds", package = "limeaid"))
  saved_sine_lime_explain =
    readRDS(system.file("extdata", "sine_lime_explain.rds", package = "limeaid"))

  # Check that the explanations have not changed
  testthat::expect_identical(iris_lime_explain, iris_lime_explain)
  testthat::expect_identical(sine_lime_explain$explain %>%
                               select(sim_method:label_prob),
                             saved_sine_lime_explain$explain %>%
                               select(sim_method:label_prob))

})
