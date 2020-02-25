context("test-plot_functions")

## Iris Data, Model, and LIME -----------------------------------------

# Iris training and testing
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Random forest model
set.seed(573939903)
model <- randomForest::randomForest(Species ~ .,
                                    data = cbind(iris_train, 
                                                 Species = iris_lab))

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
                                nbins = 2:3,
                                gower_pow = c(0.5, 1),
                                seed = 20190914)

## Tests for Plotting Functions ---------------------------------------

test_that("feature_heatmap", {
  
  # Basic feature heatmap
  fh <- feature_heatmap(iris_lime_explain$explain)
  vdiffr::expect_doppelganger(title = "Basic Heatmap", fig = fh)
  
  # Heatmap with feature_nums specified
  fh_fnumb <- feature_heatmap(iris_lime_explain$explain, feature_nums = 1)
  vdiffr::expect_doppelganger(title = "feature_nums = 1 Heatmap ", fig = fh_fnumb)

})

test_that("metric_plot", {
  
  # Basic metric plot
  mp <- metric_plot(iris_lime_explain$explain)
  vdiffr::expect_doppelganger(title = "Basic Metric Plot", fig = mp)
  
  # Metric plot with with metrics specified
  mp_met <- metric_plot(iris_lime_explain$explain, metrics = "msee")
  vdiffr::expect_doppelganger(title = "metrics = 'MSEE' Metric Plot", fig = mp_met)
  
})


