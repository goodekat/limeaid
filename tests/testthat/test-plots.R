context("test-plot_functions")

## Iris Data, Model, and LIME -----------------------------------------

# Prepare training and testing data
x_train = sine_data_train[c("x1", "x2", "x3")]
y_train = factor(sine_data_train$y)
x_test = sine_data_test[1:5, c("x1", "x2", "x3")]
y_test = factor(sine_data_test$y)[1:5]

# Fit a random forest to the sine training data
set.seed(573939903)
rf <- randomForest::randomForest(x = x_train, y = y_train)

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
                 return_perms = TRUE,
                 seed = 20190914)

## Tests for Plotting Functions ---------------------------------------

test_that("plot_feature_heatmap", {
  
  # Basic feature heatmap
  fh1 <- plot_feature_heatmap(le$explain)
  vdiffr::expect_doppelganger(title = "Basic heatmap", fig = fh1)
  
  # Heatmap with feature_nums and facet_var specified
  fh2 <- plot_feature_heatmap(le$explain, feature_nums = 1, facet_var = y_test)
  vdiffr::expect_doppelganger(title = "Heatmap with feature_nums and facet_var", fig = fh2)
  
  # Heatmaps with orderings
  fh3 <- plot_feature_heatmap(le$explain, order_method = "obs_num")
  vdiffr::expect_doppelganger(title = "Heatmap ordered by obs_num", fig = fh3)
  fh4 <- plot_feature_heatmap(le$explain, order_method = "sort_features")
  vdiffr::expect_doppelganger(title = "Heatmap ordered by sort_features", fig = fh4)
  fh5 <- plot_feature_heatmap(le$explain, order_method = "PCA")
  vdiffr::expect_doppelganger(title = "Heatmap ordered by PCA", fig = fh5)
  testthat::expect_error(plot_feature_heatmap(le$explain, order_method = "other"))

})

test_that("plot_metrics", {
  
  # Basic metric plot
  mp <- plot_metrics(le$explain)
  vdiffr::expect_doppelganger(title = "Basic Metric Plot", fig = mp)
  
  # Metric plot with with metrics specified
  mp_met <- plot_metrics(le$explain, metrics = "msee")
  vdiffr::expect_doppelganger(title = "metrics = 'MSEE' Metric Plot", fig = mp_met)
  
})

test_that("plot_expl_scatter", {
  
  # Basic eoi plot
  eoip <- plot_expl_scatter(explanation = le$explain[1:2,])
  vdiffr::expect_doppelganger(title = "Explanation Plot", 
                              fig = eoip)
  
  # eoi plot with all options
  eoi_opt <- plot_expl_scatter(explanation = le$explain[1:2,], 
                      alpha = 0.5, 
                      bins = FALSE, 
                      weights = TRUE, 
                      title.opt = FALSE)
  vdiffr::expect_doppelganger(title = "Explanation Plot with Options", 
                              fig = eoi_opt)
  
})
