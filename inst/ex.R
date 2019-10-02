library(caret)
library(dplyr)

# Split up the data set
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Create Random Forest model on iris data
model <- train(iris_train, iris_lab, method = 'rf')

iris_lime_explain <- apply_limes(train = iris_train,
           test = iris_test,
           model = model,
           label = "virginica",
           n_features = 2,
           sim_method = c('quantile_bins',
                          'equal_bins',
                          'kernel_density',
                          'normal_approx'),
           nbins = 2:6,
           seed = 20190914)

feature_heatmap(iris_lime_explain$explain)
feature_heatmap(iris_lime_explain$explain, feature_nums = 1)
feature_heatmap(iris_lime_explain$explain, feature_nums = 2)
feature_heatmap(iris_lime_explain$explain, feature_nums = 1:2)
feature_heatmap(iris_lime_explain)
feature_heatmap(iris_lime_explain$explain) +
  gretchenalbrecht::scale_fill_gretchenalbrecht(
    palette = "last_rays", discrete = TRUE)

compare_limes(iris_lime_explain$explain)

plot_compare(iris_lime_explain$explain)
plot_compare(iris_lime_explain$explain, metrics = "msee")

iris_lime <- lime::lime(x = iris_train, model = model)
iris_myexplain <- myexplain(x = iris_test, explainer = iris_lime,
                            labels = "versicolor", n_features = 2)

# Separte the data into training and testing parts
rs <- sample(1:600, 500, replace = FALSE)
sine_data_train <- sine_data[rs,]
sine_data_test <- sine_data[-rs,]

# Fit a random forest
library(randomForest)
rfsine <- randomForest(x = sine_data_train %>% select(x1, x2, x3),
                       y = sine_data_train %>% pull(y) %>% factor())
rfsine <- train(x = sine_data_train %>% select(x1, x2, x3),
                y = sine_data_train %>% pull(y),
                method = "rf")

# Obtain predictions on the training and testing data
sine_data_train$rfpred <- predict(rfsine)
sine_data_test$rfpred <- predict(rfsine, sine_data_test %>% select(x1, x2, x3))

# Apply lime with various input options
sine_lime_explain <- apply_limes(
  train = sine_data_train %>% select(x1, x2, x3),
  test = sine_data_test %>% select(x1, x2, x3),
  model = lime::as_classifier(rfsine),
  label = "1",
  n_features = 2,
  sim_method = c('quantile_bins', 'equal_bins',
                 'kernel_density', 'normal_approx'),
  nbins = 3:4,
  seed = 20190914)

feature_heatmap(sine_lime_explain$explain)
compare_limes(sine_lime_explain$explain)
plot_compare(sine_lime_explain$explain)

