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


# Generate the data
l1 <- 0
u1 <- 20
l2 <- -2
u2 <- 2
set.seed(20190913)
sine_data <- data.frame(x1 = runif(n = 600, min = l1, max = u1),
                        x2 = runif(600, min = l2, max = u2)) %>%
  mutate(y = factor(ifelse(x2 > sin(x1), 1, 0)))

# Separte the data into training and testing parts
sine_data_train <- sine_data[1:500,]
sine_data_test <- sine_data[501:600,]

# Fit a random forest
# rfsine <- randomForest(x = sine_data_train %>% select(x1, x2),
#                        y = sine_data_train %>% pull(y))
rfsine <- train(x = sine_data_train %>% select(x1, x2),
                y = sine_data_train %>% pull(y),
                method = "rf")

# Obtain predictions on the training and testing data
sine_data_train$rfpred <- predict(rfsine)
sine_data_test$rfpred <- predict(rfsine, sine_data_test %>% select(x1, x2))

# Apply lime with various input options
sine_lime_explain <- apply_lime(
  train = sine_data_train %>% select(x1, x2),
  test = sine_data_test %>% select(x1, x2),
  model = rfsine,
  label = "1",
  n_features = 2,
  sim_method = c('quantile_bins', 'equal_bins',
                 'kernel_density', 'normal_approx'),
  nbins = 2:6,
  seed = 20190914)

feature_heatmap(sine_lime_explain$explain)
compare_limes(sine_lime_explain$explain)
plot_compare(sine_lime_explain$explain)

