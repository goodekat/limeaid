library(caret)

# Split up the data set
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Create Random Forest model on iris data
model <- train(iris_train, iris_lab, method = 'rf')

iris_lime_explain <- apply_lime(train = iris_train,
           test = iris_test,
           model = model,
           label = "virginica",
           n_features = 2,
           sim_method = c('quantile_bins', 'equal_bins', 'kernel_density', 'normal_approx'),
           nbins = 2:4,
           seed = 20190914)

feature_heatmap(iris_lime_explain$explain)
feature_heatmap(iris_lime_explain$explain, feature_nums = 1)
feature_heatmap(iris_lime_explain$explain, feature_nums = 2)
feature_heatmap(iris_lime_explain$explain, feature_nums = 1:2)
feature_heatmap(iris_lime_explain)

feature_heatmap(iris_lime_explain$explain) +
  gretchenalbrecht::scale_fill_gretchenalbrecht(palette = "last_rays", discrete = TRUE)

# set.seed(20190913)
# sine_data <- data.frame(x1 = runif(n = 600, min = 0, max = 20),
#                     x2 = runif(600, min = -2, max = 2),
#                     x3 = rnorm(600)) %>%
#   mutate(y = factor(ifelse(x2 > sin(x1), 1, 0)))
#
# # Separte the data into training and testing parts
# sine_data_train <- sine_data[1:500,]
# sine_data_test <- sine_data[501:600,]
#
# # Fit a random forest
# rfsine <- train(sine_data_train %>% select(x1, x2),
#                 sine_data_train %>% pull(y),
#                 method = 'rf')
#
# sine_lime_explain <- apply_lime(train = sine_data_train %>% select(x1, x2),
#                                 test = sine_data_test %>% select(x1, x2),
#                                 model = rfsine,
#                                 label = "1",
#                                 n_features = 2,
#                                 sim_method = c('quantile_bins', 'equal_bins', 'kernel_density', 'normal_approx'),
#                                 nbins = 2:4,
#                                 seed = 20190914)
#
# feature_heatmap(sine_lime_explain$explain, 1)
# feature_heatmap(sine_lime_explain$explain, 2)
