# Code for creating sine classifier data

# Packages
library(dplyr)

# Functions for rotating the data
rot_x <- function(x, y, theta) (x * cos(theta)) - (y * sin(theta))
rot_y <- function(x, y, theta) (x * sin(theta)) + (y * cos(theta))

# Generate the data
theta = -0.9
min = -10
max = 10
set.seed(20190913)
sine_data <- data.frame(x1 = runif(600, min, max),
                        x2 = sort(runif(600, min, max))) %>%
  mutate(x1new = rot_x(x1, x2, theta),
         x2new = rot_y(x1, x2, theta),
         y = factor(ifelse(x2new > 5 * sin(x1new), "1", "0"))) %>%
  slice(sample(1:n())) %>%
  mutate(x3 = rnorm(600),
         case = 1:600) %>%
  select(case, everything())

# Separte the data into training and testing parts
set.seed(20191003)
rs <- sample(1:600, 500, replace = FALSE)
sine_data_train <- sine_data[rs,]
sine_data_test <- sine_data[-rs,]

# Save the data
#usethis::use_data(sine_data_train, overwrite = TRUE)
#usethis::use_data(sine_data_test, overwrite = TRUE)
