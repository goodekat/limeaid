# Code for creating sine classifier data

# Functions for rotating the data
rot_x <- function(x, y, theta) (x * cos(theta)) - (y * sin(theta))
rot_y <- function(x, y, theta) (x * sin(theta)) + (y * cos(theta))

# Parameters for data creation
theta = -0.9
min = -10
max = 10

# Generate the data
set.seed(20190913)
sine_data <- data.frame(x1 = runif(600, min, max),
                        x2 = sort(runif(600, min, max)),
                        x3 = rnorm(600)) %>%
  mutate(x1new = rot_x(x1, x2, theta),
         x2new = rot_y(x1, x2, theta),
         y = ifelse(x2new > 5 * sin(x1new), "1", "0"))

# Save the data
#usethis::use_data(sine_data, overwrite = TRUE)
