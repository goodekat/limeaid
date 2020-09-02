
# limeaid <img align="right" width="150" height="170" src="inst/limeaid-sticker2.png">

limeaid is an R package for assessing explanations created using the R
package [lime](https://lime.data-imaginist.com/). The current
implementation was developed to be used with classification models with
a binary response and continuous features.

``` r
# Installation instructions 
devtools::install_github("goodekat/limeaid")
```

``` r
library(cowplot)
library(lime)
library(limeaid)
library(randomForest)

# Iris training and testing
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Fit a random forest model to the iris training data
set.seed(20200334)
rf <- randomForest(Species ~ .,
                   data = cbind(iris_train, 
                                Species = iris_lab))

# Run apply_lime on the iris data
lime_applied <- apply_lime(train = iris_train,
                           test = iris_test,
                           model = rf,
                           label = "virginica",
                           n_features = 2,
                           sim_method = c('quantile_bins',
                                          'equal_bins',
                                          'kernel_density'),
                           nbins = 2:4, 
                           gower_pow = c(1, 5),
                           return_perms = TRUE, 
                           seed = 20200334)

# Extract the explanations from the apply_lime output
explanations <- lime_applied$explain
```

``` r
# Create a heatmap of the features chosen
plot_feature_heatmap(explanations)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# Create another heatmap but order the features using a seriation method
plot_feature_heatmap(explanations, order_method = "PCA")
```

![](README_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
# Create a plot of metrics for comparing the lime implementations
plot_metrics(explanations, add_lines = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Plots of several explanations of interest (2-4 quantile bins)
plot_grid(plot_explain_scatter(explanations[1:2,]),
          plot_explain_scatter(explanations[11:12,]),
          plot_explain_scatter(explanations[21:22,]),
          nrow = 1)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# Plots of several explanations of interest (2-4 equal bins)
plot_grid(plot_explain_scatter(explanations[31:32,]),
          plot_explain_scatter(explanations[41:42,]),
          plot_explain_scatter(explanations[51:52,]),
          nrow = 1)
```

![](README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
# Plots of an explanation of interest (kernel density)
plot_explain_scatter(explanations[121:122,])
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
