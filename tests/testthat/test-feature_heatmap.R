context("test-feature_heatmap")

# Load in the saved explanations
saved_iris_lime_explain =
  read.csv(system.file("extdata", "iris_lime_explain.rds", package = "limeaid"))
saved_sine_lime_explain =
  read.csv(system.file("extdata", "sine_lime_explain.rds", package = "limeaid"))

test_that("validate-feature_heatmap", {
  vdiffr::expect_doppelganger("iris_heatmap",
                              feature_heatmap(saved_iris_lime_explain$explain))
  vdiffr::expect_doppelganger("sine_heatmap",
                              feature_heatmap(saved_sine_lime_explain$explain))
})
