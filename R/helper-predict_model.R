# Function for predictions with randomForest models

# Adding lime model support for randomForest models (not currently available in lime)
predict_model.randomForest <- function(x, newdata, type, ...) {

  # Compute the predictions
  if (type == "raw"){
    res <- predict(x, newdata = newdata, type = "response", ...)
  } else {
    res <- predict(x, newdata = newdata, type, ...)
  }

  # Return the appropriate set of predictions
  switch(
    type,
    raw = data.frame(Response = res, stringsAsFactors = FALSE),
    prob = as.data.frame(res)
  )
}
