#' Make confidence intervals data frame.
#' @param fit Model Object.
#' @param newdata Data Frame.
#' @param alpha Numeric Scalar.
#' @export
make_confidence_intervals <- function(fit, newdata, alpha, ...) {
  UseMethod("make_confidence_intervals")
}


#' Make prediction intervals data frame.
#' @param fit Model Object.
#' @param newdata Data Frame.
#' @param alpha Numeric Scalar.
#' @export
make_prediction_intervals <- function(fit, newdata, alpha, ...) {
  UseMethod("make_prediction_intervals")
}


#' Build a subset data frame with columns needed for prediction.
#' @export
prediction_frame <- function(fit, ...) {
  UseMethod("prediction_frame")
}


#' Update model with weights to account for heteroscedasticity.
#' @param fit Model Object.
#' @return Model Object.
#' @export
hetero <- function(fit, ...) {
  UseMethod("hetero")
}


#' Simulate new data from the fitted model objects.
#'
#' This is equivalent to the simulate methods provided for many fitted
#' objects. The only difference is that we want to get predictions for
#' new data sets.
#' @param fit Model Object.
#' @return Matrix of simulated data.
realize <- function(fit, newdata, ...) {
  UseMethod("realize")
}
