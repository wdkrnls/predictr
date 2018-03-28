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
  UseMethod("pred_columns")
}
