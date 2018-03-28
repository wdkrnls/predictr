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


#' Build prediction mesh around fitted object.
#' @param fit Model Object.
#' @export
build_mesh <- function(fit, ...) {
  UseMethod("build_mesh")
}
