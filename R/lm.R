#' Data frame with all variables needed for prediction.
#' @param fit Object.
#' @return Data Frame of prediction variables.
#' @export
prediction_frame.lm <- function(fit, untransform = TRUE) {
  pc <- fit$model[,-1,drop=FALSE] # drop response
  wcol <- grepl(x = names(pc), pattern = "^\\(weights\\)$")
  pc <- pc[,!wcol, drop=FALSE]
  tf <- lapply(sapply(names(pc), trans_function), inverse_link)
  bt <- lapply(seq_along(pc), FUN = function(i) tf[[i]](pc[[i]]))
  names(bt) <- trans_variable(names(pc))
  as.data.frame(bt)
}



#' Heteroscedastic version of the fit.
#' @param fit Model.
#' @param knots Integer Scalar number of spline knots when estimating
#' @importFrom mgcv gam s
#' @export
hetero.lm <- function(fit, knots = 15) {
  stopifnot(all(fit$weights == 1))
  ares <- abs(resid(fit))
  pred <- fitted(fit)
  hvar <- gam(ares ~ s(pred, k = knots, bs = "cr"))
  res  <- fitted(hvar)
  update(fit, weights = 1/res^2)
}

lm_rname = c("lwr" = ".lower", "upr" = ".upper", "fit" = ".fit")

#' Make data frame with confidence intervals on means for LM objects.
#' @param fit Object.
#' @param newdata Data Frame.
#' @param alpha Numeric Scalar.
#' @return Data Frame.
#' @export
make_confidence_intervals.lm <- function(fit,
                                         newdata = fit$model,
                                         alpha = 0.05) {
  stopifnot(alpha > 0, alpha < 1)
  pred <- predict(fit, newdata,
                  interval = "confidence",
                  type = "response",
                  level = 1 - alpha)
  res  <- as.data.frame(pred)
  names(res) <- recode(names(res), lookup = lm_rname)
  cbind(newdata, res)
}


#' Make data frame with prediction intervals on means for LM objects.
#'
#' Predictions about uncertainty in actually realized data as opposed
#' to mean.
#' @param fit Object.
#' @param newdata Data Frame.
#' @param alpha Numeric Scalar.
#' @return Data Frame.
#' @export
make_prediction_intervals.lm <- function(fit,
                                         newdata = fit$model,
                                         alpha = 0.05) {
  stopifnot(alpha > 0, alpha < 1)
  pred <- predict(fit, newdata,
                  interval = "prediction",
                  type = "response",
                  level = 1 - alpha)
  res  <- as.data.frame(pred)
  names(res) <- recode(names(res), lookup = lm_rname)
  cbind(newdata, res)
}
