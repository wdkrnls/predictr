#' Data frame with all variables needed for prediction.
#' @param fit Object.
#' @return Data Frame of prediction variables.
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
