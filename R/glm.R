#' Square function.
square <- function(x) x*x


#' Inverse function.
inverse <- function(x) 1/x


#' Return the function corresponding to the inverse of the specified link function.
#' @param x Function.
#' @return Function.
#' @export
inverse_link <- function(x) {
  stopifnot(length(x) == 1)
  if(is.function(x)) x <- deparse(substitute(x))
  switch(x,
         log = exp,
         logit = plogis,
         probit = pnorm,
         sqrt = square,
         identity = identity,
         inverse = inverse,
         stop("Unknown link function"))
}


#' Make data frame with confidence intervals on means for GAM and GLM objects.
#' @param fit Object.
#' @param newdata Data Frame.
#' @param alpha Numeric Scalar.
#' @return Data Frame.
#' @export
make_confidence_intervals.glm <- function(fit,
                                          newdata = fit$model,
                                          alpha = 0.05) {
  stopifnot(alpha > 0, alpha < 1)
  link <- fit$family$link
  trans <- inverse_link(link)
  pred <- predict(fit, newdata, type = "link", se.fit = TRUE)
  z <- qnorm(1 - alpha/2)
  lwr  <- trans(pred$fit - z*pred$se.fit)
  best <- trans(pred$fit)
  upr  <- trans(pred$fit + z*pred$se.fit)
  res <- data.frame(.lower = lwr, .estimate  = best, .upper = upr)
  cbind(newdata, res)
}


#' Data frame with all variables needed for prediction.
#' @param fit Object.
#' @export
prediction_frame.glm <- function(fit, untransform = TRUE) {
  pc <- fit$model[,-1,drop=FALSE]
  wcol <- grepl(x = names(pc), pattern = "^\\(weights\\)$")
  pc <- pc[,!wcol, drop=FALSE]
  tf <- lapply(sapply(names(pc), trans_function), inverse_link)
  bt <- lapply(seq_along(pc), FUN = function(i) tf[[i]](pc[[i]]))
  names(bt) <- trans_variable(names(pc))
  as.data.frame(bt)
}
