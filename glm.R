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
         sqrt = square,
         identity = identity,
         inverse = inverse,
         stop("Unknown link function"))
}


#' Make confidence intervals data frame for GAMs.
#' @param fit Object.
#' @param newdata Data Frame.
#' @param alpha Numeric Scalar.
#' @return Data Frame.
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
