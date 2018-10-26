#' Square function.
square <- function(x) x*x


#' Inverse function.
inverse <- function(x) 1/x


#' Return the function corresponding to the inverse of the specified link function.
#'
#' TODO: family(fit)$linkinv get's you this.
#' 
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
  trans <- family(fit)$linkinv
  pred <- predict(fit, newdata, type = "link", se.fit = TRUE)
  z <- qnorm(1 - alpha/2)
  lwr  <- trans(pred$fit - z*pred$se.fit)
  best <- trans(pred$fit)
  upr  <- trans(pred$fit + z*pred$se.fit)
  res <- data.frame(.fit = best, .lower = lwr, .upper = upr)
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


#' Make data frame with prediction intervals for GLM objects.
#'
#' Predictions about uncertainty in actually realized data as opposed
#' to mean.
#'
#' Unfortunately prediction intervals on GLM's are a more advanced
#' topic. In the case of Gaussian, Gamma, and Poisson families, a
#' prediction interval means something. For logistic regression, not
#' so much.
#'
#' Simon Wood suggests that a reasonable approach to generating
#' prediction intervals in these cases may be to use posterior
#' simulation. So that's what's implemented here except for the case
#' of Gaussian identity GLMs.
#' @param fit Object.
#' @param newdata Data Frame.
#' @param alpha Numeric Scalar.
#' @return Data Frame.
#' @export
make_prediction_intervals.glm <- function(fit,
                                          newdata = fit$model,
                                          alpha = 0.05,
                                          nsim = 30000) {

  stopifnot(alpha > 0, alpha < 1)
  fm <- family(fit)
  ms <- newdata
  if(fm$family == "gaussian" && fm$link == "identity") {
    s    <- sigma(fit)
    pred <- predict(fit, newdata = ms,
                    se.fit = TRUE)
    res  <- as.data.frame(pred)
    z <- qnorm(1 - alpha/2)
    ivl <- with(pred, sqrt(se.fit^2 + s^2))
    lwr  <- with(pred, fit - z*ivl)
    best <- pred$fit
    upr  <- with(pred, fit + z*ivl)
    res <- data.frame(.fit  = best, .lower = lwr, .upper = upr)
    return(cbind(ms, res))
  } else {
    ftd <- predict(fit, newdata = ms, type = "response")
    if(fm$family == "Gamma" && fm$link == "log") {
      shp <- MASS::gamma.shape(fit)$alpha
      lwr <- qgamma(alpha/2,   shape = shp, rate = shp/ftd)
      upr <- qgamma(1-alpha/2, shape = shp, rate = shp/ftd)
      return(cbind(ms, data.frame(lwr, fit = ftd, upr)))
    }
    if(fm$family == "binomial") {
      stop("Binomial prediction intervals are not meaningful. Not implemented!")
    }
    sim <- realize(fit, newdata = ms, nsim)
    pis <- apply(sim, 1, function(x) quantile(x, probs = c(alpha/2, 1-alpha/2)))
    ivl <- as.data.frame(cbind(fit = as.matrix(ftd), t(pis)))
    names(ivl) <- c(".fit", ".lower", ".upper")
    return(cbind(ms, ivl))
  }
}


#' Realize new values from a fitted GLM model.
#' @param fit Model Object.
#' @param newdata Data Frame of X predictors.
#' @param nsim Integer Scalar number of posterior simulations.
#' @return Matrix of realizations.
#' @export
realize.glm <- function(fit,
                        newdata = fit$model,
                        nsim = 30000) {
  fm <- family(fit)
  if(fm$family == "poisson" && fm$link == "log") {
    wts <- fit$prior.weights
    if (any(wts != 1)) {
      warning("ignoring prior weights")
    }
    ftd <- predict(fit, newdata, type = "response")
    sim <- rpois(nsim * length(ftd), ftd)
    return(matrix(sim, ncol = nsim))
  }
  if(fm$family == "Gamma" && fm$link == "log") {
    wts <- fit$prior.weights
    if (any(wts != 1)) {
      stop("weighted glm regression not implemented yet!")
    }
    ftd <- predict(fit, newdata, type = "response")
    shape <- MASS::gamma.shape(fit)$alpha
    sim <- rgamma(nsim * length(ftd), shape = shape, rate = shape/ftd)
    return(matrix(sim, ncol = nsim))
  }
  if(fm$family == "gaussian" && fm$link == "identity") {
    vars <- deviance(fit)/df.residual(fit)
    if (!is.null(fit$weights)) {
      vars <- vars/fit$weights
    }
    ftd <- predict(fit, newdata, type = "response")
    return(replicate(nsim, ftd + rnorm(n = nrow(newdata), sd = sqrt(vars))))
  }
  stop("GLM family and link not implemented yet!")
}
