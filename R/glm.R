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


#' Make data frame with confidence intervals on means for GAM objects.
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


#' Build mesh around a glm fitted object for a particular numeric
#' column.
#' @param fit Object.
#' @param x Character column name or column integer position
#'   corresponding to a numeric column.
#' @param statistic Function or list of Functions for different named
#'   numeric columns.
#' @param n Integer Scalar mesh points.
#' @param .max_rows Integer Scalar maximum allowed rows in mesh.
build_mesh.glm <- function(fit, x = cidx[1],
                           statistic = mean,
                           n = 1000,
                           step_size = 1L,
                           .max_rows = 1e6) {
  mdl <- fit$model[,-1,drop=FALSE]
  nm <- names(mdl)
  nm_numeric <- names(Filter(is.numeric, mdl))
  if(is.function(statistic)) {
    statistic <- setNames(rep(list(statistic), length(nm_numeric)),
                          nm_numeric)
  }

  cidx <- which(nm %in% nm_numeric)
  stopifnot(length(cidx) > 0)
  if(is.character(x)) {
    x <- which(names(mdl) == x)
  }

  rs <- lapply(seq_along(mdl), function(i) {
    nm[i]
    col <- mdl[,i,drop=TRUE]
    cls <- class(col)
    if(cls == "numeric") {
      if(i == x) {
        r <- range(col)
        return(from = seq(min(r), to = max(r), length.out = n))
      } else {
        return(statistic[[nm[i]]](col))
      }
    }
    if(cls == "integer") {
      if(i == x) {
        r <- range(col)
        return(seq.int(from = min(r), to = max(r), by = step_size))
      } else {
        return(quantile(col, probs = 0.5, type = 3, names = FALSE))
      }
    }
    if(cls == "factor") {
      if(i == x) stop("x must be numeric or integer!")
      lvl <- levels(col)
      return(factor(lvl, lvl))
    }
    stop("class not implemented!", cls, "\n")
  })

  names(rs) <- names(mdl)
  nr <- prod(unlist(lapply(rs, length)))
  if(nr > .max_rows) {
    stop("Too fine a mesh!", n)
  }
  expand.grid(rs)
}
