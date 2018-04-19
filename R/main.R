#' Build mesh around a glm fitted object for a particular numeric
#' column.
#' @param fit Object.
#' @param x Character column name or column integer position
#'   corresponding to a numeric column.
#' @param at List with levels to include for named columns.
#' @param statistic Function or list of Functions for different named
#'   numeric columns.
#' @param n Integer Scalar mesh points.
#' @param .max_rows Integer Scalar maximum allowed rows in mesh.
#' @export
build_mesh <- function(fit, x = cidx[1],
                       at = NULL,
                       statistic = mean,
                       n = 1000,
                       step_size = 1L,
                       .max_rows = 1e6) {
  mdl <- prediction_frame(fit)
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
    col <- mdl[,i,drop=TRUE]
    cls <- class(col)
    if(cls == "numeric") {
      if(i == x) {
        r <- range(col)
        return(from = seq(min(r), to = max(r), length.out = n))
      } else {
        if(nm[i] %in% names(at)) {
          return(at[[nm[i]]])
        }
        return(statistic[[nm[i]]](col))
      }
    }
    if(cls == "integer") {
      if(i == x) {
        r <- range(col)
        return(seq.int(from = min(r), to = max(r), by = step_size))
      } else {
        if(nm[i] %in% names(at)) {
          return(at[[nm[i]]])
        }
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
