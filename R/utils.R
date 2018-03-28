#' Test whether a string has been transformed.
#' @param x Character.
is_transformed <- function(x) grepl(pattern = "^.+\\(.+\\)$", x)


#' Capture a single group from a string.
#' @param x Character.
#' @param pattern Character Scalar.
#' @return Character Scalar.
#' @importFrom utils strcapture
capture <- function(x, pattern) {
  unlist(strcapture(pattern, x, proto = ""),
         use.names = FALSE)
}


#' Extract the transformation function in the variable.
#' @param x Character.
trans_function <- function(x) {
  z <- is_transformed(x)
  ifelse(z, capture(x, "^(.+)\\(.+\\)$"), "identity")
}


#' Extract the (potentially) transformed variable
#' @param x Character.
#' @return Character.
trans_variable <- function(x) {
  z <- is_transformed(x)
  ifelse(z, capture(x, "^.+\\((.+)\\)$"), x)
}


#' Return a specific value, no matter the level of x.
#'
#' This is supposed to be helpful for choosing specific values for
#' your statistic.
#' @param y Numeric value to pick.
#' @return Function with parameter x.
#' @export
at <- function(y) {
  stopifnot(length(y) == 1)
  function(x) y
}
