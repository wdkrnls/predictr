#' Test whether a string has been transformed.
is_transformed <- function(x) grepl(pattern = "^.+\\(.+\\)$", x)


#' Capture a single group from a string.
#' @importFrom utils strcapture
capture <- function(x, pattern) {
  unlist(strcapture(pattern, x, proto = ""),
         use.names = FALSE)
}


#' Extract the transformation function in the variable.
trans_function <- function(x) {
  z <- is_transformed(x)
  ifelse(z, capture(x, "^(.+)\\(.+\\)$"), "identity")
}


#' Extract the (potentially) transformed variable
trans_variable <- function(x) {
  z <- is_transformed(x)
  ifelse(z, capture(x, "^.+\\((.+)\\)$"), x)
}

