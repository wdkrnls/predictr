fast_ci_plot <- function(data, x = nm[1], ..., ribbon_alpha = 0.3) {
  nm <- names(Filter(f = function(x) is.numeric(x) && length(x) > 1, data))
  ggplot(data, mapping = aes_string(x, ".estimate", ...)) +
}


fast_gg <- function(data, ...) {
  nm <- names(Filter(f = function(x) is.numeric(x) && length(x) > 1, data))
  ggplot(data, mapping = aes_string(x, ".estimate"), ...)
}


fast_ci <- function(line_args = list(),
                    ribbon_args = list(mapping = aes(ymin = .lower, ymax = .upper),
                                       alpha = 0.3)) {
  list(do.call(geom_line, line_args),
       do.call(geom_ribbon, ribbon_args))
}
