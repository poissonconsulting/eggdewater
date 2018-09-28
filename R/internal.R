prop_qt <- function(prop) {
  prop <- 1 - (1 - prop) / 2
  stats::qt(prop, Inf)
}