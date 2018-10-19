prop_qt <- function(prop) {
  prop <- 1 - (1 - prop) / 2
  stats::qt(prop, Inf)
}

dot <- function(x) paste0(".", x)

set_na <- function(x) {
  is.na(x) <- TRUE
  x
}
