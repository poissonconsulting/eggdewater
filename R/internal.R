calc_atu <- function(start, atu, cumsum) {
  cumsum <- cumsum - start
  wch <- which(cumsum >= atu)
  if(!length(wch)) return(0L)
  wch[1]
}

calc_atus <- function(atu, cumsum) {
  vapply(cumsum, calc_atu, 1L, atu = atu, cumsum = cumsum)
}

prop_qt <- function(prop) {
  prop <- 1 - (1 - prop) / 2
  stats::qt(prop, Inf)
}

dot <- function(x) paste0(".", x)

set_na <- function(x) {
  is.na(x) <- TRUE
  x
}
