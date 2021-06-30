#' Survival by Elevation
#'
#' @param nest A number of the nest elevation.
#' @param eggs A double vector of the water elevations during the egg stage.
#' @param egg_int A count of the number intervals eggs can survive dewatering.
#'
#' @return A number of the proportion surviving.
#' @export
edw_survival_elevation <- function(nest, eggs, egg_int = 0L) {
  chk_scalar(nest, 1)
  chk_vector(eggs)
  check_values(eggs, 1)
  check_dim(eggs)
  chk_integer(egg_int)
  chk_gte(egg_int)

  eggs <- eggs <= nest
  eggs <- rle(eggs)
  eggs <- eggs$lengths[eggs$values]
  if(!length(eggs)) return(1)
  eggs <- max(eggs)
  if(eggs > egg_int) return(0)
  return(1)
}


#' Survival by Elevation and Intervals
#'
#' @inheritParams edw_survival_elevation
#' @param alevins A double vector of the water elevations during the alevin stage.
#' @param alevin_int A count of the number of intervals alevins can survive dewatering.
#'
#' @return A number of the proportion surviving.
#' @export
edw_survival_elevation2 <- function(nest, eggs, alevins,
                              egg_int = 0L, alevin_int = 0L) {
  chk_scalar(nest, 1)
  chk_vector(eggs)
  check_values(eggs, 1)
  chk_vector(alevins)
  check_values(alevins, 1)
  
  chk_integer(egg_int)
  chk_gte(egg_int)

  chk_integer(alevin_int)
  chk_gte(alevin_int)

  eggs <- edw_survival_elevation(nest, eggs, egg_int)
  alevins <- edw_survival_elevation(nest, alevins, alevin_int)
  eggs * alevins
}
