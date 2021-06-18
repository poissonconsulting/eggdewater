#' Survival by Elevation
#'
#' @param nest A number of the nest elevation.
#' @param eggs A double vector of the water elevations during the egg stage.
#' @param egg_int A count of the number intervals eggs can survive dewatering.
#'
#' @return A number of the proportion surviving.
#' @export
edw_survival_elevation <- function(nest, eggs, egg_int = 0L) {
  chk_scalar(nest)
  chk_vector(eggs)
  chk_whole_number(egg_int)

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
  chk_scalar(nest)
  chk_vector(eggs)
  chk_vector(alevins)
  chk_whole_number(egg_int)
  chk_whole_number(alevin_int)
  
  eggs <- edw_survival_elevation(nest, eggs, egg_int)
  alevins <- edw_survival_elevation(nest, alevins, alevin_int)
  eggs * alevins
}
