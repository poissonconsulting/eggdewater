#' Transect Elevation
#' 
#' @param hydrograph A data frame of the discharge values.
#' @param model A data frame of discharge and elevation values.
#' @param discharge A string of the name of the column with the discharge values.
#' @param elevation A string of the name of the column with the elevation values.
#' @return The hydrograph data with an elevation column.
#' @export
#' @examples
#' edw_transect_elevation(
#'   data.frame(Discharge = 12), 
#'   data.frame(Discharge = c(10,15), Elevation = c(40,50)))
edw_transect_elevation <- function(hydrograph, model, discharge = "Discharge", 
                                   elevation = "Elevation") {
  check_string(discharge)
  check_string(elevation)
  check_data(hydrograph, discharge)
  check_missing_colnames(hydrograph, elevation)
  check_data(model, c(discharge, elevation), nrow = TRUE)
  
  if(!nrow(hydrograph)) return(double(0))
  
  check_vector(hydrograph[[discharge]], c(0, .Machine$double.xmax), x_name =
                 paste0("column '", discharge, "' of hydrograph"))
  
  check_vector(model[[discharge]], c(0, .Machine$double.xmax), x_name =
                 paste0("column '", discharge, "' of model"))
  
  check_vector(model[[elevation]], 1, x_name =
                 paste0("column '", elevation, "' of model"))
  
  out <- approx(model[[discharge]], model[[elevation]], xout = hydrograph[[discharge]])$y
  hydrograph[[elevation]] <- out
  hydrograph
}
