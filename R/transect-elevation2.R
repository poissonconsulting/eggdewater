transect_elevation2 <- function(model, hydrograph, discharge, elevation) {
  out <- interp::interp(
    x = model[[discharge[1]]], y = model[[discharge[2]]], z = model[[elevation]], 
    xo = hydrograph[[discharge[1]]], yo = hydrograph[[discharge[2]]], output = "points")$z
  hydrograph[[elevation]] <- out
  hydrograph
}

#' Transect Elevation2
#' 
#' @inheritParams edw_transect_elevation
#' @param discharge A string of the names of the column with the discharge values.
#' @return The hydrograph data with an elevation column.
#' @export
edw_transect_elevation2 <- function(hydrograph, model, 
                                    discharge = c("Discharge", "Discharge2"), 
                                    elevation = "Elevation") {
  check_vector(discharge, "", unique = TRUE, length = 2L)
  check_string(elevation)
  check_data(hydrograph, discharge)
  check_missing_colnames(hydrograph, c(elevation, "..ID"))
  check_data(model, c(discharge, elevation), nrow = TRUE)
  
  if(!nrow(hydrograph)) return(double(0))
  
  check_vector(hydrograph[[discharge[1]]], c(0, .Machine$double.xmax), x_name =
                 paste0("column '", discharge[1], "' of hydrograph"))
  
  check_vector(hydrograph[[discharge[2]]], c(0, .Machine$double.xmax), x_name =
                 paste0("column '", discharge[2], "' of hydrograph"))
  
  check_vector(model[[discharge[1]]], c(0, .Machine$double.xmax), x_name =
                 paste0("column '", discharge[1], "' of model"))
  
  check_vector(model[[discharge[2]]], c(0, .Machine$double.xmax), x_name =
                 paste0("column '", discharge[2], "' of model"))
  
  check_vector(model[[elevation]], 1, x_name =
                 paste0("column '", elevation, "' of model"))
  
  transect_elevation2(model, hydrograph, discharge, elevation)
}
