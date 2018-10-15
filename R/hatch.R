#' Date to Hatch based on Accumulated Thermal Units (ATUs)
#'
#' @param x A dts object.
#' @param dtt A string of the name of the column with the Date or POSIXct values.
#' @param colname A string of the name of the column with the temperature data.
#' @param atus A named list of the daily accumulated thermal units to life stage.
#' @return The modified dts object.
#' @export
edw_hatch <- function(x, dtt = "DateTime", colname = "Value", 
                     atus = list(Hatch = 320L, Emergence = 480L)) {
  check_string(colname)
  check_dts(x, dtt = dtt, colname = colname, unique = TRUE, sorted = TRUE,
            complete = TRUE)
  check_named(atus, unique = TRUE)
  check_missing_names(atus, c(dtt, colname))
  lapply(atus, check_count, x_name = "elements of atus")
  
  if(!length(atus)) return(x)
  
  units <- dtt_units(x[[dtt]])
  times <- switch(units,
                  months = 1/12,
                  days = 1,
                  hours = 24,
                  minutes = 24 * 60,
                  seconds = 24 * 60 * 60,
                  err("units of x must not be 'years'"))
  atus <- lapply(atus, function(x) x * times)
  
  wch <- lapply(atus, calc_atus, cumsum(x[[colname]]))
  x[names(wch)] <- set_na(x[[dtt]][1])
  for(col in names(wch)) {
    x[[col]][wch[[col]] != 0L] <- x[[dtt]][wch[[col]]]
  }
  x
}
