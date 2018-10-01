date_to_atu <- function(date, atu, dts, date_time = "DateTime", value = "Value") {
  dts <- dts[dts[[date_time]] >= date,]
  dts[[value]] <- dts[[value]] - dts[[value]][1]
  dts <- dts[dts[[value]] >= atu,]
  dts[[date_time]][1]
}

#' Date to Accumulated Thermal Units (ATUs)
#'
#' @param date A Date vector.
#' @param atu A count of the ATUs
#' @param dts A dts object.
#' @param date_time A string of the Date column name in dts.
#' @param value A string of the temperature column name in dts.
#'
#' @return A Date vector.
#' @export
edw_hatch_timing <- function(date, atu, dts, date_time = "DateTime", value = "Value") {
  check_vector(date, Sys.Date())
  check_count(atu)
  check_dts(dts, date_time = date_time, value = value, complete = TRUE, 
            sorted = TRUE, units = "days", key = date_time, nrow = TRUE,
            values = c(0, 30))

  if(!length(date)) return(Sys.Date()[-1])
  
  dts[[date_time]] <- dtt_date(dts[[date_time]])
  
  if(any(date < dts[[date_time]][1]) || any(date > dts[[date_time]][nrow(dts)]))
    err("date values must fall within dts object dates")
  
  dts[[value]] <- cumsum(dts[[value]])
  
  date <- vapply(date, FUN = date_to_atu, FUN.VALUE = Sys.Date(), 
                 atu, dts, date_time, value)
  class(date) <- "Date"
  date
}
