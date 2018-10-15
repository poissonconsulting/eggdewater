#' Spawning
#' 
#' The spawning densities are calculated using dnorm.
#' Its important to realize that the densities will need rescaling.
#'
#' @param x A dts data frame.
#' @param dtt A string of the name of the column with the DateTime values.
#' @param colname A string of the name of the column to save the spawning densities to.
#' @param peak A Date vector of the mean spawn dates.
#' @param sd A positive integer scalar or vector of the standard deviation in days in the distribution of spawning.
#'
#' @return The dts data frame with a column of the spawning densities. 
#' @export
edw_spawning <- function(x, dtt = "DateTime", colname = "Spawning",
                         peak = as.Date("1972-06-01"), sd = 7L) {
  check_dts(x, dtt = dtt, nrow = TRUE, floored = TRUE, sorted = TRUE, 
            unique = TRUE, complete = TRUE)
  check_string(colname)
  if(dtt == colname) err("colname must not be '", dtt, "'")
  check_missing_colnames(x, dot(dtt))
  check_vector(peak, Sys.Date())
  check_vector(sd, c(1L, 90L), length = c(1L, 1L, length(peak)))
  
  x[[colname]] <- 0
  if(!length(peak)) return(x)
  
  if(length(sd) == 1L) sd <- rep(sd, length(peak))
  
  peak <- dtt_date_time(peak, tz = dtt_tz(x[[dtt]]))
  peak <- as.integer(peak)
  x[[dot(dtt)]] <- dtt_date_time(x[[dtt]])
  x[[dot(dtt)]] <- as.integer(x[[dot(dtt)]])
  sd <- sd * 24L * 60L * 60L
  
  for(i in seq_along(peak)) {
    x[[colname]] <- x[[colname]] + dnorm(x[[dot(dtt)]], mean = peak[i], sd = sd[i])
  }
  x[[dot(dtt)]] <- NULL
  x
}