spawn_timing <- function(date, sd, prop) {
  date <- dtt_floor(date)
  diff <- ceiling(sd * prop_qt(prop))
  dates <- seq(date - diff, date + diff, by = "days")
  dist <- dnorm(as.integer(dates), as.integer(date), sd = sd)
  dist <- dist / sum(dist)
  dts <- data.frame(Date = dates, Value = dist, MeanSpawnDate = date)
  if(requireNamespace("tibble", quietly = TRUE)) dts <- tibble::as_tibble(dts)
  dts
}

#' Spawn Timing
#'
#' Assumes that spawning is normally distributed.
#' There must be no overlap between the spawning associated with each mean spawn date.
#' 
#' @param date A Date vector of the mean spawn date.
#' @param sd A number of the standard deviation in the spawning distribution.
#' @param prop A number of the proportion of spawning to consider.
#' @return A dts object where DateTime is the Date, Value is the yearly proportion
#' and MeanSpawnDate is the mean spawn date.
#' @export
#' @examples
#' edw_spawn_timing(as.Date(c("2000-03-30", "2001-04-06")), sd = 1)
edw_spawn_timing <- function(date, sd, prop = 0.95) {
  check_vector(date, Sys.Date(), length = TRUE)
  check_scalar(sd, c(1, 60))
  check_scalar(prop, c(0.5, 0.99))

  dts <- lapply(date, spawn_timing, sd, prop)
  dts <- do.call("rbind", dts)
  if(anyDuplicated(dts$Date))
    err("overlapping spawn timings are not permitted")
  dts <- dts[order(dts$Date),]
  dts
}
