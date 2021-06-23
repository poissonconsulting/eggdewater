transect_weighting <- function(riverkm, spawning, rkm) {
  weight <- rep(0, length(rkm))
  if(riverkm <= rkm[1]) {
    weight[1] <- spawning
  } else if(riverkm >= rkm[length(rkm)]) {
    weight[length(rkm)] <- spawning
  } else if(any(riverkm == rkm)) {
    weight[which(riverkm == rkm)] <- spawning
  } else {
    t1 <- which(riverkm > rkm)
    t2 <- which(riverkm < rkm)
    t1 <- t1[length(t1)]
    t2 <- t2[1]
    w1 <- (rkm[t2] - riverkm) / (rkm[t2] - rkm[t1])
    weight[t1] <- spawning * w1
    weight[t2] <- spawning * (1 - w1)
  }
  weight
}

#' Transect Weighting
#' 
#' @param rkm A double vector of the transect river kms
#' @param data A data frame with columns indicating the river km and proportion of spawning.
#' @param riverkm A string of the name of the column in data with the river km.
#' @param spawning A string of the name of the column in data with the spawning proportions.
#' @return A double vector of the transect weightings.
#' @export
#' @examples
#' edw_transect_weighting(c(1,2), data = data.frame(
#'   RiverKm = c(0.5,1.9,3),
#'   Spawning = c(0.1,0.8,0.1)
#' ))
edw_transect_weighting <- function(rkm, data, riverkm = "RiverKm", spawning = "Spawning") {
  chk_vector(rkm)
  check_values(rkm, 1)
  chk_unique(rkm)
  
  check_data(data, nrow = TRUE)
  check_names(data, names = c(riverkm, spawning))
  
  chk_unique(data[[riverkm]])
  chk_vector(data[[riverkm]], x_name =
               paste0("column '", riverkm, "' of data"))
  check_values(data[[riverkm]], 1)
  
  chk_vector(data[[spawning]], x_name = paste0("column '", spawning, "' of data"))
  check_dim(data[[spawning]])
  chk_range(data[[spawning]], c(0,1))

  data <- mapply(transect_weighting, data[[riverkm]], data[[spawning]], 
                 MoreArgs = list(rkm = sort(rkm)), SIMPLIFY = FALSE)
  
  data <- do.call("rbind", data)
  data <- apply(data, 2, sum)
  data <- data[(1:length(rkm))[order(rkm)]]
  data
}
