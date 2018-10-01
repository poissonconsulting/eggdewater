context("transect-elevation2")

test_that("transect_elevation2", {
  
  hydrograph <- data.frame(Discharge = 15, Discharge2 = 90)
  model <- data.frame(Discharge = c(10,20,10,20),
                      Discharge2 = c(50,50,100,100),
                      Elevation = c(3,4,6,9))
  
  expect_identical(edw_transect_elevation2(hydrograph, model),
                   data.frame(Discharge = 15, Discharge2 = 90, Elevation = 6.7))
})

