context("transect-elevation2")

test_that("transect_elevation2", {
  
  hydrograph <- data.frame(Discharge = 15, Discharge2 = 90)
  model <- data.frame(Discharge = c(10,20,10,20),
                      Discharge2 = c(50,50,100,100),
                      Elevation = c(3,4,6,9))
  
  expect_equal(edw_transect_elevation2(hydrograph, model),
               data.frame(Discharge = 15, Discharge2 = 90, Elevation = 6.7),
               check.attributes = FALSE)
})

test_that("rivers_model", {
  hydrograph <- rivers_model
  hydrograph$ModelElevation <- hydrograph$Elevation
  hydrograph$Elevation <- NULL
  
  hydrograph <- edw_transect_elevation2(hydrograph, rivers_model, discharge = c("River1", "River2"))
  expect_equal(hydrograph[c("River1", "River2", "Elevation")], rivers_model,
               check.attributes = FALSE)
})


