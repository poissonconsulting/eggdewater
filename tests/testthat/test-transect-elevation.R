context("transect-elevation")

test_that("transect_elevation", {
  expect_identical(edw_transect_elevation(
    data.frame(Discharge = c(0,10,12,15,16)), 
    data.frame(Discharge = c(10,15), Elevation = c(40,50))),
    data.frame(Discharge = c(0,10,12,15,16),
               Elevation = c(NA,40,44,50,NA)))
})

