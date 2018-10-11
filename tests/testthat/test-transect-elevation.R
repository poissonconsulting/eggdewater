context("transect-elevation")

test_that("transect_elevation", {
  expect_identical(edw_transect_elevation(
    data.frame(Discharge = c(12,10,0,15,16)), 
    data.frame(Discharge = c(10,15), Elevation = c(40,50))),
    data.frame(Discharge = c(12,10,0,15,16),
               Elevation = c(44,40,NA,50,NA)))
})

