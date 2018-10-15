context("spawning")

test_that("spawning", {
  data <- data.frame(DateTime = seq(as.Date("1972-01-01"), as.Date("1973-01-01"),
                                    by = 1))
  
  data <- edw_spawning(data, peak = as.Date(c("1972-03-01", "1972-09-01", "1973-01-02")))
  expect_equal(data$Spawning[1], 7.338773e-23, tolerance = 1e-29) 
  expect_equal(data$Spawning[61], 6.596268e-07, tolerance = 1e-13)
  expect_equal(data$Spawning[153], 4.253848e-44, tolerance = 1e-50) 
  expect_equal(data$Spawning[245], 6.596268e-07, tolerance = 1e-13)
  expect_equal(data$Spawning[246], 6.529301e-07, tolerance = 1e-13)
  expect_equal(data$Spawning[367], 6.529301e-07, tolerance = 1e-13)
})
