context("spawn-timing")

test_that("spawn-timing", {
  data <- spawn_timing(as.Date("2000-01-01"), 1, 0.95)
  expect_is(data, "data.frame")
  expect_identical(colnames(data), c("DateTime", "Value", "MeanSpawnDate"))
  expect_identical(data$DateTime, seq(as.Date("1999-12-30"), as.Date("2000-01-03"), by = "days"))
  expect_equal(data$Value, c(0.05448868, 0.24420134, 0.40261995, 0.24420134, 0.05448868))
  expect_identical(data$MeanSpawnDate, rep(as.Date("2000-01-01"), 5))
})

test_that("spawn-timing", {
  expect_error(
    edw_spawn_timing(as.Date(c("2001-04-06", "2001-04-07")), sd = 2),
    "overlapping spawn timings are not permitted")
  data <- edw_spawn_timing(as.Date(c("2001-04-06", "2000-03-30")), sd = 2)
  expect_is(data, "data.frame")
  expect_identical(colnames(data), c("DateTime", "Value", "MeanSpawnDate"))
  expect_identical(nrow(data), 18L)
  expect_identical(data$DateTime[1], as.Date("2000-03-26"))
  expect_identical(data$MeanSpawnDate[1], as.Date("2000-03-30"))
  expect_identical(data$MeanSpawnDate[nrow(data)], as.Date("2001-04-06"))
  expect_equal(data$Value[1], 0.0276, tolerance = 1e-04)
  expect_equal(data$Value[nrow(data)], 0.0276, tolerance = 1e-04)
})
