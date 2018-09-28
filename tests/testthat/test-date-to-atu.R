context("date_to_atu")

test_that("date_to_atu", {
  data <- data.frame(DateTime = as.Date(paste0("2000-01-", c("01", "02", "03", "04"))))
  data$Value <- 1:4
  expect_identical(date_to_atu(as.Date("2000-01-01"), 0, data), as.Date("2000-01-01"))
  expect_identical(date_to_atu(as.Date("2000-01-01"), 1, data), as.Date("2000-01-02"))
  expect_identical(date_to_atu(as.Date("2000-01-02"), 1, data), as.Date("2000-01-03"))
  date <- Sys.Date()
  is.na(date) <- TRUE
  expect_identical(date_to_atu(as.Date("2000-01-01"), 4, data), date)
})

test_that("edw_date_to_atu", {
  data <- data.frame(DateTime = as.Date(paste0("2000-01-", c("01", "02", "03", "04", "05"))))
  data$Value <- 1
  expect_identical(edw_date_to_atu(as.Date(c("2000-01-02", "2000-01-01")), atu = 2L, data),
                   as.Date(c("2000-01-04", "2000-01-03")))
})