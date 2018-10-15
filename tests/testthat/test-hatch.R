context("hatch")

test_that("hatch", {
  data <- data.frame(DateTime = as.Date(paste0("2000-01-", c("01", "02", "03", "04"))))
  data$Value <- 1:4
  expect_identical(colnames(edw_hatch(data)), c("DateTime", "Value", "Hatch", "Emergence"))

  expect_identical(edw_hatch(data)$Hatch, rep(NA_real_, 4))
  expect_error(edw_hatch(data, atus = list(Hatch = 2)), 
               "elements of atus must be class integer")
  expect_identical(edw_hatch(data, atus = list(Hatch = 0L))$Hatch, data$DateTime)
  expect_identical(edw_hatch(data, atus = list(Hatch2 = 3L))$Hatch2, 
                   c(as.Date(paste0("2000-01-", c("03", "03", "04"))), NA))
  
  data$DateTime <- dttr::dtt_date_time(data$DateTime, tz = "Etc/GMT+8")
  hatch2 <- edw_hatch(data, atus = list(Hatch2 = 3L))$Hatch2
  expect_identical(data$DateTime[3], hatch2[1])
})
