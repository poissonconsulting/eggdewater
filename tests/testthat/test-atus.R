context("atus")

test_that("atus", {
  data <- data.frame(DateTime = as.Date(paste0("2000-01-", c("01", "02", "03", "04"))))
  data$Value <- 1:4
  expect_identical(colnames(edw_atus(data)), c("DateTime", "Value", "Hatch", "Emergence"))

  expect_identical(edw_atus(data)$Hatch, rep(NA_real_, 4))
  expect_error(edw_atus(data, atus = list(Hatch = 2)), 
               "elements of atus must be class integer")
  expect_identical(edw_atus(data, atus = list(Hatch = 0L))$Hatch, data$DateTime)
  expect_identical(edw_atus(data, atus = list(Hatch2 = 3L))$Hatch2, 
                   c(as.Date(paste0("2000-01-", c("03", "03", "04"))), NA))
})
