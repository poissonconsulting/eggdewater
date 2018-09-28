context("internal")

test_that("prop_qt", {
  expect_equal(prop_qt(0.95), 1.959964)
  expect_equal(prop_qt(0.99), 2.575829, tolerance = 1e-06)
})