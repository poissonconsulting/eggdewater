context("context")

test_that("survival_elevation", {
  expect_identical(edw_survival_elevation(0, -1), 0)
  expect_identical(edw_survival_elevation(0, 0), 0)
  expect_identical(edw_survival_elevation(0, 1), 1)
  expect_identical(edw_survival_elevation(0, c(1,1,1)), 1)
  expect_identical(edw_survival_elevation(0, c(0,1,1)), 0)
  expect_identical(edw_survival_elevation(0, c(1,1,0)), 0)
  expect_identical(edw_survival_elevation(0, c(1,1,0), 1L), 1)
  expect_identical(edw_survival_elevation(0, c(0,1,0), 1L), 1)
  expect_identical(edw_survival_elevation(0, c(0,0,1), 1L), 0)
  expect_identical(edw_survival_elevation(0, c(0,0,0), 1L), 0)
})

test_that("survival_elevation2", {
  expect_identical(edw_survival_elevation2(0, -1, 1), 0)
  expect_identical(edw_survival_elevation2(0, 0, 1), 0)
  expect_identical(edw_survival_elevation2(0, 1, 1), 1)
  expect_identical(edw_survival_elevation2(0, 1, 0), 0)
  expect_identical(edw_survival_elevation2(0, c(0,1,1), c(0,0,1), 1L, 2L), 1)
  expect_identical(edw_survival_elevation2(0, c(0,1,0), c(0,0,1), 1L, 2L), 1)
  expect_identical(edw_survival_elevation2(0, c(0,0,1), c(0,0,1), 1L, 2L), 0)
})