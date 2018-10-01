context("transect-weighting")

test_that("transect_weighting", {
  expect_equal(transect_weighting(-9, 0.5, c(-11,-8)), c(0.1666667, 0.3333333),
               tolerance = 1e-07)
})

test_that("edw_transect_weighting", {
  expect_equal(edw_transect_weighting(c(1,2), data = data.frame(
    RiverKm = c(0.5),
    Spawning = c(1))), c(1, 0))

  expect_equal(edw_transect_weighting(c(1,2), data = data.frame(
    RiverKm = c(3),
    Spawning = c(1))), c(0, 1))

  expect_equal(edw_transect_weighting(c(1,2), data = data.frame(
    RiverKm = c(1.9),
    Spawning = c(1))), c(0.1, 0.9))
  
  expect_equal(edw_transect_weighting(c(1,2), data = data.frame(
    RiverKm = c(0.5,1.9,3),
    Spawning = c(0.1,0.8,0.1))), c(0.18, 0.82))
  
  expect_equal(edw_transect_weighting(c(2,1), data = data.frame(
    RiverKm = c(0.5,1.9,3),
    Spawning = c(0.1,0.8,0.1))), c(0.82,0.18))
})
