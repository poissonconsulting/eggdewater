context("data")

test_that("rivers_model", {
  expect_identical(checkr::check_data(rivers_model, values = list(
    River1 = c(250, 5000), River2 = c(250, 5000)),
                              nrow = 100L), rivers_model)
})