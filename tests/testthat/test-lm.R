context("test-lm")

model <- lm(mpg ~ hp, data = mtcars)

test_that("Profiling confidence intervals for ordinary linear regression works", {
  ci <- make_confidence_intervals(model)
  expect_equal(dim(ci), dim(model$model) + c(0, 3))
})

test_that("Profiling prediction intervals for ordinary linear regression works", {
  pi <- make_prediction_intervals(model)
  expect_equal(dim(ci), dim(model$model) + c(0, 3))
})