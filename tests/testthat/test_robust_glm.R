library(testthat)
library(robustglm)

test_that("robust_glm runs without errors", {
  set.seed(123)
  df <- data.frame(y = runif(50, 0, 1),
                   x1 = rep(1:2, 50),
                   x2 = runif(50, .5, 1),
                   x3 = runif(50, 0, .5))

  fit <- robust_glm(y ~ x1 + x2, df, family = "quasibinomial")

  expect_s3_class(fit, "robust_glm")
  expect_true("sandwich" %in% names(fit))
})

test_that("predict.robust_glm returns expected output", {
  set.seed(123)
  df <- data.frame(y = runif(50, 0, 1),
                   x1 = rep(1:2, 50),
                   x2 = runif(50, .5, 1),
                   x3 = runif(50, 0, .5))

  fit <- robust_glm(y ~ x1 + x2, df, family = "quasibinomial")
  preds <- robustglm:::predict.robust_glm(fit, newdata = df, type = "response",
                                          se.fit = TRUE)

  expect_true(is.list(preds))
  expect_true("se.fit" %in% names(preds))
})