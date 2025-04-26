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
  preds <- predict(fit, newdata = df, type = "response", se.fit = TRUE)

  expect_true(is.list(preds))
  expect_true("se.fit" %in% names(preds))
})

test_that("predict.robust_glm works correctly", {
  # Simulated dataset
  set.seed(123)
  df <- data.frame(
    y = runif(50, 0, 1),
    x1 = rep(1:2, 25),
    x2 = runif(50, 0.5, 1),
    x3 = runif(50, 0, 0.5)
  )

  # Fit robust_glm
  fit <- robust_glm(y ~ x1 + x2, data = df, family = "quasibinomial")

  # Predict without se.fit
  preds_link <- predict(fit, newdata = df, type = "link", se.fit = FALSE)
  preds_resp <- predict(fit, newdata = df, type = "response", se.fit = FALSE)

  expect_type(preds_link, "double")
  expect_type(preds_resp, "double")
  expect_length(preds_link, nrow(df))
  expect_length(preds_resp, nrow(df))
  expect_false(anyNA(preds_link))
  expect_false(anyNA(preds_resp))

  # Predict with se.fit = TRUE
  preds_se_link <- predict(fit, newdata = df, type = "link", se.fit = TRUE)
  preds_se_resp <- predict(fit, newdata = df, type = "response", se.fit = TRUE)

  expect_named(preds_se_link, c("fit", "se.fit"))
  expect_named(preds_se_resp, c("fit", "se.fit"))
  expect_length(preds_se_link$fit, nrow(df))
  expect_length(preds_se_resp$fit, nrow(df))
  expect_length(preds_se_link$se.fit, nrow(df))
  expect_length(preds_se_resp$se.fit, nrow(df))
  expect_false(anyNA(preds_se_link$se.fit))
  expect_false(anyNA(preds_se_resp$se.fit))

  # Predict with confidence intervals
  preds_ci <- predict(fit, newdata = df, type = "response",
                      se.fit = TRUE, interval = "confidence", level = 0.95)

  expect_named(preds_ci, c("fit", "se.fit", "conf.low", "conf.high"))
  expect_length(preds_ci$fit, nrow(df))
  expect_length(preds_ci$conf.low, nrow(df))
  expect_length(preds_ci$conf.high, nrow(df))
  expect_true(all(preds_ci$conf.low <= preds_ci$fit))
  expect_true(all(preds_ci$fit <= preds_ci$conf.high))
})

test_that("robust_glm drops collinear variables", {
  set.seed(123)
  df <- data.frame(
    y = runif(50, 0, 1),
    x1 = rep(1:2, 25),
    x2 = rep(1:2, 25), # perfectly collinear with x1
    x3 = runif(50, 0, 0.5)
  )

  expect_warning(fit <- robust_glm(y ~ x1 + x2, data = df, family = "quasibinomial"),
                 "Dropped collinear terms")

  expect_s3_class(fit, "robust_glm")
  expect_true("collinear_terms" %in% names(fit))
  expect_true("x2" %in% fit$collinear_terms)
})

test_that("predict.robust_glm uses object$data when newdata is NULL", {
  set.seed(123)
  df <- data.frame(
    y = runif(50, 0, 1),
    x1 = rep(1:2, 25),
    x2 = runif(50, 0.5, 1),
    x3 = runif(50, 0, 0.5)
  )

  fit <- robust_glm(y ~ x1 + x2, data = df, family = "quasibinomial")

  preds <- predict(fit, type = "response", se.fit = FALSE)

  expect_type(preds, "double")
  expect_length(preds, nrow(df))
})

test_that("predict.robust_glm errors on wrong newdata type", {
  set.seed(123)
  df <- data.frame(
    y = runif(50, 0, 1),
    x1 = rep(1:2, 25),
    x2 = runif(50, 0.5, 1),
    x3 = runif(50, 0, 0.5)
  )

  fit <- robust_glm(y ~ x1 + x2, data = df, family = "quasibinomial")

  expect_error(predict(fit, newdata = as.matrix(df)),
               "newdata must be a data frame")
})