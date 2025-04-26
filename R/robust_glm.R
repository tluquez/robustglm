#' Fit a Generalized Linear Model with Robust Weights and Errors
#'
#' This function fits a generalized linear model (GLM) with robust weights per
#'  observation, to down-weight outliers, and robust sandwich standard errors,
#'  to account for the lack of independance or heteroscedasticity.
#'
#' @param formula A formula specifying the model.
#' @param data A data frame containing the variables in the model.
#' @param subset An optional character vector specifying a subset of observations
#'               to be used in the model.
#' @param family A character string or function (see \code{lm()}) specifying
#' the distribution family in the GLM (default is "binomial").
#' @param robust_weights Logical indicating whether to compute robust model weights
#'                       (default is TRUE).
#' @param sandwich Logical indicating whether to compute sandwich standard errors
#'                 (default is TRUE).
#' @param add_ci Logical indicating whether to add confidence intervals to the model
#'               coefficients (default is TRUE).
#' @param p An optional progressor object to monitor progress (default is NULL).
#' @param ... Other arguments passed on to \code{stats::glm}.
#'
#' @return An object of class \code{"glm"} with additional attributes such as
#'         confidence intervals, sandwich standard errors, and collinear terms.
#'
#' @examples
#' df <- data.frame(y = runif(50, 0, 1),
#'                  x1 = rep(1:2, 50),
#'                  x2 = runif(50, .5, 1),
#'                  x3 = runif(50, 0, .5))
#' fit <- robust_glm(y ~ x1 + x2, df, family = "quasibinomial")
#' summary(fit)
#'
#' @importFrom MASS rlm
#' @importFrom bestNormalize bestNormalize
#' @importFrom sandwich vcovHC
#' @importFrom lmtest coeftest coefci
#' @export

robust_glm <- function(formula, data, subset = NULL, family = "quasibinomial",
                       robust_weights = T, sandwich = T, add_ci = T, p = NULL,
                       ...) {
  # Input validation
  if (missing(formula) || missing(data)) {
    stop("Both 'formula' and 'data' arguments must be provided.")
  }
  if (!inherits(formula, "formula") && !is.character(formula)) {
    stop("Argument 'formula' must be either a character formula or a formula object.")
  }
  if (!inherits(formula, "formula") && is.character(formula)) {
    formula <- as.formula(formula)
  }
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data frame.")
  }
  if (!is.null(subset) && !is.character(subset)) {
    stop("Argument 'subset' must be a character vector.")
  }

  # Subset data
  if (!is.null(subset)) {
    data <- subset(data, eval(parse(text = subset)))
    data <- droplevels(data)
  }

  # Extract model terms
  y <- get_response(formula)
  covs <- get_covs(formula)

  # Check if terms are present in the data
  missing_terms <- c(y, covs)[!(c(y, covs) %in% names(data))]
  if (length(missing_terms) > 0) {
    stop("Variable(s) '", paste(missing_terms, collapse = "', '"),
         "' not found in the data.")
  }

  # Clean data
  data <- data[, c(y, covs)]
  data <- na.omit(data)

  # Extract the response variable values
  prop <- data[[y]]

  # Check for collinear terms
  x <- model.matrix(formula, data)
  ncovs <- ncol(x)
  QR <- qr(x)
  if (QR$rank < ncovs) {
    collinear_terms <- colnames(x)[QR$pivot[(QR$rank + 1):ncovs]]
    if (all(collinear_terms %in% covs)) {
      formula <- reformulate(covs[!covs %in% collinear_terms], response = y)
    }
    collinear_terms <- paste(collinear_terms, collapse = ", ")

    warning("Dropped collinear terms: ", collinear_terms)
  } else {
    collinear_terms <- NULL
  }

  # Compute robust model weights if requested
  if (robust_weights && is.numeric(data[[y]]) &&
      length(unique(data[[y]])) > 2) {
    # From [0, 1] to (-Inf, +Inf)
    data$prop_norm <- bestNormalize::bestNormalize(prop, loo = T, quiet = T)$x.t
    robust_formula <- update.formula(formula, prop_norm ~ .)
    rweights <- MASS::rlm(robust_formula, data = data)$w

    if (length(rweights) != length(prop)) {
      stop("Weight and response have different lengths. Any NA maybe?")
    }

    data$rweights <- rweights

    # Fit a generalized linear model with robust weights
    fit <- stats::glm(formula, data = data, family = family, weights = rweights,
                      ...)
  } else {
    fit <- stats::glm(formula, data = data, family = family, ...)
  }

  if (add_ci) {
    fit$ci <- suppressMessages(confint(fit))
    colnames(fit$ci) <- c("conf.low", "conf.high")
  }

  # Add sandwich errors
  if (sandwich) {
    vcov_mat <- sandwich::vcovHC(fit, type = "HC3")
    fit$vcov_mat <- vcov_mat
    fit$sandwich <- lmtest::coeftest(fit, vcov. = vcov_mat)
    sandwich_ci <- lmtest::coefci(fit, vcov. = vcov_mat)
    fit$sandwich <- cbind(fit$sandwich, sandwich_ci)
    colnames(fit$sandwich) <- c("estimate", "std.error", "statistic", "p.value",
                                "conf.low", "conf.high")
  }

  # Add collinear terms
  fit$collinear_terms <- collinear_terms

  # Add class robust_glm
  class(fit) <- c("robust_glm", class(fit))

  # Step forward the progressor bar
  if (!is.null(p) && inherits(p, "progressor")) p()

  return(fit)
}

#' @export
get_response <- function(formula) {
  if (inherits(formula, c("lm", "glm", "robust_glm"))) {
    formula <- formula$formula
  } else if(!inherits(formula, "formula")) {
    stop("Not formula or model object")
  }
  all.vars(formula[[length(formula) - 1]])
}

#' @export
get_covs <- function(formula) {
  if (inherits(formula, c("lm", "glm", "robust_glm"))) {
    formula <- formula$formula
  } else if(!inherits(formula, "formula")) {
    stop("Not formula or model object")
  }
  all.vars(formula[[length(formula)]])
}

#' @export
predict.robust_glm <- function(object, newdata = NULL,
                               type = c("link", "response"),
                               se.fit = FALSE, sandwich = TRUE,
                               interval = c("none", "confidence"),
                               level = 0.95,
                               ...) {
  type <- match.arg(type)
  interval <- match.arg(interval)

  if (is.null(newdata)) {
    newdata <- object$data
  }

  if (!is.data.frame(newdata)) {
    stop("newdata must be a data frame.")
  }

  # Get model matrix
  X <- model.matrix(object$formula, newdata)

  # Use base predict.glm to get linear predictor
  pred_link <- stats::predict.glm(object, newdata = newdata,
                                  type = "link", se.fit = FALSE, ...)

  fit <- if (type == "response") {
    family <- object$family$family
    linkinv <- object$family$linkinv

    if (is.null(linkinv)) {
      stop("Unsupported family/link function.")
    }
    linkinv(pred_link)
  } else {
    pred_link
  }

  if (se.fit && sandwich && !is.null(object$vcov_mat)) {
    V <- object$vcov_mat
    se_link <- sqrt(diag(X %*% V %*% t(X)))

    if (type == "link") {
      se_final <- se_link
    } else if (type == "response") {
      # Delta method: derivative of inverse link
      mu <- object$family$linkinv(pred_link)
      dlinkinv <- mu * (1 - mu) # derivative for logistic
      se_final <- dlinkinv * se_link
    }

    result <- list(fit = fit, se.fit = se_final)

    if (interval == "confidence") {
      z <- qnorm((1 + level) / 2)
      result$conf.low <- fit - z * se_final
      result$conf.high <- fit + z * se_final
    }

    return(result)
  }

  return(fit)
}

#' Get Predicted Values from a Model
#'
#' This function calculates predicted values from a fitted model, setting covariates
#' to their median (for numeric variables) or mode (for categorical variables).
#'
#' @param mod A fitted model object, such as from `glm`, `lm`, or `robust_glm`.
#' @param exclude_cols Columns in `mod$data` to exclude from transformation. A
#'  tidyselect specification.
#' @return A tibble with linear predictors, predicted outcome, and standard
#'  errors of the predicted outcome.
#' @importFrom dplyr mutate across matches select rename
#' @importFrom tibble tibble
#' @export

get_predicted <- function(mod, exclude_cols = NULL, type = "link") {

  if (!"data" %in% names(mod)) {
    stop("The provided model does not contain a 'data' component.")
  }
  if (!inherits(mod, c("glm", "lm", "robust_glm"))) {
    stop("The model must be of class 'glm', 'lm', or 'robust_glm'.")
  }

  # Compute new_data with median/mode for covariates
  new_data <- mod$data %>%
    dplyr::mutate(dplyr::across(
      .cols = -dplyr::any_of({{ exclude_cols }}),
      .fns = ~ if (is.numeric(.)) {median(., na.rm = T)} else {Mode(.)}
    ))

  # Compute predictions
  preds <- if (inherits(mod, "robust_glm")) {
    robustglm:::predict.robust_glm(mod, newdata = new_data, type = type,
                                   se.fit = T)
  } else {
    predict(mod, newdata = new_data, type = type, se.fit = T)
  }

  # Format output tibble
  tibble::tibble(
    linear_predictor = mod$linear.predictors,
    y_predicted = preds$fit,
    y_predicted_se = preds$se.fit,
    row_names = rownames(mod$data)
  )
}
