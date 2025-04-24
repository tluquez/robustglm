#' Tidy Model Terms
#'
#' This function tidies model terms, including coefficients, confidence intervals,
#' and sandwich standard errors, from a list of model objects or a single model object.
#'
#' @param model A list of or a single robust_glm object.
#' @param id A character string specifying the identifier column name
#'  (default is "id").
#' @param exponentiate Logical. Exponentiate estimate and confidence
#'  intervals.
#'
#' @return A data frame containing tidied model terms with columns:
#' \describe{
#'   \item{\code{id}}{Identifier column name.}
#'   \item{\code{term}}{Model term names.}
#'   \item{\code{estimate}}{Estimate of coefficients.}
#'   \item{\code{std.error}}{Standard error of coefficients.}
#'   \item{\code{statistic}}{Value of test statistics.}
#'   \item{\code{p.value}}{p-value of test statistics.}
#'   \item{\code{conf.low}}{Lower bound of confidence interval.}
#'   \item{\code{conf.high}}{Upper bound of confidence interval.}
#'   \item{\code{std.error_hc}}{Sandwich standard error of coefficients.}
#'   \item{\code{statistic_hc}}{Value of test statistics with sandwich
#'    standard errors.}
#'   \item{\code{p.value_hc}}{p-value of test statistics with sandwich
#'    standard errors.}
#'   \item{\code{conf.low_hc}}{Lower bound of confidence interval with
#'    sandwich standard errors.}
#'   \item{\code{conf.high_hc}}{Upper bound of confidence interval with
#'    sandwich standard errors.}
#'    \item{\code{estimate_exp}}{Exponentiated estimate.}
#'   \item{\code{conf.low_exp}}{Lower bound of exponentiated confidence
#'    interval.}
#'   \item{\code{conf.high_exp}}{Upper bound of exponentiated confidence
#'    interval.}
#'   \item{\code{std.error_exp}}{Exponentiated standard error.}
#'   \item{\code{std.error_hc_exp}}{Exponentiated sandwich standard error.}
#' }
#'
#' @importFrom broom tidy
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr left_join select rename_with
#' @importFrom purrr map_dfr reduce
#' @export

tidy_terms <- function(model, id = NULL, exponentiate = F) {
  # If lm or glm supplied, convert to a list
  if (inherits(model, c("lm", "glm"))) {
    model <- list(model)
  }

  # Check if model contains at least one model object
  if (length(model) == 0) {
    stop("model must contain at least one model object.")
  }

  # Tidy terms for each model object and combine results
  terms_df <- purrr::map_dfr(model, function(mod) {
    # Check if mod is a valid model object
    if (!inherits(mod, c("lm", "glm"))) {
      stop("model must be a valid 'lm' or 'glm' object.")
    }
    if (!inherits(mod, "robust_glm")) {
      stop("model must be of class 'robust_glm'.")
    }

    # Check if mod contains ci and sandwich, and they are not NA
    ci_df <- if (!is.null(mod$ci) && !all(is.na(mod$ci))) {
      mod$ci %>%
        as.data.frame() %>%
        tibble::rownames_to_column("term")
    } else {
      data.frame(term = names(coef(mod)), conf.low = NA, conf.high = NA)
    }

    sandwich_df <- if (!is.null(mod$sandwich) && !all(is.na(mod$sandwich))) {
      mod$sandwich %>%
        as.data.frame() %>%
        dplyr::select(-estimate) %>%
        dplyr::rename_with(~ paste0(.x, "_hc")) %>%
        tibble::rownames_to_column("term")
    } else {
      data.frame(term = names(coef(mod)), std.error_hc = NA, statistic_hc = NA,
                 p.value_hc = NA, conf.low_hc = NA, conf.high_hc = NA)
    }

    # Tidy model terms and combine with ci and sandwich
    df <- purrr::reduce(list(broom::tidy(mod), ci_df, sandwich_df),
                        dplyr::left_join, by = "term")

    # Exponentiate estimate and confidence intervals
    if (exponentiate) {
      df <- df %>%
        dplyr::mutate(dplyr::across(dplyr::matches("conf|estimate"), exp,
                                    .names = "{.col}_exp"))
    }

    return(df)
  }, .id = id)

  return(terms_df)
}

#' Tidy Model Summary Statistics
#'
#' Tidies summary statistics of model objects, such as convergence status,
#' dispersion, and performance metrics.
#'
#' @param model A list of or a single robust_glm object.
#' @param id A character string specifying the identifier column name
#'  (default is "id").
#' @param ... Arguments passed to \code{broom::tidy()}.
#'
#' @return A data frame containing tidied model summary statistics with
#'  columns:
#' \describe{
#'   \item{\code{id}}{Identifier column name.}
#'   \item{\code{converged}}{Convergence status of the model.}
#'   \item{\code{dispersion}}{Dispersion value of the model.}
#'   \item{\code{glance}}{Summary statistics of the model.}
#'   \item{\code{rmse}}{Root mean square error.}
#'   \item{\code{mse}}{Mean square error.}
#'   \item{\code{hosmer_chisq}}{Hosmer goodness-of-fit test statistic.}
#'   \item{\code{hosmer_df}}{Degrees of freedom for Hosmer goodness-of-fit
#'    test.}
#'   \item{\code{hosmer_p.value}}{p-value of Hosmer goodness-of-fit test.}
#'   \item{\code{R2_Tjur}}{Tjur's R-squared.}
#'   \item{\code{Log_loss}}{Logarithmic loss.}
#' }
#'
#' @importFrom purrr map_dfr
#' @importFrom broom glance
#' @importFrom performance performance_rmse performance_mse performance_hosmer
#' @importFrom performance r2_tjur performance_logloss
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export

tidy_model <- function(model, id = NULL, ...) {
  # If lm or glm supplied, convert to a list
  if (inherits(model, c("lm", "glm"))) {
    model <- list(model)
  }

  # Check if model contains at least one model object
  if (length(model) == 0) {
    stop("Model must contain at least one model object.")
  }

  # Tidy summary statistics for each model object and combine results
  mods_df <- purrr::map_dfr(model, function(mod) {
    # Check if mod is a valid model object
    if (!inherits(mod, "lm") && !inherits(mod, "glm")) {
      stop("Model must be a valid 'lm' or 'glm' object.")
    }
    if (!inherits(mod, "robust_glm")) {
      stop("model must be of class 'robust_glm'.")
    }

    # Extract summary statistics from the model object
    summary_stats <- list(
      converged = tryCatch(mod$converged, error = function(e) NA),
      dispersion = tryCatch(
        if ("dispersion" %in% names(summary(mod))) summary(mod)$dispersion else NA,
        error = function(e) NA
      ),
      rmse = tryCatch(performance::performance_rmse(mod),
                      error = function(e) NA),
      mse = tryCatch(performance::performance_mse(mod),
                     error = function(e) NA),
      hosmer_chisq = tryCatch(performance::performance_hosmer(mod)$chisq,
                              error = function(e) NA),
      hosmer_df = tryCatch(performance::performance_hosmer(mod)$df,
                           error = function(e) NA),
      hosmer_p.value = tryCatch(performance::performance_hosmer(mod)$p.value,
                                error = function(e) NA),
      R2_Tjur = tryCatch(performance::r2_tjur(mod)[[1]],
                         error = function(e) NA),
      Log_loss = tryCatch(performance::performance_logloss(mod)[[1]],
                          error = function(e) NA)
    )
    glance_df <- tryCatch(broom::glance(mod, ...), error = function(e) NA)
    if (!is.null(glance_df)) summary_stats <- c(as.list(glance_df),
                                                summary_stats)

    # Combine summary statistics into a data frame
    tibble::as_tibble(summary_stats)
  }, .id = id)

  return(mods_df)
}