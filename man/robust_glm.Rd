% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/robust_glm.R
\name{robust_glm}
\alias{robust_glm}
\title{Fit a Generalized Linear Model with Robust Weights and Errors}
\usage{
robust_glm(
  formula,
  data,
  subset = NULL,
  family = "quasibinomial",
  robust_weights = T,
  sandwich = T,
  add_ci = T,
  p = NULL,
  ...
)
}
\arguments{
\item{formula}{A formula specifying the model.}

\item{data}{A data frame containing the variables in the model.}

\item{subset}{An optional character vector specifying a subset of observations
to be used in the model.}

\item{family}{A character string or function (see \code{lm()}) specifying
the distribution family in the GLM (default is "binomial").}

\item{robust_weights}{Logical indicating whether to compute robust model weights
(default is TRUE).}

\item{sandwich}{Logical indicating whether to compute sandwich standard errors
(default is TRUE).}

\item{add_ci}{Logical indicating whether to add confidence intervals to the model
coefficients (default is TRUE).}

\item{p}{An optional progressor object to monitor progress (default is NULL).}

\item{...}{Other arguments passed on to \code{stats::glm}.}
}
\value{
An object of class \code{"glm"} with additional attributes such as
confidence intervals, sandwich standard errors, and collinear terms.
}
\description{
This function fits a generalized linear model (GLM) with robust weights per
observation, to down-weight outliers, and robust sandwich standard errors,
to account for the lack of independance or heteroscedasticity.
}
\examples{
df <- data.frame(y = runif(50, 0, 1),
                 x1 = rep(1:2, 50),
                 x2 = runif(50, .5, 1),
                 x3 = runif(50, 0, .5))
fit <- robust_glm(y ~ x1 + x2, df, family = "quasibinomial")
summary(fit)

}
