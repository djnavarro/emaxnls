

#' Coefficents for an Emax regression
#'
#' @param object An `emaxnls` object
#' @param level Confidence level for interval estimate
#' @param ... Ignored
#'
#' @returns A tibble
#'
#' @exportS3Method stats::coef
coef.emaxnls <- function(object, level = 0.95, ...) {
  sss <- summary(.extract_nls(object))
  coef_tbl <- sss$coef
  ci <- .confint_quiet(.extract_nls(object), level = level)
  ci <- ci$result
  coef_tbl |>
    as.data.frame() |>
    tibble::rownames_to_column("label") |>
    tibble::as_tibble() |>
    dplyr::select(
      label = label,
      estimate = Estimate,
      std_error = `Std. Error`,
      t_statistic = `t value`,
      p_value = `Pr(>|t|)`
    ) |>
    dplyr::mutate(
      ci_lower = ci[, 1],
      ci_upper = ci[, 2]
    )
}

#' Variance-covariance matrix for an Emax regression
#' 
#' @param object An `emaxnls` object
#' @param ... Ignored
#'
#' @returns A matrix
#'
#' @exportS3Method stats::vcov
vcov.emaxnls <- function(object, ...) {
  vcov(.extract_nls(object), ...)
}

#' Residuals for an Emax regression
#'
#' @param object An `emaxnls` object
#' @param ... Ignored
#'
#' @returns Numeric vector of residuals
#'
#' @exportS3Method stats::residuals
residuals.emaxnls <- function(object, ...) {
  residuals(.extract_nls(object), ...)
}

#' Simulate responses from Emax regression model
#'
#' @param object An `emaxnls` object
#' @param nsim Number of replicates
#' @param seed Used to set RNG seed
#' @param ... Ignored
#'
#' @returns A data frame or tibble
#' 
#' @examples
#' mod <- emax_nls(
#'   structural_model = response_1 ~ exposure_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#' simulate(mod)
#'
#' @exportS3Method stats::simulate
simulate.emaxnls <- function(object, nsim = 1, seed = NULL, ...) {
  .emax_resample(
    mod = object,
    nsim = nsim,
    seed = seed
  )
}


#' Log-likelihood for an Emax regression model
#'
#' @param object An `emaxnls` object
#' @param REML For `nls` objects only `REML = FALSE` is supported
#' @param ... Ignored
#'
#' @returns Returns an object of class `logLik`. This is a number with 
#' at least one attribute, "df" (degrees of freedom), giving the 
#' number of (estimated) parameters in the model.
#'
#' @exportS3Method stats::logLik
logLik.emaxnls <- function(object, REML = FALSE, ...) {
  # logLik.nls doesn't support REML=TRUE; but let stats pkg handle the message
  stats::logLik(.extract_nls(object), REML = REML, ...) 
}


#' Akaike information criterion / Bayesian information criterion 
#'
#' @param object An `emaxnls` object
#' @param ... Optionally, more fitted model objects
#' @param k Penalty per parameter in the AIC
#'
#' @returns
#' If just one object is provided, a numeric value with the corresponding AIC (or BIC). 
#' If multiple objects are provided, a data.frame with rows corresponding to the objects 
#' and columns representing the number of parameters in the model (df) and the AIC or BIC.
#'
#' @name AIC
NULL

#' @exportS3Method stats::AIC
#' @rdname AIC
AIC.emaxnls <- function(object, ..., k = 2) {
  emaxnls_mods <- list(object, ...)
  nls_mods <- lapply(emaxnls_mods, .extract_nls)
  do.call(stats::AIC, nls_mods)
}

#' @exportS3Method stats::BIC
#' @rdname AIC
BIC.emaxnls <- function(object, ...) {
  emaxnls_mods <- list(object, ...)
  nls_mods <- lapply(emaxnls_mods, .extract_nls)
  do.call(stats::BIC, nls_mods)
}


#' Analysis of variance for Emax regression models
#'
#' @param object An `emaxnls` object
#' @param ... Additional fitted model objects
#'
#' @returns Analysis of variance tables for a sequence of `emaxnls` models
#'
#' @exportS3Method stats::anova
anova.emaxnls <- function(object, ...) {
  emaxnls_mods <- list(object, ...)
  nls_mods <- lapply(emaxnls_mods, .extract_nls)
  do.call(stats::anova, nls_mods)
}


#' Residual standard deviation for Emax regression models
#'
#' @param object An `emaxnls` object
#' @param ... Ignored
#'
#' @returns Numeric
#'
#' @exportS3Method stats::sigma
sigma.emaxnls <- function(object, ...) {
  evalq(stats::sigma(model), envir = object$env)
}


#' Number of observations for an Emax regression model
#'
#' @param object An `emaxnls` object
#' @param ... Ignored
#'
#' @returns Numeric
#'
#' @exportS3Method stats::nobs
nobs.emaxnls <- function(object, ...) {
  evalq(stats::nobs(model), envir = object$env)
}


#' Residual degrees of freedom for an Emax regression model
#'
#' @param object An `emaxnls` object
#' @param ... Ignored
#'
#' @returns Numeric
#'
#' @exportS3Method stats::df.residual
df.residual.emaxnls <- function(object, ...) {
  evalq(stats::df.residual(model), envir = object$env)
}


#' Model deviance for an Emax regression 
#'
#' @param object An `emaxnls` object
#' @param ... Ignored
#'
#' @returns Numeric
#'
#' @exportS3Method stats::deviance
deviance.emaxnls <- function(object, ...) {
  evalq(stats::deviance(model), envir = object$env)
}

#' Fitted values for an Emax regression 
#'
#' @param object An `emaxnls` object
#' @param ... Ignored
#'
#' @returns Numeric vector of fitted values
#'
#' @exportS3Method stats::fitted
fitted.emaxnls <- function(object, ...) {
  evalq(stats::fitted(model), envir = object$env)
}


#' Confidence intervals for Emax regression model parameters
#'
#' @param object An `emaxnls` object
#' @param parm A specification of which parameters are to be given confidence intervals, 
#' either a vector of numbers or a vector of names. If `parm = NULL`, all parameters are 
#' considered.
#' @param level The confidence level required
#' @param ... Ignored
#'
#' @returns
#' A matrix (or vector) with columns giving lower and upper confidence limits for each 
#' parameter. These will be labelled as (1-level)/2 and 1 - (1-level)/2 in % (by default 
#' 2.5% and 97.5%).
#' 
#' @exportS3Method stats::confint
confint.emaxnls <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    ci <- .confint_quiet(.extract_nls(object), level = level, ...)
  } else {
    ci <- .confint_quiet(.extract_nls(object), parm = parm, level = level, ...)
  }
  ci$result
}

#' Predicting from Emax regression models
#'
#' @param object An `emaxnls` object
#' @param newdata A named list or data frame in which to look for variables with which to predict. 
#' If `newdata` is missing the fitted values at the original data points are returned.
#' @param se.fit A switch indicating if standard errors are required.
#' @param interval A character string indicating if prediction intervals or a confidence interval 
#' on the mean responses are to be calculated. Can be "none", "confidence", or "prediction"
#' @param level A numeric scalar between 0 and 1 giving the confidence level for the intervals 
#' (if any) to be calculated.
#' @param ... Ignored
#'
#' @returns As `xgxr::predict.nls()`.
#'
#' @details
#' The `predict()` method for for Emax regression is a thin wrapper around `xgxr::predict.nls()`. 
#' Please see the documentation for that function. 
#' 
#' @exportS3Method stats::predict
predict.emaxnls <- function(object, 
                            newdata = NULL, 
                            se.fit = FALSE,
                            interval = "none",
                            level = 0.95, 
                            ...) {
  xgxr::predict.nls(
    object$env$model, 
    newdata, 
    se.fit, 
    interval, 
    level, 
    ...
  )
}