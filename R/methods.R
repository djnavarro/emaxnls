

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
  sss <- summary(object$result)
  coef_tbl <- sss$coef
  ci <- nlstools::confint2(object$result, level = level)
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
#'#' @param object An `emaxnls`` object

#' @param object An `emaxnls` object
#' @param ... Ignored
#'
#' @returns A matrix
#'
#' @exportS3Method stats::vcov
vcov.emaxnls <- function(object, ...) {
  vcov(object$result, ...)
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
  residuals(object$result, ...)
}

#' Print an Emax regression model object
#'
#' @param x An `emaxnls` object
#' @param ... Ignored
#'
#' @returns Invisibly returns the original object
#'
#' @exportS3Method base::print
print.emaxnls <- function(x, ...) {

  cat("Structural model:\n\n")
  cat("  Exposure: ", as.character(x$variables$exposure), "\n")
  cat("  Response: ", as.character(x$variables$response), "\n")
  cat("  Emax type:", x$model_type, "\n\n")
  cat("Covariate model:\n\n")
  cat("  E0:      ", deparse(x$covariate_model$E0), "\n")
  cat("  Emax:    ", deparse(x$covariate_model$Emax), "\n")
  cat("  logEC50: ", deparse(x$covariate_model$logEC50), "\n")
  if (x$model_type == "sigmoidal") {
    cat("  logHill: ", deparse(x$covariate_model$logHill), "\n")
  }
  cat("\n")
  if(is.null(x$result)) {
    cat("Model does not converge\n")
  } else {
    cat("Coefficient table:\n\n")
    ccc <- utils::capture.output(print(coef(x)))
    ccc <- ccc[c(-1, -3)]
    cat(ccc, sep = "\n")
    cat("\n")
    cat("Variance-covariance matrix:\n\n")
    print(vcov(x), digits = 2)
  }

  return(invisible(x))
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
  stats::logLik(object$result, REML = REML, ...) 
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
  mods <- list(object, ...)
  nls_mods <- lapply(mods, function(x) x$result)
  do.call(stats::AIC, nls_mods)
}

#' @exportS3Method stats::BIC
#' @rdname AIC
BIC.emaxnls <- function(object, ...) {
  mods <- list(object, ...)
  nls_mods <- lapply(mods, function(x) x$result)
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
  mods <- list(object, ...)
  nls_mods <- lapply(mods, function(x) x$result)
  do.call(stats::anova, nls_mods)
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
  .confint_quiet <- purrr::quietly(stats::confint)
  if (is.null(parm)) {
    ci <- .confint_quiet(object$result, level = level, ...)
  } else {
    ci <- .confint_quiet(object$result, parm = parm, level = level, ...)
  }
  ci$result
}

#' Predicting from Emax regression models
#'
#' @param object An `emaxnls` object
#' @param newdata A named list or data frame in which to look for variables with which to predict. 
#' If `newdata` is missing the fitted values at the original data points are returned.
#' @param interval A character string indicating if prediction intervals or a confidence interval 
#' on the mean responses are to be calculated.
#' @param level A numeric scalar between 0 and 1 giving the confidence level for the intervals 
#' (if any) to be calculated.
#' @param ... Ignored
#'
#' @returns The `predict()` method for Emax regression objects always returns a data frame or tibble
#'
#' @details
#' Note that `predict()` for Emax regression calls `investr::predFit()` to obtain confidence and 
#' prediction intervals. Please see the documentation for that function. 
#' 
#' @exportS3Method stats::predict
predict.emaxnls <- function(object, 
                                newdata, 
                                interval = c("none", "confidence", "prediction"),
                                level = 0.95, 
                                ...
) {
  
}