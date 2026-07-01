
# the nls_null class is used as a return value when the model does not converge 
.nls_null <- function() {
  structure(NA_real_, class = "emaxnls_null")
}

#' @exportS3Method stats::logLik
logLik.emaxnls_null <- function(object, REML = FALSE, ...) {
  .nls_null()
}

#' @exportS3Method base::print
print.emaxnls_null <- function(x, ...) {
 cat("model does not converge") 
}

#' Coefficients for an Emax regression
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param back_transform Should log-scaled parameters (logEC50, logHill) be back-transformed to original scale?
#' @param ... Ignored
#'
#' @returns A vector of coefficients
#' 
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#' 
#' # coefficients on the estimation scale
#' coef(mod_c)
#' 
#' # coefficients with log-scale parameters back-transformed
#' coef(mod_c, back_transform = TRUE)
#' 
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' coef(mod_b)
#'
#' @exportS3Method stats::coef
coef.emaxnls <- function(object, back_transform = FALSE, ...) {
  if (!.is_converged(object)) return(.nls_null())
  cc <- stats::coef(.get_nls(object), ...)
  if (back_transform) {
    trans_cases <- grep("^log", names(cc))
    names(cc) <- gsub("^log", "", names(cc))
    cc[trans_cases] <- exp(cc[trans_cases])
  } 
  cc
}

#' Variance-covariance matrix for an Emax regression
#' 
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param ... Ignored
#'
#' @returns A matrix
#' 
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#' vcov(mod_c)
#' 
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' vcov(mod_b)
#'
#' @exportS3Method stats::vcov
vcov.emaxnls <- function(object, ...) {
  if (!.is_converged(object)) return(.nls_null())
  stats::vcov(.get_nls(object), ...)
}

#' Residuals for an Emax regression model
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param type For `emaxlogistic` objects: the type of residuals to return.
#'   `"pearson"` (default) returns Pearson residuals; `"deviance"` returns
#'   deviance residuals. Ignored for `emaxnls` objects.
#' @param ... Ignored
#'
#' @returns A numeric vector of residuals
#'
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' residuals(mod_c)[1:20]
#'
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' residuals(mod_b)[1:20]
#' residuals(mod_b, type = "deviance")[1:20]
#'
#' @name residuals
#' @exportS3Method stats::residuals
residuals.emaxnls <- function(object, ...) {
  if (!.is_converged(object)) return(.nls_null())
  stats::residuals(.get_nls(object), ...)
}

#' Simulate responses from an Emax regression model
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param nsim Number of replicates
#' @param seed Used to set RNG seed
#' @param ... Ignored
#'
#' @details
#' The `simulate()` method samples new parameter values from the multivariate
#' normal distribution implied by the estimated covariance matrix, then
#' simulates responses at those parameter values using
#' `mvtnorm::rmvnorm()`. For `emaxlogistic` objects, predicted probabilities
#' are computed from each parameter draw and binary outcomes are drawn from
#' `Bernoulli(p)` for each observation.
#'
#' @returns A data frame with `nsim` columns named `sim_1`, `sim_2`, etc.
#'
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' if (requireNamespace("mvtnorm", quietly = TRUE)) simulate(mod_c)
#'
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' if (requireNamespace("mvtnorm", quietly = TRUE)) simulate(mod_b)
#'
#' @name simulate
#' @exportS3Method stats::simulate
simulate.emaxnls <- function(object, nsim = 1, seed = NULL, ...) {
  if (!.is_converged(object)) return(.nls_null())
  .emax_resample(
    mod = object,
    nsim = nsim,
    seed = seed
  )
}


#' Log-likelihood for an Emax regression model
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param REML For `emaxnls` objects only `REML = FALSE` is supported.
#'   Ignored for `emaxlogistic` objects.
#' @param ... Ignored
#'
#' @returns An object of class `logLik` with at least one attribute, `"df"`
#'   (degrees of freedom), giving the number of estimated parameters in the
#'   model.
#'
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' logLik(mod_c)
#'
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' logLik(mod_b)
#'
#' @name logLik
#' @exportS3Method stats::logLik
logLik.emaxnls <- function(object, REML = FALSE, ...) {
  if (!.is_converged(object)) return(.nls_null())
  # logLik.nls doesn't support REML=TRUE; but let stats pkg handle the message
  stats::logLik(.get_nls(object), REML = REML, ...) 
}


#' Akaike information criterion / Bayesian information criterion 
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param ... Optionally, more fitted model objects
#' @param k Penalty per parameter in the AIC
#'
#' @returns
#' If just one object is provided, a numeric value with the corresponding AIC (or BIC). 
#' If multiple objects are provided, a data.frame with rows corresponding to the objects 
#' and columns representing the number of parameters in the model (`df`) and the AIC or BIC.
#'
#' @examples
#' mod_0 <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' mod_1 <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#'
#' # calculate AIC for individual models
#' AIC(mod_0)
#' AIC(mod_1)
#'
#' # calculate AIC for a sequence of models
#' AIC(mod_0, mod_1)
#'
#' # calculate BIC for individual models
#' BIC(mod_0)
#' BIC(mod_1)
#'
#' # calculate BIC for a sequence of models
#' BIC(mod_0, mod_1)
#'
#' # emaxlogistic models
#' mod_b0 <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' mod_b1 <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' AIC(mod_b0, mod_b1)
#' BIC(mod_b0, mod_b1)
#'
#' @name AIC
NULL

#' @exportS3Method stats::AIC
#' @rdname AIC
AIC.emaxnls <- function(object, ..., k = 2) {
  emaxnls_mods <- list(object, ...)
  x <- match.call()
  mod_names <- unlist(unname(lapply(as.list(x[-1]), as.character)))
  mod_names <- mod_names[seq_along(emaxnls_mods)]
  if (length(emaxnls_mods) == 1L & !.is_converged(object)) return(.nls_null())
  converged <- unlist(.map(emaxnls_mods, .is_converged))
  if (!all(converged)) {
    .warn("dropping non-converging models")
    emaxnls_mods <- emaxnls_mods[converged]
    mod_names <- mod_names[converged]
  }
  if (length(emaxnls_mods) == 1L) return(stats::AIC(.get_nls(object))) 
  nls_mods <- .map(
    .x = emaxnls_mods, 
    .f = function(mm) {
      nls_mod <- .get_nls(mm)
      if (is.null(nls_mod)) nls_mod <- .nls_null()
      nls_mod
    }
  )
  aic_vals <- unlist(.map(
    .x = nls_mods,
    .f = stats::AIC
  ))
  df_vals <- unlist(.map(
    .x = emaxnls_mods,
    .f = function(mm) evalq(stats::df.residual(model), envir = mm$env)
  ))
  out <- data.frame(df = df_vals, AIC = aic_vals, row.names = mod_names)
  out
}

#' @exportS3Method stats::BIC
#' @rdname AIC
BIC.emaxnls <- function(object, ...) {
  emaxnls_mods <- list(object, ...)
  x <- match.call()
  mod_names <- unlist(unname(lapply(as.list(x[-1]), as.character)))
  mod_names <- mod_names[seq_along(emaxnls_mods)]
  if (length(emaxnls_mods) == 1L & !.is_converged(object)) return(.nls_null())
  converged <- unlist(.map(emaxnls_mods, .is_converged))
  if (!all(converged)) {
    .warn("dropping non-converging models")
    emaxnls_mods <- emaxnls_mods[converged]
    mod_names <- mod_names[converged]
  }
  if (length(emaxnls_mods) == 1L) return(stats::BIC(.get_nls(object))) 
  nls_mods <- .map(
    .x = emaxnls_mods, 
    .f = function(mm) {
      nls_mod <- .get_nls(mm)
      if (is.null(nls_mod)) nls_mod <- .nls_null()
      nls_mod
    }
  )
  bic_vals <- unlist(.map(
    .x = nls_mods,
    .f = stats::BIC
  ))
  df_vals <- unlist(.map(
    .x = emaxnls_mods,
    .f = function(mm) evalq(stats::df.residual(model), envir = mm$env)
  ))
  out <- data.frame(df = df_vals, BIC = bic_vals, row.names = mod_names)
  out
}


#' Analysis of variance for Emax regression models
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param ... Additional fitted model objects of the same class
#'
#' @details
#' For `emaxnls` objects, calls `stats::anova()` on the underlying `nls`
#' objects to produce an ANOVA table for the sequence of models. For
#' `emaxlogistic` objects, computes a likelihood ratio chi-squared test
#' comparing nested models; the test statistic is the difference in
#' deviances and the reference distribution is chi-squared with degrees
#' of freedom equal to the difference in the number of parameters.
#'
#' @returns For `emaxnls` objects, an analysis of variance table for a
#'   sequence of models. For `emaxlogistic` objects, a data frame with
#'   columns `Df`, `Deviance`, `Df_diff`, `LRT`, and `Pr(>Chi)`.
#'
#' @examples
#' mod_0 <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' mod_1 <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' anova(mod_0, mod_1)
#'
#' # emaxlogistic: likelihood ratio test
#' mod_b0 <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' mod_b1 <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' anova(mod_b0, mod_b1)
#'
#' @name anova
#' @exportS3Method stats::anova
anova.emaxnls <- function(object, ...) {
  emaxnls_mods <- list(object, ...)
  converged <- unlist(.map(emaxnls_mods, .is_converged))
  if (!all(converged)) {
    .warn("dropping non-converging models")
    emaxnls_mods <- emaxnls_mods[converged]
  }
  if (length(emaxnls_mods) < 2L) {
    .warn("anova is only defined for sequences of models")
    return(NULL)
  }
  nls_mods <- .map(emaxnls_mods, .get_nls)
  do.call(stats::anova, nls_mods)
}


#' Residual standard deviation for Emax regression models
#'
#' @param object An `emaxnls` object
#' @param ... Ignored
#'
#' @returns Numeric
#' 
#' @examples
#' mod <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#' sigma(mod)
#'
#' @exportS3Method stats::sigma
sigma.emaxnls <- function(object, ...) {
  if (!.is_converged(object)) return(.nls_null())  
  evalq(stats::sigma(model), envir = object$env)
}


#' Number of observations for an Emax regression model
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param ... Ignored
#'
#' @returns Numeric
#' 
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' nobs(mod_c)
#'
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' nobs(mod_b)
#'
#' @exportS3Method stats::nobs
nobs.emaxnls <- function(object, ...) {
  if (!.is_converged(object)) return(.nls_null())
  evalq(stats::nobs(model), envir = object$env)
}


#' Residual degrees of freedom for an Emax regression model
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param ... Ignored
#'
#' @returns Numeric
#'
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' df.residual(mod_c)
#'
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' df.residual(mod_b)
#'
#' @name df.residual
#' @exportS3Method stats::df.residual
df.residual.emaxnls <- function(object, ...) {
  if (!.is_converged(object)) return(.nls_null())
  evalq(stats::df.residual(model), envir = object$env)
}


#' Model deviance for an Emax regression model
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param ... Ignored
#'
#' @returns Numeric. For `emaxnls` objects, returns the residual sum of
#'   squares. For `emaxlogistic` objects, returns the binomial deviance
#'   (`-2 * logLik`).
#'
#' @examples
#' # emaxnls deviance (residual sum of squares)
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' deviance(mod_c)
#'
#' # emaxlogistic deviance (binomial deviance)
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' deviance(mod_b)
#'
#' @name deviance
#' @exportS3Method stats::deviance
deviance.emaxnls <- function(object, ...) {
  if (!.is_converged(object)) return(.nls_null())
  evalq(stats::deviance(model), envir = object$env)
}

#' Fitted values for an Emax regression model
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param type For `emaxlogistic` objects: `"response"` (default) returns
#'   fitted probabilities; `"link"` returns the linear predictor on the
#'   logit scale. Ignored for `emaxnls` objects.
#' @param ... Ignored
#'
#' @returns A numeric vector of fitted values
#'
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' fitted(mod_c)[1:20]
#'
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' fitted(mod_b)[1:20]
#' fitted(mod_b, type = "link")[1:20]
#'
#' @name fitted
#' @exportS3Method stats::fitted
fitted.emaxnls <- function(object, ...) {
  if (!.is_converged(object)) return(.nls_null())
  evalq(stats::fitted(model), envir = object$env)
}


#' Confidence intervals for Emax regression model parameters
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param parm A specification of which parameters are to be given confidence intervals,
#'   either a vector of numbers or a vector of names. If `parm = NULL`, all parameters
#'   are considered.
#' @param level The confidence level required
#' @param back_transform Should log-scaled parameters (logEC50, logHill) be back-transformed to original scale?
#' @param ... Ignored
#'
#' @returns A matrix (or vector) with columns giving lower and upper confidence limits
#'   for each parameter. These will be labeled as (1-level)/2 and 1 - (1-level)/2 in %
#'   (by default 2.5% and 97.5%).
#'
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#'
#' # 95% confidence interval on the estimation scale
#' confint(mod_c)
#'
#' # 90% confidence interval on the estimation scale
#' confint(mod_c, level = 0.9)
#'
#' # 95% confidence interval with log-scale parameters back-transformed
#' confint(mod_c, back_transform = TRUE)
#'
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' confint(mod_b)
#'
#' @exportS3Method stats::confint
confint.emaxnls <- function(object, parm = NULL, level = 0.95, back_transform = FALSE, ...) {
  if (!.is_converged(object)) return(.nls_null())
  if (is.null(parm)) {
    ci <- .confint_quiet(.get_nls(object), level = level, ...)
  } else {
    ci <- .confint_quiet(.get_nls(object), parm = parm, level = level, ...)
  }
  ci <- ci$result

  if (back_transform) {
    trans_cases <- grep("^log", rownames(ci))
    rownames(ci) <- gsub("^log", "", rownames(ci))
    ci[trans_cases,] <- exp(ci[trans_cases,])
  } 

  ci
}

#' Predicting from Emax regression models
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param newdata A named list or data frame in which to look for variables with which to predict.
#'   If `newdata` is missing the fitted values at the original data points are returned.
#' @param type For `emaxlogistic` objects: `"response"` (default) returns predicted
#'   probabilities; `"link"` returns the linear predictor on the logit scale. Ignored
#'   for `emaxnls` objects.
#' @param se.fit A switch indicating if standard errors are required.
#' @param interval A character string indicating if prediction intervals or a confidence
#'   interval on the mean responses are to be calculated. Can be `"none"`, `"confidence"`,
#'   or `"prediction"`.
#' @param level A numeric scalar between 0 and 1 giving the confidence level for the
#'   intervals (if any) to be calculated.
#' @param ... Ignored
#'
#' @returns The return value differs slightly depending on inputs. When `se.fit = FALSE`,
#'   it produces a vector or matrix of predictions with column names `fit`, `lwr` and `upr`
#'   if the `interval` argument is set. When `se.fit = TRUE`, it returns a list with the
#'   following components:
#'
#' - `fit`: vector or matrix as above
#' - `se.fit`: standard error of the predicted means
#' - `residual.scale`: residual standard deviation
#' - `df`: residual degrees of freedom
#'
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#'
#' # return a vector of predictions
#' predict(mod_c)[1:20]
#'
#' # return a matrix with confidence intervals
#' predict(mod_c, interval = "confidence", se.fit = FALSE)
#'
#' # emaxlogistic predicted probabilities
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' predict(mod_b)[1:20]
#' predict(mod_b, type = "link")[1:20]
#'
#' @name predict
#' @exportS3Method stats::predict
predict.emaxnls <- function(object, 
                            newdata = NULL, 
                            se.fit = FALSE,
                            interval = "none",
                            level = 0.95, 
                            ...) {
  if (!.is_converged(object)) return(.nls_null())
  .predict_nls(
    object$env$model, 
    newdata, 
    se.fit, 
    interval, 
    level, 
    ...
  )
}

#' Summary of an Emax regression model
#'
#' @param object An `emaxnls` object
#' @param conf_level Confidence level for interval estimates
#' @param back_transform Should log-scaled parameters (logEC50, logHill) be back-transformed to original scale?
#' @param ... Ignored
#'
#' @returns A data frame or tibble containing a table of parameter estimates and other statistical summaries. 
#' Please note that the `summary()` method is experimental (moreso than other methods), and the return value 
#' may be modified in future releases as the package matures.
#'
#' @exportS3Method base::summary
#' 
#' @examples 
#' mod <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#' 
#' # standard summary
#' summary(mod)
#' 
#' # summary with adjusted confidence level
#' summary(mod, conf_level = 0.99)
#' 
#' # summary with log-scale parameters transformed to original scale
#' summary(mod, back_transform = TRUE)
#' 
summary.emaxnls <- function(object, conf_level = 0.95, back_transform = FALSE, ...) {
  if (!.is_converged(object)) return(.nls_null())
  .coef_table(object = object, level = conf_level, back_transform = back_transform)
}
