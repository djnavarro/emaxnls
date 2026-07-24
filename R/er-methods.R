
# Interoperability with erplots ------------------------------------------
#
# emaxnls has no hard dependency on erplots (a modeling package shouldn't
# need to pull in ggplot2/patchwork). But if erplots *is* installed and
# loaded, emaxnls's model objects should work seamlessly with erplots'
# model-agnostic plotting API, which relies on the
# `er_predict()`/`er_simulate()`/`er_summary()` generics defined in erplots
# (see `erplots::er_model_interface`).
#
# These methods are registered lazily at load time (via `.onLoad()` below),
# so that neither erplots nor its dependencies need to be installed for
# emaxnls's modeling functions to work standalone.
#
# Dispatch note: registering against class "emaxnls" is sufficient for both
# `emaxnls` and `emaxlogistic` objects, because `emaxlogistic` inherits from
# `emaxnls`. Internal calls to `predict()`, `coef()`, `vcov()`, `summary()`,
# and `emax_fun()` inside these methods will re-dispatch to the
# `emaxlogistic`-specific variants where they exist, so the three methods
# below work correctly for both subclasses.


# Helpers -----------------------------------------------------------------

# Augments `newdata` with reference-value columns for any covariates used by
# `model` that are not already columns of `newdata`. This is needed because
# erplots' model layer only passes the exposure variable (and optionally one
# stratification variable) to `er_predict()`/`er_simulate()`, but
# `predict.emaxnls()` and `emax_fun()` require *every* variable referenced
# in the expanded formula to be present.
#
# Reference values:
# - numeric columns: the column mean across `model$data`
# - factor/character columns: the first factor level of that column
#   (i.e., the conventional "baseline" category)
#
# Only covariate variables (param_type == "covariate" in model$info$variables)
# are ever added; structural variables (exposure, response) are never touched.
.emax_reference_data <- function(model, newdata) {
  vars <- model$info$variables
  needed <- vars$var_name[vars$param_type == "covariate"]
  needed <- stats::na.omit(unique(needed))
  missing_vars <- setdiff(needed, names(newdata))
  if (length(missing_vars) == 0L) return(newdata)
  aug <- newdata
  for (v in missing_vars) {
    col <- model$data[[v]]
    if (is.numeric(col)) {
      aug[[v]] <- mean(col, na.rm = TRUE)
    } else {
      aug[[v]] <- levels(factor(col))[1L]
    }
  }
  aug
}

# Computes a simple R-squared for an `emaxnls` (continuous) model:
# 1 - RSS/TSS, where TSS uses the response column from `model$data`.
# Returns NA_real_ for `emaxlogistic` models (R-squared is not meaningful
# for binary responses under the binomial link function).
.emax_r_squared <- function(model) {
  if (inherits(model, "emaxlogistic")) return(NA_real_)
  vars <- model$info$variables
  resp_var <- vars$var_name[vars$param_name == "response"]
  resp_var <- resp_var[!is.na(resp_var)][1L]
  y <- model$data[[resp_var]]
  rss <- stats::deviance(model)
  tss <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  1 - rss / tss
}


# er_predict() ------------------------------------------------------------

#' @param model A fitted `emaxnls` or `emaxlogistic` model object.
#' @param newdata A data frame containing at minimum the exposure variable.
#'   Any covariate columns required by the model but absent from `newdata`
#'   are filled in with reference values (numeric: column mean; factor/
#'   character: first factor level) from the original training data.
#' @param conf_level Confidence level for the interval. Defaults to `0.95`.
#' @param ... Additional arguments (currently ignored).
#' @noRd
er_predict.emaxnls <- function(model, newdata, conf_level = 0.95, ...) {
  aug  <- .emax_reference_data(model, newdata)
  pred <- stats::predict(
    model,
    newdata  = aug,
    se.fit   = FALSE,
    interval = "confidence",
    level    = conf_level
  )
  # predict.emaxnls() / predict.emaxlogistic() both return a data frame with
  # columns fit, lwr, upr when se.fit = FALSE and interval = "confidence".
  # predict.emaxlogistic() applies plogis() to all three columns when
  # type = "response" (the default), so predictions are already on the
  # probability scale for binary models.
  newdata$fit_resp <- pred[["fit"]]
  newdata$ci_lower <- pred[["lwr"]]
  newdata$ci_upper <- pred[["upr"]]
  newdata
}


# er_simulate() -----------------------------------------------------------

#' @param model A fitted `emaxnls` or `emaxlogistic` model object.
#' @param newdata A data frame containing at minimum the exposure variable.
#' @param nsim Number of parameter draws (curves) to simulate. Defaults to
#'   `100`. Note this default intentionally differs from `emaxnls`'s own
#'   `simulate()` default (`nsim = 1`); it matches the `erplots` contract.
#' @param seed Optional integer seed for reproducibility.
#' @param ... Additional arguments (currently ignored).
#' @noRd
er_simulate.emaxnls <- function(model, newdata, nsim = 100, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)

  aug <- .emax_reference_data(model, newdata)

  est  <- stats::coef(model)   # on log scale (back_transform = FALSE default)
  cov  <- stats::vcov(model)
  draws <- .rmvnorm(nsim, mean = est, sigma = cov)
  colnames(draws) <- names(est)

  # emax_fun() dispatches on model's actual class:
  # - emax_fun.emaxnls() returns the raw Emax curve value
  # - emax_fun.emaxlogistic() wraps it in plogis(), giving probabilities
  f <- emax_fun(model)

  # `sim_resp` is erplots' er_vpc_plot(model = ...) contract addition (see
  # `erplots::er_model_interface`): a full response-scale draw, adding
  # observation-level noise on top of `fit_resp`, mirroring the noise
  # models already used by simulate.emaxnls()/simulate.emaxlogistic()
  # (.emax_resample()/.emax_logistic_resample() in emaxnls-simulate.R /
  # emaxlogistic-methods.R): Bernoulli(fit_resp) for emaxlogistic,
  # Normal(fit_resp, sigma(model)) for emaxnls.
  is_binary <- inherits(model, "emaxlogistic")
  resid_sd  <- if (is_binary) NA_real_ else stats::sigma(model)

  reps <- vector("list", nsim)
  for (i in seq_len(nsim)) {
    row            <- newdata
    row$sim_id     <- i
    row$fit_resp   <- f(param = draws[i, ], data = aug)
    row$sim_resp   <- if (is_binary) {
      stats::rbinom(nrow(row), size = 1, prob = row$fit_resp)
    } else {
      stats::rnorm(nrow(row), mean = row$fit_resp, sd = resid_sd)
    }
    reps[[i]]      <- row
  }
  do.call(rbind, reps)
}


# er_summary() ------------------------------------------------------------

#' @param model A fitted `emaxnls` or `emaxlogistic` model object.
#' @param ... Additional arguments (currently ignored).
#' @noRd
er_summary.emaxnls <- function(model, ...) {
  smry <- summary(model)
  # summary() returns an emaxnls_null sentinel when the model did not converge
  if (inherits(smry, "emaxnls_null")) return(NULL)

  # The coefficient table returned by summary.emaxnls() / summary.emaxlogistic()
  # has columns: label, estimate, std_error, {t_statistic|z_statistic},
  # p_value, ci_lower, ci_upper.  Detect which test-statistic column is present
  # rather than assuming, so this method works for both model subclasses.
  stat_col <- intersect(c("t_statistic", "z_statistic"), names(smry))[1L]

  # `term` is set to the same value as `label` (e.g. "E0_cnt_a"). emaxnls does
  # not currently expose a separate bare-term name, so duplicating is the safe,
  # conservative choice.  The erplots-side consumer falls back to `term` when
  # `label` is absent, which would yield the same value anyway.
  coefficients <- data.frame(
    term      = smry$label,
    estimate  = smry$estimate,
    std_error = smry$std_error,
    statistic = smry[[stat_col]],
    p_value   = smry$p_value,
    conf_low  = smry$ci_lower,
    conf_high = smry$ci_upper,
    stringsAsFactors = FALSE
  )

  glance <- data.frame(
    n            = stats::nobs(model),
    df_residual  = stats::df.residual(model),
    logLik       = as.numeric(stats::logLik(model)),
    aic          = stats::AIC(model),
    bic          = stats::BIC(model),
    deviance     = stats::deviance(model),
    r_squared    = .emax_r_squared(model),
    converged    = !inherits(stats::coef(model), "emaxnls_null"),
    stringsAsFactors = FALSE
  )

  list(
    # Emax models have no single privileged parameter (the four structural
    # parameters — E0, Emax, EC50, Hill — are unrelated to each other), so
    # p_value is always NULL rather than an arbitrary choice.
    p_value      = NULL,
    coefficients = coefficients,
    glance       = glance
  )
}


# S3 registration ---------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  .s3_register("erplots::er_predict",  "emaxnls", er_predict.emaxnls)
  .s3_register("erplots::er_simulate", "emaxnls", er_simulate.emaxnls)
  .s3_register("erplots::er_summary",  "emaxnls", er_summary.emaxnls)
}

# Registers `method` as an S3 method for `generic` (given as
# "package::generic") and `class`, without requiring `package` to be
# installed or loaded. Vendored from the standard tidyverse pattern
# (e.g. vctrs::s3_register()) to avoid adding a dependency for one helper.
.s3_register <- function(generic, class, method) {
  pieces  <- strsplit(generic, "::")[[1L]]
  package <- pieces[[1L]]
  generic <- pieces[[2L]]

  register <- function(...) {
    envir <- asNamespace(package)
    registerS3method(generic, class, method, envir = envir)
  }

  if (isNamespaceLoaded(package)) {
    register()
  }
  setHook(packageEvent(package, "onLoad"), function(...) register())

  invisible()
}
