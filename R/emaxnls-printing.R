
#' Print an Emax regression model object
#'
#' The `print()` method for `emaxnls` and `emaxlogistic` objects provides a
#' concise model overview: the structural and covariate formulas, key fit
#' statistics, and a coefficient table showing estimates and confidence
#' intervals. Hypothesis tests are deliberately omitted from the printed
#' output; use `summary()` for inferential results.
#'
#' @param x An `emaxnls` or `emaxlogistic` object
#' @param conf_level Confidence level for the coefficient intervals shown in
#'   the printed table. Defaults to 0.95.
#' @param ... Ignored
#'
#' @returns Invisibly returns the original object
#'
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' print(mod_c)
#'
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' print(mod_b)
#'
#' @name print
#' @exportS3Method base::print
print.emaxnls <- function(x, conf_level = 0.95, ...) {

  cat("Structural model:\n\n")
  cat("  Exposure: ", as.character(.get_exposure_name(x)), "\n")
  cat("  Response: ", as.character(.get_response_name(x)), "\n")
  cat("  Emax type:", .get_model_type(x), "\n\n")
  cat("Covariate model:\n\n")
  cat("  E0:      ", deparse(.get_covariate_formula(x, "E0")), "\n")
  cat("  Emax:    ", deparse(.get_covariate_formula(x, "Emax")), "\n")
  cat("  logEC50: ", deparse(.get_covariate_formula(x, "logEC50")), "\n")
  if (.get_model_type(x) == "sigmoidal") {
    cat("  logHill: ", deparse(.get_covariate_formula(x, "logHill")), "\n")
  }
  cat("\n")

  if (is.null(.get_nls(x))) {
    cat("Model does not converge\n")
  } else {
    cat("Model fit:\n\n")
    cat("  Observations:        ", stats::nobs(x), "\n")
    cat("  Residual df:         ", stats::df.residual(x), "\n")
    cat("  Residual std. error: ", round(stats::sigma(x), 4L), "\n")
    cat("  AIC:                 ", round(stats::AIC(x), 4L), "\n\n")

    tbl <- .coef_table(x, level = conf_level)
    tbl <- tbl[, c("label", "estimate", "std_error", "ci_lower", "ci_upper")]
    names(tbl)[4:5] <- c("lower", "upper")

    cat("Coefficients (", round(conf_level * 100), "% CI):\n\n", sep = "")
    ccc <- utils::capture.output(print(tbl))
    ccc <- ccc[c(-1, -3)]
    cat(ccc, sep = "\n")
    cat("\nUse summary() for hypothesis tests.\n")
  }

  return(invisible(x))
}


#' Summary of an Emax regression model
#'
#' Returns a tidy coefficient table for a fitted `emaxnls` or `emaxlogistic`
#' model, combining parameter estimates, standard errors, test statistics,
#' p-values, and confidence intervals.
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param conf_level Confidence level for interval estimates. Defaults to 0.95.
#' @param back_transform Should logEC50 and logHill parameters be
#'   back-transformed to the concentration scale? If `TRUE`, these parameters
#'   are exponentiated and renamed to `EC50` and `Hill` respectively, and
#'   their confidence intervals are transformed accordingly. Standard errors
#'   and test statistics on the back-transformed scale are not available and
#'   are set to `NA`.
#' @param p_adjust Method for adjusting p-values for multiple comparisons,
#'   passed to [stats::p.adjust()]. Defaults to `"none"`. Parameters with
#'   suppressed p-values (see `suppress_nonsensical`) are excluded from the
#'   adjustment set.
#' @param simultaneous If `TRUE`, compute simultaneous (joint) confidence
#'   intervals using the multivariate normal distribution via
#'   [mvtnorm::qmvnorm()]. This gives wider intervals that provide joint
#'   coverage at `conf_level` across all parameters simultaneously. Defaults
#'   to `FALSE`.
#' @param suppress_nonsensical If `TRUE` (the default), suppress the test
#'   statistic and p-value for `logEC50_Intercept`. The logEC50 intercept is
#'   estimated on the log-concentration scale, and testing `H0: logEC50 = 0`
#'   is equivalent to testing whether EC50 equals 1 on the concentration
#'   scale — a threshold with no general pharmacometric meaning. The
#'   confidence interval for logEC50 is always reported regardless of this
#'   setting. Pass `suppress_nonsensical = FALSE` to restore the raw test
#'   results.
#' @param ... Ignored
#'
#' @details
#' ## Which tests are reported by default
#'
#' Most parameters have a meaningful point null at zero:
#' - `Emax_Intercept`: tests whether any exposure-response relationship exists.
#' - `logHill_Intercept`: tests whether logHill = 0, i.e., whether the Hill
#'   parameter equals 1 on the concentration scale, which would mean the
#'   sigmoidal model reduces to a hyperbolic one.
#' - `E0_Intercept`: tests whether the baseline response is zero. This is
#'   informative when the outcome is expressed as change from baseline, a
#'   common convention in pharmacometrics.
#' - Covariate beta terms: test whether a given covariate has any effect on
#'   the corresponding structural parameter.
#'
#' The one exception is `logEC50_Intercept`. The model is parameterised in
#' terms of logEC50 (on the log-concentration scale), not EC50 directly, so
#' the null `H0: logEC50 = 0` corresponds to testing EC50 = 1 on the
#' concentration scale — a value with no intrinsic pharmacometric meaning.
#' By default, the test statistic and p-value for `logEC50_Intercept` are
#' suppressed (set to `NA`), while the confidence interval for logEC50 is
#' retained. To work with the EC50 on the concentration scale, use
#' `back_transform = TRUE`.
#'
#' ## Simultaneous intervals
#'
#' When `simultaneous = TRUE`, a single critical value is derived from the
#' joint multivariate normal distribution of the standardised parameter
#' estimates, using the correlation matrix implied by `vcov()`. The resulting
#' intervals have simultaneous coverage at `conf_level` and will be wider
#' than the individual (pointwise) intervals.
#'
#' @returns A tibble with one row per model parameter and columns for the
#'   estimate, standard error, test statistic, p-value, and confidence
#'   interval bounds. The column for the test statistic is named
#'   `t_statistic` for `emaxnls` models and `z_statistic` for `emaxlogistic`
#'   models. The return format is experimental and may change in future
#'   releases.
#'
#' @exportS3Method base::summary
#'
#' @examples
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#'
#' # standard summary (logEC50_Intercept p-value suppressed by default)
#' summary(mod_c)
#'
#' # show all tests, including the logEC50 intercept
#' summary(mod_c, suppress_nonsensical = FALSE)
#'
#' # Bonferroni-adjusted p-values
#' summary(mod_c, p_adjust = "bonferroni")
#'
#' # simultaneous confidence intervals
#' summary(mod_c, simultaneous = TRUE)
#'
#' # adjusted confidence level
#' summary(mod_c, conf_level = 0.99)
#'
#' # back-transform logEC50 and logHill to concentration scale
#' summary(mod_c, back_transform = TRUE)
#'
#' # logistic emax equivalent
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' summary(mod_b)
#'
#' @name summary
summary.emaxnls <- function(object, conf_level = 0.95, back_transform = FALSE,
                             p_adjust = "none", simultaneous = FALSE,
                             suppress_nonsensical = TRUE, ...) {
  if (!.is_converged(object)) return(.nls_null())
  .coef_table(
    object               = object,
    level                = conf_level,
    back_transform       = back_transform,
    p_adjust             = p_adjust,
    simultaneous         = simultaneous,
    suppress_nonsensical = suppress_nonsensical
  )
}


# construct the coefficient table for continuous Emax models
.coef_table <- function(object, level = 0.95, back_transform = FALSE,
                        p_adjust = "none", simultaneous = FALSE,
                        suppress_nonsensical = FALSE, ...) {
  sss <- summary(.get_nls(object))
  coef_tbl <- sss$coef

  if (simultaneous) {
    ci <- .simultaneous_ci(object, level = level)
  } else {
    ci <- tryCatch(
      {
        ci_try <- .confint_quiet(.get_nls(object), level = level)
        ci_try$result
      },
      error = function(e) {
        .warn(paste0(
          "Profile likelihood confidence intervals failed; ",
          "falling back to Wald intervals."
        ))
        .wald_ci(
          .get_nls(object), level = level,
          df = stats::df.residual(.get_nls(object))
        )
      }
    )
  }

  coef_tbl <- as.data.frame(coef_tbl)
  coef_tbl <- .rownames_to_column(coef_tbl, "label")
  coef_tbl <- .as_tibble(coef_tbl)
  coef_tbl <- coef_tbl[, c("label", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  names(coef_tbl) <- c("label", "estimate", "std_error", "t_statistic", "p_value")

  coef_tbl$ci_lower <- ci[, 1]
  coef_tbl$ci_upper <- ci[, 2]

  # suppress before back_transform so the logEC50 label is still in its
  # original form when the pattern match runs
  if (suppress_nonsensical) {
    coef_tbl <- .suppress_nonsensical_pvalues(coef_tbl)
  }

  if (back_transform) {
    trans_cases <- grep("^log", coef_tbl$label)
    coef_tbl$label <- gsub("^log", "", coef_tbl$label)
    coef_tbl$estimate[trans_cases]    <- exp(coef_tbl$estimate[trans_cases])
    coef_tbl$std_error[trans_cases]   <- NA_real_
    coef_tbl$t_statistic[trans_cases] <- NA_real_
    coef_tbl$p_value[trans_cases]     <- NA_real_
    coef_tbl$ci_lower[trans_cases]    <- exp(coef_tbl$ci_lower[trans_cases])
    coef_tbl$ci_upper[trans_cases]    <- exp(coef_tbl$ci_upper[trans_cases])
  }

  if (p_adjust != "none") {
    non_na <- !is.na(coef_tbl$p_value)
    coef_tbl$p_value[non_na] <- stats::p.adjust(
      coef_tbl$p_value[non_na],
      method = p_adjust
    )
  }

  return(coef_tbl)
}


# suppress test statistics and p-values for parameters that have no natural
# point null on the scale on which they are estimated
#
# logEC50_Intercept is estimated on the log-concentration scale, so testing
# H0: logEC50 = 0 is equivalent to testing EC50 = 1 on the concentration
# scale — a value with no intrinsic pharmacometric meaning
.suppress_nonsensical_pvalues <- function(coef_tbl) {
  nonsensical <- grepl("^logEC50.*_Intercept$", coef_tbl$label)
  stat_col <- intersect(c("t_statistic", "z_statistic"), names(coef_tbl))
  if (length(stat_col) > 0) {
    coef_tbl[[stat_col]][nonsensical] <- NA_real_
  }
  coef_tbl$p_value[nonsensical] <- NA_real_
  return(coef_tbl)
}


# compute pointwise Wald confidence intervals as a fallback when
# profile likelihood CI computation fails
#
# df: degrees of freedom for the t-distribution; use NULL for z (normal)
.wald_ci <- function(nls_obj, level = 0.95, df = NULL) {
  est   <- stats::coef(nls_obj)
  se    <- sqrt(diag(stats::vcov(nls_obj)))
  alpha <- (1 - level) / 2
  crit  <- if (!is.null(df) && is.finite(df)) {
    stats::qt(1 - alpha, df = df)
  } else {
    stats::qnorm(1 - alpha)
  }
  ci <- cbind(est - crit * se, est + crit * se)
  rownames(ci) <- names(est)
  colnames(ci) <- paste0(c(alpha, 1 - alpha) * 100, "%")
  ci
}


# compute simultaneous (joint) Wald confidence intervals using the
# multivariate normal critical value from mvtnorm::qmvnorm()
.simultaneous_ci <- function(object, level = 0.95) {
  nls_obj <- .get_nls(object)
  est     <- stats::coef(nls_obj)
  cov_mat <- stats::vcov(nls_obj)
  se      <- sqrt(diag(cov_mat))
  cor_mat <- stats::cov2cor(cov_mat)
  crit    <- mvtnorm::qmvnorm(level, sigma = cor_mat, tail = "both.tails")$quantile
  ci      <- cbind(est - crit * se, est + crit * se)
  rownames(ci) <- names(est)
  alpha        <- (1 - level) / 2
  colnames(ci) <- paste0(c(alpha, 1 - alpha) * 100, "%")
  return(ci)
}


# simplified version of scales::label_pvalue()
.show_p <- function(x, accuracy = 0.001) {
  .assert(is.numeric(x), "`x` must be numeric")
  .assert(.is_scalar_num(accuracy), "`accuracy must be a single number")
  prefix <- c("<", "", ">")
  digits <- -floor(log10(accuracy))
  fmt <- paste0("%1.", digits, "f")
  out <- sprintf(fmt, x)
  out <- paste0(prefix[[2]], out)
  out[x < accuracy] <- paste0(prefix[[1]], accuracy)
  out[x > 1 - accuracy] <- paste0(prefix[[3]], 1 - accuracy)
  out[is.na(x)] <- NA
  names(out) <- names(x)
  return(out)
}
