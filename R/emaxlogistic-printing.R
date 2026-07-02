
#' @rdname print
#' @exportS3Method base::print
print.emaxlogistic <- function(x, conf_level = 0.95, ...) {

  cat("Structural model:\n\n")
  cat("  Exposure:      ", as.character(.get_exposure_name(x)), "\n")
  cat("  Response:      ", as.character(.get_response_name(x)), "\n")
  cat("  Emax type:     ", .get_model_type(x), "\n")
  cat("  Response type: binary (logit link)\n\n")
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
    cat("  Observations: ", stats::nobs(x), "\n")
    cat("  Residual df:  ", stats::df.residual(x), "\n")
    cat("  Deviance:     ", round(stats::deviance(x), 4L), "\n")
    cat("  AIC:          ", round(stats::AIC(x), 4L), "\n\n")

    tbl <- .coef_table_logistic(x, level = conf_level)
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


#' @rdname summary
#' @exportS3Method base::summary
summary.emaxlogistic <- function(object, conf_level = 0.95,
                                  back_transform = FALSE, p_adjust = "none",
                                  simultaneous = FALSE,
                                  suppress_nonsensical = TRUE, ...) {
  if (!.is_converged(object)) return(.nls_null())
  .coef_table_logistic(
    object               = object,
    level                = conf_level,
    back_transform       = back_transform,
    p_adjust             = p_adjust,
    simultaneous         = simultaneous,
    suppress_nonsensical = suppress_nonsensical
  )
}


# build the logistic coefficient table using z-statistics
.coef_table_logistic <- function(object, level = 0.95, back_transform = FALSE,
                                  p_adjust = "none", simultaneous = FALSE,
                                  suppress_nonsensical = FALSE, ...) {

  cc <- stats::coef(object)
  se <- sqrt(diag(stats::vcov(object)))
  z  <- cc / se
  p  <- 2 * stats::pnorm(-abs(z))

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
        .wald_ci(.get_nls(object), level = level, df = NULL)
      }
    )
  }

  coef_tbl <- data.frame(
    label       = names(cc),
    estimate    = unname(cc),
    std_error   = unname(se),
    z_statistic = unname(z),
    p_value     = unname(p),
    ci_lower    = ci[, 1],
    ci_upper    = ci[, 2],
    stringsAsFactors = FALSE
  )
  coef_tbl <- .as_tibble(coef_tbl)

  # suppress before back_transform so the logEC50 label is still in its
  # original form when the pattern match runs
  if (suppress_nonsensical) {
    coef_tbl <- .suppress_nonsensical_pvalues(coef_tbl)
  }

  if (back_transform) {
    trans_cases <- grep("^log", coef_tbl$label)
    coef_tbl$label[trans_cases]      <- gsub("^log", "", coef_tbl$label[trans_cases])
    coef_tbl$estimate[trans_cases]   <- exp(coef_tbl$estimate[trans_cases])
    coef_tbl$std_error[trans_cases]  <- NA_real_
    coef_tbl$z_statistic[trans_cases] <- NA_real_
    coef_tbl$p_value[trans_cases]    <- NA_real_
    coef_tbl$ci_lower[trans_cases]   <- exp(coef_tbl$ci_lower[trans_cases])
    coef_tbl$ci_upper[trans_cases]   <- exp(coef_tbl$ci_upper[trans_cases])
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
