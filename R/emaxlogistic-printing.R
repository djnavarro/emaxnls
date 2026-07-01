
#' Print a logistic Emax regression model object
#'
#' @param x An `emaxlogistic` object
#' @param ... Ignored
#'
#' @returns Invisibly returns the original object
#'
#' @exportS3Method base::print
print.emaxlogistic <- function(x, ...) {

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
    cat("Coefficient table:\n\n")
    ccc <- utils::capture.output(print(.coef_table_logistic(x)))
    ccc <- ccc[c(-1, -3)]
    cat(ccc, sep = "\n")
    cat("\n")
    cat("Deviance:", round(stats::deviance(x), 4L), "\n")
    cat("AIC:     ", round(stats::AIC(x), 4L), "\n")
  }

  return(invisible(x))
}


#' Summary of a logistic Emax regression model
#'
#' @param object An `emaxlogistic` object
#' @param conf_level Confidence level for interval estimates
#' @param back_transform Should log-scaled parameters (logEC50, logHill) be
#'   back-transformed to the original scale?
#' @param ... Ignored
#'
#' @returns A data frame containing a table of parameter estimates with
#'   z-statistics, p-values, and confidence intervals
#'
#' @exportS3Method base::summary
summary.emaxlogistic <- function(object, conf_level = 0.95,
                                 back_transform = FALSE, ...) {
  if (!.is_converged(object)) return(.nls_null())
  .coef_table_logistic(object = object, level = conf_level,
                       back_transform = back_transform)
}


# build the logistic coefficient table using z-statistics
.coef_table_logistic <- function(object, level = 0.95, back_transform = FALSE, ...) {

  cc   <- stats::coef(object)
  se   <- sqrt(diag(stats::vcov(object)))
  z    <- cc / se
  p    <- 2 * stats::pnorm(-abs(z))

  ci   <- .confint_quiet(.get_nls(object), level = level)
  ci   <- ci$result

  coef_tbl <- data.frame(
    label        = names(cc),
    estimate     = unname(cc),
    std_error    = unname(se),
    z_statistic  = unname(z),
    p_value      = unname(p),
    ci_lower     = ci[, 1],
    ci_upper     = ci[, 2],
    stringsAsFactors = FALSE
  )
  coef_tbl <- .as_tibble(coef_tbl)

  if (back_transform) {
    trans_cases <- grep("^log", coef_tbl$label)
    coef_tbl$label[trans_cases] <- gsub("^log", "", coef_tbl$label[trans_cases])
    coef_tbl$estimate[trans_cases]  <- exp(coef_tbl$estimate[trans_cases])
    coef_tbl$std_error[trans_cases] <- NA_real_
    coef_tbl$ci_lower[trans_cases]  <- exp(coef_tbl$ci_lower[trans_cases])
    coef_tbl$ci_upper[trans_cases]  <- exp(coef_tbl$ci_upper[trans_cases])
  }

  return(coef_tbl)
}
