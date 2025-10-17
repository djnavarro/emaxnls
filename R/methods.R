

#' Coefficents for an Emax regression
#'
#' @param object An emaxnls_fit object
#' @param level Confidence level for interval estimate
#' @param ... Ignored
#'
#' @returns A tibble
#'
#' @exportS3Method stats::coef
coef.emaxnls_fit <- function(object, level = 0.95, ...) {
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
#'
#' @param object An emaxnls_fit object
#' @param ... Ignored
#'
#' @returns A matrix
#'
#' @exportS3Method stats::vcov
vcov.emaxnls_fit <- function(object, ...) {
  vcov(object$result, ...)
}

#' Residuals for an Emax regression
#'
#' @param object An emaxnls_fit object
#' @param ... Ignored
#'
#' @returns Numeric vector of residuals
#'
#' @exportS3Method stats::residuals
residuals.emaxnls_fit <- function(object, ...) {
  residuals(object$result, ...)
}

#' Print an Emax regression model object
#'
#' @param x An emaxnls_fit object
#' @param ... Ignored
#'
#' @returns Invisibly returns the original object
#'
#' @exportS3Method base::print
print.emaxnls_fit <- function(x, ...) {

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

