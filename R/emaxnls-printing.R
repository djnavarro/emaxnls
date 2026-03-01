
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
  if(is.null(.get_nls(x))) {
    cat("Model does not converge\n")
  } else {
    cat("Coefficient table:\n\n")
    ccc <- utils::capture.output(print(.coef_table(x)))
    ccc <- ccc[c(-1, -3)]
    cat(ccc, sep = "\n")
    cat("\n")
    cat("Variance-covariance matrix:\n\n")
    print(vcov(x), digits = 2)
  }

  return(invisible(x))
}

# construct the coefficient table
.coef_table <- function(object, level = 0.95, ...) {
  sss <- summary(.get_nls(object))
  coef_tbl <- sss$coef
  ci <- .confint_quiet(.get_nls(object), level = level)
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

# simplified version of scales::label_pvalue()
.show_p <- function(x, accuracy = 0.001) {
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

