
#' Print an Emax regression model object
#'
#' The `print()` method for `emaxnls` and `emaxlogistic` regression models 
#' reports the structural and covariate model along with a table of 
#' coefficients, test statistics, and confidence intervals
#'
#' @param x An `emaxnls` or `emaxlogistic` object
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
  }

  return(invisible(x))
}


#' Summary of an Emax regression model
#'
#' Returns a tidy coefficient table for a fitted `emaxnls` model, combining
#' parameter estimates, standard errors, test statistics, p-values, and
#' confidence intervals.
#'
#' @param object An `emaxnls` or `emaxlogistic` object
#' @param conf_level Confidence level for interval estimates
#' @param back_transform Should log-scaled parameters (logEC50, logHill) be back-transformed to original scale?
#' @param ... Ignored
#'
#' @details
#' The `back_transform` argument applies the same log-scale transformation as
#' in `coef()` and `confint()`, exponentiating logEC50 and logHill and
#' renaming them. For continuous Emax regressions, the test statistic is a
#' t-statistic, whereas binary Emax models report z-statistics. 
#' 
#' @returns A data frame containing one row per model parameter with columns
#'   for the estimate, standard error, test statistic, p-value, and confidence
#'   interval bounds. The return format is experimental and may change in
#'   future releases.
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
#' # standard summary
#' summary(mod_c)
#' 
#' # summary with adjusted confidence level
#' summary(mod_c, conf_level = 0.99)
#' 
#' # summary with log-scale parameters transformed to original scale
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
summary.emaxnls <- function(object, conf_level = 0.95, back_transform = FALSE, ...) {
  if (!.is_converged(object)) return(.nls_null())
  .coef_table(object = object, level = conf_level, back_transform = back_transform)
}


# construct the coefficient table
.coef_table <- function(object, level = 0.95, back_transform = FALSE, ...) {
  sss <- summary(.get_nls(object))
  coef_tbl <- sss$coef
  ci <- .confint_quiet(.get_nls(object), level = level)
  ci <- ci$result
  
  coef_tbl <- as.data.frame(coef_tbl) 
  coef_tbl <- .rownames_to_column(coef_tbl, "label")
  coef_tbl <- .as_tibble(coef_tbl)
  coef_tbl <- coef_tbl[, c("label", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  names(coef_tbl) <- c("label", "estimate", "std_error", "t_statistic", "p_value") 

  coef_tbl$ci_lower <- ci[, 1]
  coef_tbl$ci_upper <- ci[, 2]

  if (back_transform) {
    trans_cases <- grep("^log", coef_tbl$label)
    coef_tbl$label <- gsub("^log", "", coef_tbl$label)
    coef_tbl$estimate[trans_cases] <- exp(coef_tbl$estimate[trans_cases])
    coef_tbl$std_error[trans_cases] <- NA_real_
    coef_tbl$ci_lower[trans_cases] <- exp(coef_tbl$ci_lower[trans_cases])
    coef_tbl$ci_upper[trans_cases] <- exp(coef_tbl$ci_upper[trans_cases])
  }

  return(coef_tbl)
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

