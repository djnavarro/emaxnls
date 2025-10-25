
# catch errors if nls throws them
.nls_safe <- purrr::safely(stats::nls)


# are these two emax models the same?
# note: this should probably have a "strictness" argument governing how
# deeply we want to push the comparison
.is_same <- function(mod1, mod2) {
  identical(
    .extract_parameter_names(mod1), 
    .extract_parameter_names(mod2)
  )
}

.is_formula <- function(x, sides = NULL) {
  out <- inherits(x, "formula")
  if (!is.null(sides)) {
    if (sides == 1) out <- out && length(x) == 2
    if (sides == 2) out <- out && length(x) == 3
  }
  out
}

.is_sigmoidal <- function(covariate_model) {
  length(covariate_model) == 4
}

.is_hyperbolic <- function(covariate_model) {
  length(covariate_model) == 3
}

utils::globalVariables(c(
  "label",
  "Estimate",
  "Std. Error",
  "t value",
  "Pr(>|t|)",
  "response_1",
  "response_2", 
  "exposure_1",  
  "exposure_2",
  "bin_pred",
  "bin_prob",
  "cnt_a",
  "cnt_b",
  "cnt_c",
  "bin_d",
  "bin_e",
  "term",
  "vcov",
  "coef",
  "mu",
  "residuals",
  "variable",
  "formula",
  "algorithm",
  "control",
  "design", 
  "model", 
  "predict_args",
  "lower",
  "start",
  "upper"
))