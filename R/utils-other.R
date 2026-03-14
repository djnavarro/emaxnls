
# purrr wrapped functions ------

.nls_safe      <- purrr::safely(stats::nls)
.nls_lm_safe   <- purrr::safely(minpack.lm::nlsLM)
.confint_quiet <- purrr::quietly(stats::confint)

# global variable declaration ------

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