
# allow a user-facing version
.emax_logistic_init <- function(structural_model, covariate_model, data) {
  .validate_structural_formula(structural_model, names(data))
  .validate_covariate_formula(covariate_model, names(data))
  tmp <- .construct_design(structural_model, covariate_model, data)
  variables <- .construct_variables(structural_model, covariate_model, tmp$lookup)
  ini <- .guess_init_logistic(variables, tmp$design)
  return(ini)
}

# workhorse function: mirrors .guess_init() but works on the logit scale
.guess_init_logistic <- function(variables, design) {

  coefficient_vec <- .drop_na(variables$coef_name)
  coefficient_table <- .tibble(
    parameter = gsub("_.*$", "", coefficient_vec),
    covariate = gsub("^[^_]*_", "", coefficient_vec)
  )

  exp_var <- .filter(variables, param_name == "exposure")$var_name
  rsp_var <- .filter(variables, param_name == "response")$var_name
  exp <- design[[exp_var]]
  rsp <- design[[rsp_var]]

  # transform binary response to empirical logit scale
  rsp_logit <- .logit(.clamp(rsp, 1e-5, 1 - 1e-5))

  base_guess <- .guess_base_logistic(exp, rsp, rsp_logit)
  base_resid <- .guess_resid_logistic(base_guess, exp, rsp_logit)
  cov_names <- .filter(variables, param_type == "covariate")$term
  cov_names <- unique(cov_names)

  scale_guess <- .guess_var_scale(exp, rsp_logit, base_resid, cov_names, design)
  resid_max <- max(abs(base_resid))
  beta_max <- 5
  loghill_max <- 5

  ini <- coefficient_table
  ini$start <- with(coefficient_table, .case_when(
    covariate != "Intercept" ~ 0,
    covariate == "Intercept" & parameter == "E0"      ~ base_guess["E0"],
    covariate == "Intercept" & parameter == "Emax"    ~ base_guess["Emax"],
    covariate == "Intercept" & parameter == "logEC50" ~ base_guess["logEC50"],
    covariate == "Intercept" & parameter == "logHill" ~ base_guess["logHill"]
  ))
  ini$lower <- with(coefficient_table, .case_when(
    covariate == "Intercept" & parameter == "E0"      ~ base_guess["E0"] - 2 * resid_max,
    covariate == "Intercept" & parameter == "Emax"    ~ base_guess["Emax"] - 2 * resid_max,
    covariate == "Intercept" & parameter == "logEC50" ~ min(base_guess["logEC01"], min(log(exp[exp > 0]))),
    covariate == "Intercept" & parameter == "logHill" ~ base_guess["logHill"] - loghill_max,
    covariate != "Intercept" & parameter == "E0"      ~ -beta_max * scale_guess$rsp / scale_guess$cov[covariate],
    covariate != "Intercept" & parameter == "Emax"    ~ -beta_max * scale_guess$rsp / scale_guess$cov[covariate],
    covariate != "Intercept" & parameter == "logEC50" ~ -beta_max * scale_guess$exp / scale_guess$cov[covariate],
    covariate != "Intercept" & parameter == "logHill" ~ -loghill_max
  ))
  ini$upper <- with(coefficient_table, .case_when(
    covariate == "Intercept" & parameter == "E0"      ~ base_guess["E0"] + 2 * resid_max,
    covariate == "Intercept" & parameter == "Emax"    ~ base_guess["Emax"] + 2 * resid_max,
    covariate == "Intercept" & parameter == "logEC50" ~ max(base_guess["logEC99"], max(log(exp[exp > 0]))),
    covariate == "Intercept" & parameter == "logHill" ~ base_guess["logHill"] + loghill_max,
    covariate != "Intercept" & parameter == "E0"      ~ beta_max * scale_guess$rsp / scale_guess$cov[covariate],
    covariate != "Intercept" & parameter == "Emax"    ~ beta_max * scale_guess$rsp / scale_guess$cov[covariate],
    covariate != "Intercept" & parameter == "logEC50" ~ beta_max * scale_guess$exp / scale_guess$cov[covariate],
    covariate != "Intercept" & parameter == "logHill" ~ loghill_max
  ))

  names(ini$start) <- coefficient_vec
  names(ini$lower) <- coefficient_vec
  names(ini$upper) <- coefficient_vec

  return(ini)
}

# construct a base structural model guess on the logit scale
.guess_base_logistic <- function(exposure, response, response_logit) {

  is_placebo <- exposure == 0
  n_placebo  <- sum(is_placebo)
  n_total    <- length(exposure)
  n_dosed    <- n_total - n_placebo

  log_exp_dosed  <- log(exposure[!is_placebo])
  rsp_logit_dosed <- response_logit[!is_placebo]

  # fit a log-linear model on the logit scale for dosed subjects
  m <- stats::lm(rsp_logit_dosed ~ log_exp_dosed)
  my <- stats::predict(m, newdata = data.frame(log_exp_dosed = min(log_exp_dosed) + c(0, 1)))
  mx <- exp(min(log_exp_dosed) + c(0, 1))
  E0_dosed <- my[1] - mx[1] / (mx[2] - mx[1]) * (my[2] - my[1])

  if (n_placebo == 0) {
    E0 <- E0_dosed
  } else {
    E0_placebo <- mean(response_logit[is_placebo])
    E0 <- (n_placebo * E0_placebo + n_dosed * E0_dosed) / n_total
  }

  Emax <- stats::predict(m, newdata = data.frame(log_exp_dosed = max(log_exp_dosed))) - E0

  E50 <- E0 + Emax * 0.5
  E01 <- E0 + Emax * 0.01
  E99 <- E0 + Emax * 0.99
  b <- stats::coef(m)
  logEC50 <- (E50 - b[1]) / b[2]
  logEC01 <- (E01 - b[1]) / b[2]
  logEC99 <- (E99 - b[1]) / b[2]

  logHill <- 0

  est <- c(E0, Emax, logEC50, logHill, logEC01, logEC99)
  names(est) <- c("E0", "Emax", "logEC50", "logHill", "logEC01", "logEC99")
  return(est)
}

# guess residuals on the logit scale
.guess_resid_logistic <- function(est, exposure, response_logit) {
  h     <- exp(est["logHill"])
  exph  <- exposure ^ h
  ec50h <- exp(est["logEC50"]) ^ h
  prd   <- est["E0"] + est["Emax"] * (exph / (exph + ec50h))
  resid <- response_logit - prd
  return(resid)
}
