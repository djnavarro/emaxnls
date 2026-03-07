# guess parameters for initialisation ------

.guess_init <- function(store) {

  coefficient_vec <- unname(unlist(store$coefficients))

  coefficient_table <- tibble::tibble(
    parameter = gsub("_.*$", "", coefficient_vec),
    covariate = gsub("^[^_]*_", "", coefficient_vec)
  )

  exp_var <- store$variables$exposure 
  rsp_var <- store$variables$response
  exp <- store$design[[exp_var]]
  rsp <- store$design[[rsp_var]]
  base_guess <- .guess_base(exp, rsp)
  base_resid <- .guess_resid(base_guess, exp, rsp)
  cov_names <- unique(unlist(c(
    store$variables$E0, 
    store$variables$Emax, 
    store$variables$logEC50, 
    store$variables$logHill
  )))

  scale_guess <- .guess_var_scale(exp, rsp, base_resid, cov_names, store$design)
  resid_max <- max(abs(base_resid))
  beta_max <- 5
  loghill_max <- 5

  ini <- coefficient_table |>
    dplyr::mutate(
      start = dplyr::case_when(
        covariate != "Intercept" ~ 0,
        covariate == "Intercept" & parameter == "E0"      ~ base_guess["E0"],
        covariate == "Intercept" & parameter == "Emax"    ~ base_guess["Emax"],
        covariate == "Intercept" & parameter == "logEC50" ~ base_guess["logEC50"],
        covariate == "Intercept" & parameter == "logHill" ~ base_guess["logHill"]
      ),
      lower = dplyr::case_when(
        covariate == "Intercept" & parameter == "E0"      ~ base_guess["E0"] - resid_max,
        covariate == "Intercept" & parameter == "Emax"    ~ base_guess["Emax"] - resid_max,
        covariate == "Intercept" & parameter == "logEC50" ~ min(base_guess["logEC01"], min(log(exp[exp>0]))),
        covariate == "Intercept" & parameter == "logHill" ~ base_guess["logHill"] - loghill_max,
        covariate != "Intercept" & parameter == "E0"      ~ -beta_max * scale_guess$rsp / scale_guess$cov[covariate],
        covariate != "Intercept" & parameter == "Emax"    ~ -beta_max * scale_guess$rsp / scale_guess$cov[covariate],
        covariate != "Intercept" & parameter == "logEC50" ~ -beta_max * scale_guess$exp / scale_guess$cov[covariate],
        covariate != "Intercept" & parameter == "logHill" ~ -loghill_max 
      ),
      upper = dplyr::case_when(
        covariate == "Intercept" & parameter == "E0"      ~ base_guess["E0"] + resid_max,
        covariate == "Intercept" & parameter == "Emax"    ~ base_guess["Emax"] + resid_max,
        covariate == "Intercept" & parameter == "logEC50" ~ max(base_guess["logEC99"], max(log(exp[exp>0]))),
        covariate == "Intercept" & parameter == "logHill" ~ base_guess["logHill"] + loghill_max,
        covariate != "Intercept" & parameter == "E0"      ~ beta_max * scale_guess$rsp / scale_guess$cov[covariate],
        covariate != "Intercept" & parameter == "Emax"    ~ beta_max * scale_guess$rsp / scale_guess$cov[covariate],
        covariate != "Intercept" & parameter == "logEC50" ~ beta_max * scale_guess$exp / scale_guess$cov[covariate],
        covariate != "Intercept" & parameter == "logHill" ~ loghill_max
      )
    )
  
  names(ini$start) <- coefficient_vec
  names(ini$lower) <- coefficient_vec
  names(ini$upper) <- coefficient_vec

  return(ini)
}

.guess_base <- function(exposure, response) {

  # separate placebo samples from dosed samples
  is_placebo <- exposure == 0
  n_placebo <- sum(is_placebo)
  n_total <- length(exposure)
  n_dosed <- n_total - n_placebo

  # log-exposure and response for the dosed samples
  log_exp_dosed <- log(exposure[!is_placebo])
  rsp_dosed <- response[!is_placebo]

  # fit a log-linear model for the dosed samples, and 
  # extrapolate a crude estimate of E0 from it
  m <- stats::lm(rsp_dosed ~ log_exp_dosed)
  my <- stats::predict(m, newdata = data.frame(log_exp_dosed = min(log_exp_dosed) + c(0, 1)))
  mx <- exp(min(log_exp_dosed) + c(0, 1))
  E0_dosed  <- my[1] - mx[1]/(mx[2] - mx[1]) * (my[2] - my[1])

  # initial value for E0 is the weighted average of the 
  # dosing-model estimate, and the placebo-samples estimate
  if (n_placebo == 0) {
    E0 <- E0_dosed
  } else {
    E0_placebo <- mean(response[is_placebo])
    E0 <- (n_placebo * E0_placebo + n_dosed * E0_dosed) / n_total
  }

  # estimate emax based on the predicted response at the maximum
  # observed exposure in the data
  Emax <- stats::predict(m, newdata = data.frame(log_exp_dosed = max(log_exp_dosed))) - E0

  # estimate logEC50 using the model, and the estimates for E0 and Emax
  E50 <- E0 + Emax * 0.5
  E01 <- E0 + Emax * 0.01
  E99 <- E0 + Emax * 0.99
  b <- stats::coef(m)
  logEC50 <- (E50 - b[1]) / b[2] 
  logEC01 <- (E01 - b[1]) / b[2] 
  logEC99 <- (E99 - b[1]) / b[2] 

  # fix the initial estimate of logHill at 0
  logHill <- 0

  est <- c(E0, Emax, logEC50, logHill, logEC01, logEC99)
  names(est) <- c("E0", "Emax", "logEC50", "logHill", "logEC01", "logEC99")

  return(est)
}

.guess_resid <- function(est, exposure, response) {
  h <- exp(est["logHill"])
  exph <- exposure ^ h
  ec50h <- exp(est["logEC50"]) ^ h
  prd <- est["E0"] + est["Emax"] * (exph / (exph + ec50h))
  resid <- response - prd
  return(resid)
}

.guess_var_scale <- function(exposure, response, residuals, cov_names, design) {
  sds <- list(
    cov = purrr::map_dbl(
      .x = cov_names,
      .f = function(nn) stats::sd(design[[nn]])
    ),
    exp = stats::sd(exposure),
    rsp = stats::sd(response),
    res = stats::sd(residuals)
  )
  names(sds$cov) <- cov_names
  return(sds)
}