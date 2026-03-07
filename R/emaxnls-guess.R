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
  resid_max <- max(abs(base_resid))

  ini <- coefficient_table |>
    dplyr::mutate(
      start = dplyr::case_when(
        covariate != "Intercept" ~ 0,
        covariate == "Intercept" & parameter == "E0"      ~ base_guess["E0"],
        covariate == "Intercept" & parameter == "Emax"    ~ base_guess["Emax"],
        covariate == "Intercept" & parameter == "logEC50" ~ base_guess["logEC50"],
        covariate == "Intercept" & parameter == "logHill" ~ base_guess["logHill"],
      ),
      lower = dplyr::case_when(
        parameter == "E0"      ~ base_guess["E0"] - resid_max,
        parameter == "Emax"    ~ base_guess["Emax"] - resid_max,
        covariate == "Intercept" & parameter == "logEC50" ~ base_guess["logEC50"] - 10,
        covariate != "Intercept" & parameter == "logEC50" ~ 0,
        parameter == "logHill" ~ -2
      ),
      upper = dplyr::case_when(
        parameter == "E0"      ~ base_guess["E0"] + resid_max,
        parameter == "Emax"    ~ base_guess["Emax"] + resid_max,
        parameter == "logEC50" ~ base_guess["logEC50"] + 10,
        parameter == "logHill" ~ 4
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
  m <- lm(rsp_dosed ~ log_exp_dosed)
  my <- predict(m, newdata = data.frame(log_exp_dosed = min(log_exp_dosed) + c(0, 1)))
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
  Emax <- predict(m, newdata = data.frame(log_exp_dosed = max(log_exp_dosed))) - E0

  # estimate logEC50 using the model, and the estimates for E0 and Emax
  E50 <- E0 + Emax / 2
  b <- coef(m)
  logEC50 <- (E50 - b[1]) / b[2] 

  # fix the initial estimate of logHill at 0
  logHill <- 0

  est <- c(E0, Emax, logEC50, logHill)
  names(est) <- c("E0", "Emax", "logEC50", "logHill")

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