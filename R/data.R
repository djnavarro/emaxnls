
.simulate_emax_data <- function(seed = 123) {

  .continuous_covariate <- function(n) {
    stats::rbeta(n, 2, 2) * 10
  }

  .binary_covariate <- function(n, p) {
    as.numeric(stats::runif(n) <= p)
  }

  .emf <- function(exposure, emax, ec50, e0, gamma = 1) {
    e0 + emax * (exposure^gamma) / (ec50^gamma + exposure^gamma)
  }

  .simulate_exposure <- function(dose, n, meanlog = 4, sdlog = 0.5) {
    dose * stats::qlnorm(
      p = stats::runif(n, min = .01, max = .99),
      meanlog = meanlog,
      sdlog = sdlog
    )
  }

  .simulate_dose_data <- function(dose, n, par) {
    tibble::tibble(
      dose = dose,
      exposure_1 = .simulate_exposure(dose, n = n),
      exposure_2 = 0.7 * exposure_1 + 0.3 * .simulate_exposure(dose, n = n),

      # add continuous and binary covariates
      cnt_a = .continuous_covariate(n = n),
      cnt_b = .continuous_covariate(n = n),
      cnt_c = .continuous_covariate(n = n),
      bin_d = .binary_covariate(n = n, p = .5),
      bin_e = .binary_covariate(n = n, p = .7),

      # response 1 is continuous
      response_1 = .emf(
        exposure_1,
        emax = par$emax_1,
        ec50 = par$ec50_1,
        e0 = par$e0_1,
        gamma = par$gamma_1
      ) +
        par$coef_a1 * cnt_a +
        par$coef_b1 * cnt_b +
        par$coef_c1 * cnt_c +
        par$coef_d1 * bin_d +
        stats::rnorm(n, 0, par$sigma_1),

      # response 2 is binary; start with the predictor
      bin_pred = .emf(
        exposure_1,
        emax = par$emax_2,
        ec50 = par$ec50_2,
        e0 = par$e0_2,
        gamma = par$gamma_2
      ) +
        par$coef_a2 * cnt_a +
        par$coef_b2 * cnt_b +
        par$coef_c2 * cnt_c +
        par$coef_d2 * bin_d,

      # convert
      bin_prob = 1 / (1 + exp(-bin_pred)),
      response_2 = as.numeric(stats::runif(n) < bin_prob)
    ) |>
      dplyr::select(-bin_pred, -bin_prob) # remove intermediate variables
  }

  set.seed(seed)

  par <- list(
    # parameters for continuous response
    emax_1  = 10,
    ec50_1  = 4000,
    e0_1    = 5,
    gamma_1 = 1,
    sigma_1 = .5,
    coef_a1 = .5,
    coef_b1 = 0,
    coef_c1 = 0,
    coef_d1 = 0,

    # parameters for binary response
    emax_2  = 5,
    ec50_2  = 8000,
    e0_2    = -4.5,
    gamma_2 = 1,
    coef_a2 = .5,
    coef_b2 = 0,
    coef_c2 = 0,
    coef_d2 = 1
  )

  # create data set assuming three dosing groups
  dat <- dplyr::bind_rows(
    .simulate_dose_data(dose = 0, n = 100, par = par),
    .simulate_dose_data(dose = 100, n = 100, par = par),
    .simulate_dose_data(dose = 200, n = 100, par = par),
    .simulate_dose_data(dose = 300, n = 100, par = par)
  ) |> 
    dplyr::relocate(response_1, response_2, .after = exposure_2)

  return(dat)
}


# emax_df <- .simulate_emax_data(seed = 123)
# usethis::use_data(emax_df, overwrite = TRUE)


#' Sample simulated data for Emax exposure-response models with covariates.
#'
#' @name emax_df
#' @format A data frame with columns:
#' \describe{
#' \item{dose}{Nominal dose, units not specified}
#' \item{exposure_1}{Exposure value, units and metric not specified}
#' \item{exposure_2}{Exposure value, units and metric not specified, but different from exposure_1}
#' \item{response_1}{Continuous response value (units not specified)}
#' \item{response_2}{Binary response value (group labels not specified)}
#' \item{cnt_a}{Continuous valued covariate}
#' \item{cnt_b}{Continuous valued covariate}
#' \item{cnt_c}{Continuous valued covariate}
#' \item{bin_d}{Binary valued covariate}
#' \item{bin_e}{Binary valued covariate}
#' }
#' @details
#'
#' This simulated dataset is entirely synthetic. It is a generic data set that can be used
#' to illustrate Emax modeling. It contains variables corresponding to dose and exposure,
#' and includes both a continuous response variable and a binary response variable. Three
#' continuous valued covariates are included, along with two binary covariates.
#'
#' You can find the data generating code in the package source code,
#' under `R/data.R`
#'
#' @examples
#' emax_df
"emax_df"
