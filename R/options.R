
# convert coefficient names into table
.as_coefficient_table <- function(coefficients) {
  coefficients |>
    tibble::tibble() |>
    tidyr::separate(
      coefficients,
      into = c("parameter", "covariate"),
      sep = "_",
      extra = "merge",
      remove = FALSE
    )
}

# initial values and boundaries for model parameters; these
# are currently hard coded, and somewhat arbitrarily at that
.get_coefficient_settings <- function(coefficients) {
  coefficients |>
    .as_coefficient_table() |>
    dplyr::mutate(
      start = dplyr::case_when(
        covariate != "Intercept" ~ 0,
        covariate == "Intercept" & parameter == "E0"      ~ -2,
        covariate == "Intercept" & parameter == "Emax"    ~ -1,
        covariate == "Intercept" & parameter == "logEC50" ~ 5,
        covariate == "Intercept" & parameter == "logHill" ~ 0,
      ),
      lower = dplyr::case_when(
        parameter == "E0"      ~ -10,
        parameter == "Emax"    ~ -10,
        covariate == "Intercept" & parameter == "logEC50" ~ 1,
        covariate != "Intercept" & parameter == "logEC50" ~ 0,
        parameter == "logHill" ~ -2
      ),
      upper = dplyr::case_when(
        parameter == "E0"      ~ 20,
        parameter == "Emax"    ~ 20,
        parameter == "logEC50" ~ 10,
        parameter == "logHill" ~ 4
      )
    )
}

