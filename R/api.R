
#' Emax model with arbitrary covariates (does not support interactions)
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each of specifying a
#' @param data A data frame
#' @param dosing Column specifying the dosing regimen
#' @param quiet When quiet=TRUE messages and warnings are suppressed
#'  
#' @returns
#' An object of class `emax_fit`
#' 
#' @export
emax_nls <- function(structural_model,
                     covariate_model,
                     data,
                     dosing = NULL,
                     quiet = FALSE) {
  .emax_nls(
    structural_model = structural_model,
    covariate_model = covariate_model,
    data = data,
    dosing = dosing,
    quiet = quiet
  )
}