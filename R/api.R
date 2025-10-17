
#' Emax model with arbitrary covariates (does not support interactions)
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each of specifying a covariate model for a structural parameter
#' @param data A data frame
#' @param quiet When quiet=TRUE messages and warnings are suppressed
#'  
#' @returns
#' An object of class `emax_fit`
#' 
#' @examples
#' emax_nls(
#'   structural_model = response_1 ~ exposure_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#'  
#' @export
emax_nls <- function(structural_model,
                     covariate_model,
                     data,
                     quiet = FALSE) {
  .emax_nls(
    structural_model = structural_model,
    covariate_model = covariate_model,
    data = data,
    quiet = quiet
  )
}