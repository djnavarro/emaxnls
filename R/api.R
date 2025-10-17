
#' Emax model with arbitrary covariates (does not support interactions)
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each of specifying a covariate model for a structural parameter
#' @param data A data frame
#' @param quiet When quiet=TRUE messages and warnings are suppressed
#'  
#' @returns
#' An object of class `emaxnls_fit`
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

#' Add or remove a covariate term from an Emax regression
#'
#' @param object An emaxnls_fit object
#' @param formula A formula such as E0 ~ AGE
#' @param quiet When quiet=TRUE messages and warnings are suppressed
#'
#' @returns
#' An object of class `emaxnls_fit`
#'
#' @examples
#' mod_0 <- emax_nls(response_1 ~ exposure_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
#' mod_1 <- emax_nls(response_1 ~ exposure_1, list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), emax_df)
#' 
#' emax_add_term(mod_0, E0 ~ cnt_a)
#' emax_remove_term(mod_1, E0 ~ cnt_a)
#' 
#' @name emax_update
NULL

#' @export
#' @rdname emax_update
emax_add_term <- function(object, formula, quiet = FALSE) {
  .emax_add_term(object = object, formula = formula, quiet = quiet)
}

#' @export
#' @rdname emax_update
emax_remove_term <- function(object, formula, quiet = FALSE) {
  .emax_remove_term(object = object, formula = formula, quiet = quiet)
}

#' Stepwise covariate modelling for Emax regression
#'
#' @param mod An emaxnls_fit object
#' @param candidates A list of candidate covariates
#' @param threshold Threshold for addition or removal
#' @param quiet When quiet=TRUE messages and warnings are suppressed
#' @param history When history=TRUE the sequence of models tested is stored
#' @param seed Seed for the RNG state
#'
#' @returns
#' An object of class `emaxnls_fit`
#'
#' @examples
#' base_model <- emax_nls(response_1 ~ exposure_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
#' 
#' covariate_list <- list(
#'   E0 = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e"),
#'   Emax = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e")
#' )
#' 
#' final_mod <- base_model |> 
#'   emax_forward(candidates = covariate_list, threshold = .01) |> 
#'   emax_backward(candidates = covariate_list, threshold = .001) 
#' 
#' final_mod
#' 
#' @name emax_scm
NULL

#' @export
#' @rdname emax_scm
emax_forward <- function(mod,
                         candidates,
                         threshold = .01,
                         quiet = FALSE,
                         history = TRUE,
                         seed = NULL) {
  .emax_forward(
    mod = mod,
    candidates = candidates,
    threshold = threshold,
    quiet = quiet,
    history = history, 
    seed = seed
  )
}

#' @export
#' @rdname emax_scm
emax_backward <- function(mod,
                          candidates,
                          threshold = .001,
                          quiet = FALSE,
                          history = TRUE,
                          seed = NULL) {
  .emax_backward(
    mod = mod,
    candidates = candidates,
    threshold = threshold,
    quiet = quiet,
    history = history, 
    seed = seed
  )
}