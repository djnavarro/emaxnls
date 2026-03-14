
#' Emax model with arbitrary covariates (does not support interactions)
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each of specifying a 
#' covariate model for a structural parameter
#' @param data A data frame
#' @param init Initial values and bounds for parameters. See `emax_nls_init()`
#' @param opts Model fitting and optimization options. See `emax_nls_options()`
#'  
#' @returns
#' An object of class `emaxnls`
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
                     init = NULL,
                     opts = NULL) {  
  .emax_nls(
    structural_model = structural_model,
    covariate_model = covariate_model,
    data = data,
    init = init,
    opts = opts
  )
}

#' Settings used to estimate Emax model
#'
#' @param optim_method Character string specifying the algorithm used to solve 
#' the nonlinear least squares optimization problem. Supported pptions are 
#' "gauss", "port", and "levenberg". See details.
#' @param optim_control A list of arguments used to control the behavior of 
#' the optimization algorithm. Allowed values differ depending on which 
#' algorithm is used
#' @param quiet When `quiet=TRUE`, messages are suppressed
#' @param weights Numeric vector providing the weights for observations. When
#' specified, weighted least squares is used
#' @param na.action How should missing values in the data be handled
#'
#' At present there are three supported values for `optim_method`:
#' 
#' - "gauss": Estimate parameters using the Gauss-Newton algorithm. This is 
#'   equivalent to the using "default" option in `nls()`
#' - "port": Estimate parameters using bounded optimization with the "nl2sol" 
#'   algorithm from from the the Port library. Equivalent to "port" in `nls()`
#' - "levenberg": Estimate parameters using the Levenberg-Marquardt algorithm. 
#'   This is equivalent to using `nlsLM()` from the "minpack.lm" package.
#' 
#' Note that the Golub-Pereyra algorithm for partially linear least-squares (i.e. the 
#' "plinear" option in `nls()`) is not currently supported for Emax regression. Informal
#' testing suggests it does not perform well for these models, and rarely converges.
#' 
#' @returns List
#'
#' @export
emax_nls_options <- function(optim_method = "port",
                             optim_control = NULL,
                             quiet = FALSE,
                             weights = NULL,
                             na.action = options("na.action")) {
  .emax_nls_options(
    optim_method = optim_method,
    optim_control = optim_control,
    quiet = quiet,
    weights = weights,
    na.action = na.action
  )
}

#' Construct an initial guess for the Emax model parameters
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each of specifying a 
#' covariate model for a structural parameter
#' @param data A data frame
#'
#' @returns A data frame
#'
#' @export
#' 
#' @examples
#' # use a heuristic to construct sensible start values, and plausible
#' # upper and lower bounds within which the estimate is expected to fall 
#' emax_nls_init(
#'   structural_model = response_1 ~ exposure_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#' 
#' # compare to the values estimated:
#' coef(emax_nls(
#'   structural_model = response_1 ~ exposure_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' ))
#' 
emax_nls_init <- function(structural_model, covariate_model, data) {
  .emax_nls_init(structural_model, covariate_model, data)
}

#' Add or remove a covariate term from an Emax regression
#'
#' @param object An `emaxnls` object
#' @param formula A formula such as E0 ~ AGE
#'
#' @returns
#' An object of class `emaxnls`
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
emax_add_term <- function(object, formula) {
  .emax_add_term(object = object, formula = formula)
}

#' @export
#' @rdname emax_update
emax_remove_term <- function(object, formula) {
  .emax_remove_term(object = object, formula = formula)
}

#' Stepwise covariate modelling for Emax regression
#'
#' @param mod An `emaxnls` object
#' @param candidates A list of candidate covariates
#' @param threshold Threshold for addition or removal
#' @param seed Seed for the RNG state
#'
#' @returns
#' An object of class `emaxnls`
#'
#' @examples
#' base_model <- emax_nls(response_1 ~ exposure_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
#' 
#' covariate_list <- list(
#'   E0 = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e"),
#'   Emax = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e")
#' )
#' 
#' mm <- emax_scm_forward(
#'   mod = base_model,
#'   candidates = covariate_list, 
#'   threshold = .01
#' )
#' final_mod <- emax_scm_backward(
#'   mod = mm,
#'   candidates = covariate_list, 
#'   threshold = .001
#' ) 
#' 
#' final_mod
#' 
#' emax_scm_history(final_mod)
#' 
#' @name emax_scm
NULL

#' @export
#' @rdname emax_scm
emax_scm_forward <- function(mod, candidates, threshold = .01, seed = NULL) {
  .emax_scm_forward(
    mod = mod,
    candidates = candidates,
    threshold = threshold,
    seed = seed
  )
}

#' @export
#' @rdname emax_scm
emax_scm_backward <- function(mod, candidates, threshold = .001, seed = NULL) {
  .emax_scm_backward(
    mod = mod,
    candidates = candidates,
    threshold = threshold,
    seed = seed
  )
}

#' @export
#' @rdname emax_scm
emax_scm_history <- function(mod) {
  .emax_scm_history(mod, is_final = TRUE)
}
