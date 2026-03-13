
#' Emax model with arbitrary covariates (does not support interactions)
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each of specifying a covariate model for a structural parameter
#' @param data A data frame
#' @param settings Settings for the nls() optimisation
#' @param quiet When quiet=TRUE messages and warnings are suppressed
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
                     settings = emax_nls_settings(),
                     quiet = FALSE) {
  .emax_nls(
    structural_model = structural_model,
    covariate_model = covariate_model,
    data = data,
    settings = settings,
    quiet = quiet
  )
}

#' Settings used to estimate Emax model
#'
#' @param init Data frame specifying initial parameters (start, upper, lower)
#' @param algorithm Has same meaning as in nls() (allowed: "default", "plinear", "port")
#' @param control Has same meaning as in nls()
#' @param ... Other arguments passed to nls()
#'
#' @returns List
#'
#' @export
emax_nls_settings <- function(init = NULL,
                              algorithm = "port",
                              control = list(
                                tol = 1e-8,
                                minFactor = 1024^-4,
                                maxiter = 200000,
                                scaleOffset = 1,
                                warnOnly = FALSE
                              ),
                              ...) {
  settings <- list(
    init = init,
    algorithm = algorithm,
    control = control,
    ...
  )
  settings$start <- NULL
  settings$upper <- NULL
  settings$lower <- NULL
  return(settings)
}

#' Construct initial guess of a Emax model parameters
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each of specifying a covariate model for a structural parameter
#' @param data A data frame
#'
#' @returns A data frame
#'
#' @export
#' 
#' @examples
#' # use a heuristic to construct sensible start values, and plausible
#' # upper and lower bounds within which the estimate is expected to fall 
#' emax_auto_init(
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
emax_auto_init <- function(structural_model, covariate_model, data) {
  .emax_auto_init(structural_model, covariate_model, data)
}

#' Add or remove a covariate term from an Emax regression
#'
#' @param object An `emaxnls` object
#' @param formula A formula such as E0 ~ AGE
#' @param quiet When quiet=TRUE messages and warnings are suppressed
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
emax_add_term <- function(object, formula, quiet = TRUE) {
  .emax_add_term(object = object, formula = formula, quiet = quiet)
}

#' @export
#' @rdname emax_update
emax_remove_term <- function(object, formula, quiet = TRUE) {
  .emax_remove_term(object = object, formula = formula, quiet = quiet)
}

#' Stepwise covariate modelling for Emax regression
#'
#' @param mod An `emaxnls` object
#' @param candidates A list of candidate covariates
#' @param threshold Threshold for addition or removal
#' @param quiet When quiet=TRUE messages and warnings are suppressed
#' @param history When history=TRUE the sequence of models tested is stored
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
#' final_mod <- base_model |> 
#'   emax_forward(candidates = covariate_list, threshold = .01) |> 
#'   emax_backward(candidates = covariate_list, threshold = .001) 
#' 
#' final_mod
#' 
#' emax_history(final_mod)
#' 
#' @name emax_scm
NULL

#' @export
#' @rdname emax_scm
emax_forward <- function(mod,
                         candidates,
                         threshold = .01,
                         quiet = TRUE,
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
                          quiet = TRUE,
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

#' @export
#' @rdname emax_scm
emax_history <- function(mod) {
  .emax_history(mod, is_final = TRUE)
}