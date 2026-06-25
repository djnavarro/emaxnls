
#' Estimate parameters for an Emax regression model
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each of specifying a 
#' covariate model for a structural parameter
#' @param data A data frame that includes all relevant variables
#' @param init Initial values and bounds for parameters. See `emax_nls_init()`
#' @param opts Model fitting and optimization options. See `emax_nls_options()`
#' 
#' @details
#' The `emax_nls()` function is the workhorse function for estimating an Emax
#' regression model. Pass a two-sided formula to the `structural_model` argument
#' to specify the exposure variable and the response variable 
#' (e.g., `response ~ exposure`), and pass a list of formulas to the 
#' `covariate_model` argument to specify covariates of interest. At a minimum
#' the covariate model requires specification of the covariate model for the 
#' E0 parameter, the Emax parameter, and the logEC50 parameter. For example, 
#' a formula like `E0 ~ age + group` would indicate that `age` and `group` 
#' should both be included as covariates on the baseline response E0. When 
#' no covariates are to be added, use a formula like `Emax ~ 1`. 
#' 
#' The `emax_nls()` function can support sigmoidal emax models as well as 
#' hyperbolic models. To build a sigmoidal model (where the Hill parameter)
#' is estimated from the data, the `covariate_model` argument must also 
#' include a formula for the `logHill` parameter. For instance, if the 
#' covariate model includes `logHill ~ 1`, the model will estimate the value
#' of the Hill parameter (with no covariates on it) from the data set.
#' 
#' At present, `emax_nls()` does not support binary response variables, nor
#' is it possible to specify interaction terms in the covariate model. 
#' 
#' When estimating model parameters, the `init` argument can be used to 
#' specify the starting values for the optimization. If unspecified, 
#' the `emax_nls_init()` function is used to automatically guess sensible
#' starting values. Please see the documentation of that function for 
#' additional details on manually specifying the initial values. 
#' 
#' The `emax_nls()` function currently supports three optimization methods:
#' the Gauss-Newton algorithm, the Levenberg-Marquardt algorithm, and the 
#' 'nl2sol' algorithm from the Port library. For more information on how
#' to customize the optimization procedure, please see
#' the documentation for `emax_nls_options()`.
#'  
#' @returns
#' An object of class `emaxnls`
#' 
#' @seealso `emax_nls_options()`, `emax_nls_init()`
#' 
#' @examples
#' emax_nls(
#'   structural_model = rsp_1 ~ exp_1, 
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
#' the nonlinear least squares optimization problem. Supported options are 
#' "gauss" (the default), "port", and "levenberg". See details.
#' @param optim_control A list of arguments used to control the behavior of 
#' the optimization algorithm. Allowed values differ depending on which 
#' algorithm is used
#' @param quiet When `quiet=TRUE`, messages are suppressed
#' @param weights Numeric vector providing the weights for observations. When
#' specified, weighted least squares is used
#' @param na.action How should missing values in the data be handled?
#'
#' @details
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
#' The `optim_control` argument mirrors the corresponding control arguments for 
#' the respective optimization methods:
#' 
#' - For "gauss" and "port": the list should match the output of `stats::nls.control()`
#' - For "levenberg": the list should match the output of `minpack.lm::nls.lm.control()`
#' 
#' If `optim_control = NULL`, the default settings are used for the relevant function.
#' 
#' @returns A list of settings
#' 
#' @seealso `emax_nls()`, `emax_nls_init()`
#' 
#' @examples
#' # default options
#' emax_nls_options()
#' 
#' # switch to levenberg-marquardt
#' emax_nls_options(optim_method = "levenberg")
#' 
#' 
#' @export
emax_nls_options <- function(optim_method = "gauss",
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
#' @details The `emax_nls()` function requires that the user specify the initial
#' values for the model parameters. Specifically, it expects to be supplied with
#' a data frame with columns named `parameter`, `covariate`, and `start`. If a
#' bounded optimization method is used (e.g. if the "port" method is used), the
#' data frame also needs to have columns named `lower` and `upper`. The data 
#' frame should contain one row per parameter. In most cases the user does not
#' need to define this manually, because `emax_nls_init()` can use heuristics to
#' make a sensible guess about what to use as starting values. By default this
#' is what `emax_nls()` relies upon, automatically calling `emax_nls_init()`
#' using the appropriate values for the `structural_model`, the `covariate_model`,
#' and the `data`. 
#' 
#' @export
#' 
#' @seealso `emax_nls()`, `emax_nls_options()`
#' 
#' @examples
#' # use a heuristic to construct sensible start values, and plausible
#' # upper and lower bounds within which the estimate is expected to fall 
#' emax_nls_init(
#'   structural_model = rsp_1 ~ exp_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#' 
#' # compare to the values estimated:
#' coef(emax_nls(
#'   structural_model = rsp_1 ~ exp_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' ))
#' 
emax_nls_init <- function(structural_model, covariate_model, data) {
  .emax_nls_init(structural_model, covariate_model, data)
}


#' Check Emax regression model for convergence status
#'
#' @param mod An `emaxnls` object
#'
#' @returns
#' Logical value
#'
#' @details
#' This is a convenience function that takes an Emax regression
#' object as input. It returns `TRUE` if the optimization routine
#' converged during model fitting, and `FALSE` if it did not.
#' 
#' @export
emax_converged <- function(mod) {
  .is_converged(mod)
}

#' Add or remove a covariate term from an Emax regression
#'
#' @param mod An `emaxnls` object
#' @param formula A formula such as E0 ~ AGE
#'
#' @details
#' The `emax_add_term()` and `emax_remove_term()` functions take an existing Emax regression
#' object, and allow the user to add or remove a specific term to the model. It is not expected
#' that users will need these functions very often, but they provide the basis for the stepwise
#' covariate modeling procedures that are very commonly used when building Emax regressions.
#' 
#' @seealso `emax_nls()`, [emax_scm]
#' 
#' 
#' @returns
#' An object of class `emaxnls`
#'
#' @examples
#' mod_0 <- emax_nls(rsp_1 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
#' mod_1 <- emax_nls(rsp_1 ~ exp_1, list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), emax_df)
#' 
#' if (emax_converged(mod_0)) emax_add_term(mod_0, E0 ~ cnt_a)
#' 
#' if (emax_converged(mod_1)) emax_remove_term(mod_1, E0 ~ cnt_a)
#' 
#' @name emax_update
NULL

#' @export
#' @rdname emax_update
emax_add_term <- function(mod, formula) {
  .emax_add_term(mod = mod, formula = formula)
}

#' @export
#' @rdname emax_update
emax_remove_term <- function(mod, formula) {
  .emax_remove_term(mod = mod, formula = formula)
}

#' Stepwise covariate modeling for Emax regression
#'
#' @param mod An `emaxnls` object
#' @param candidates A list of candidate covariates
#' @param threshold Threshold for addition or removal
#' @param seed Seed for the RNG state
#'
#' @returns
#' An object of class `emaxnls`
#'
#' @details
#' The emaxnls package supports stepwise covariate modeling via forward addition and 
#' backward elimination. The `emax_scm_forward()` function supports forward addition, 
#' the `emax_scm_backward()` function supports backward elimination, and the syntax 
#' is designed to allow forward-backward procedures by piping a base model to 
#' `emax_scm_forward()` and then to `emax_scm_backward()`. In both cases, the 
#' function takes an `emaxnls` regression object as the first argument, as well as 
#' a list of candidate `covariates` to be considered for addition (in the forward
#' addition case) or deletion (backward elimination). The input must be a named list,
#' with the names corresponding to the relevant structural parameter, and the values
#' should be character vector specifying covariates of interest. See the examples for
#' an illustration of how this argument should be specified.
#' 
#' As present, these functions only support stepwise regression using p-values as the
#' criterion for addition or deletion. The `threshold` argument corresponds to the 
#' threshold p-value to be used. In future, other methods (e.g., selection on the 
#' basis of AIC values) may be supported.
#' 
#' The `seed` argument is used to control the RNG state for stochastic components of 
#' the stepwise procedure. However, please note that the `seed` argument is currently 
#' experimental, and may be removed in future releases.
#' 
#' A key feature of the stepwise covariate modeling functions is that they keep track
#' of every tested model, and store information about this history internally within the
#' `emaxnls` object that gets returned. Use the `emax_scm_history()` function to extract
#' this history.
#' 
#' @seealso `emax_nls()`
#' 
#' @examples
#' base_model <- emax_nls(rsp_1 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
#' 
#' covariate_list <- list(
#'   E0 = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e"),
#'   Emax = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e")
#' )
#' 
#' # add covariates to the base model using forward addition
#' forward_model <- emax_scm_forward(
#'   mod = base_model,
#'   candidates = covariate_list, 
#'   threshold = .01
#' )
#' forward_model
#' 
#' # remove covariates from the forward model using backward deletion
#' final_model <- emax_scm_backward(
#'   mod = forward_model,
#'   candidates = covariate_list, 
#'   threshold = .001
#' ) 
#' final_model
#' 
#' # show the history of all models tested during the forward addition
#' # step and the backward deletion step
#' emax_scm_history(final_model)
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

#' Construct Emax prediction function from model object
#'
#' @param mod An `emaxnls` object
#'
#' @returns A function `f` with arguments `data` and `params`. The `data`
#' argument defaults to the data used to estimate the model, and the
#' `params` argument defaults to the estimated parameter values. Both
#' can be customized, as long as `data` contains columns corresponding
#' to each of the variables used by the model, and `params` is a named
#' numeric vector of the appropriate length. The names for `params` 
#' must exactly match the names of the vector returned by `coef(mod)`.
#' 
#' The return value for `f` is a numeric vector of model predictions for
#' each row in `data`, evaluated at parameters `params`. 
#' 
#' @seealso `emax_nls()`
#'
#' @export
#' @examples
#' 
#' mod <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#' 
#' # the emax simulation function can only be extracted if the
#' # optimization routine converged
#' if (emax_converged(mod)) {
#' 
#'   par <- coef(mod)
#'   
#'   # customizable emax function with the same structural 
#'   # model and same covariate model, defaulting to the 
#'   # same data and parameters as the original model, but
#'   # allowing user to pass their own data and parameters  
#'   mod_fn <- emax_fun(mod)
#'   
#'   # apply the function to a few rows of the original data
#'   out_1 <- mod_fn(
#'     data = emax_df[120:125, ],
#'     param = par
#'   )
#'   print(out_1)
#'   
#'   # adjust the parameters
#'   new_par <- par
#'   new_par["E0_Intercept"] <- 0
#'   
#'   # simulate the model with the adjusted parameters
#'   out_2 <- mod_fn(
#'     data = emax_df[120:125, ],
#'     param = new_par
#'   )
#'   print(out_2)
#' 
#' }
emax_fun <- function(mod) {
  .emax_fun(mod)
}