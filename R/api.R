
#' Estimate parameters for an Emax regression model
#'
#' Fits an Emax regression model for a continuous response variable using
#' nonlinear least squares. For binary outcomes, use `emax_logistic()` instead.
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each of specifying a 
#' covariate model for a structural parameter
#' @param data A data frame that includes all relevant variables
#' @param init Initial values and bounds for parameters. See `emax_nls_init()`
#' @param opts Model fitting and optimization options. See `emax_nls_options()`
#' 
#' @details
#' Pass a two-sided formula to `structural_model` to specify the response and
#' exposure variables (e.g., `response ~ exposure`), and a list of formulas to
#' `covariate_model` to specify covariates. At a minimum the covariate model
#' requires formulas for E0, Emax, and logEC50. A formula like `E0 ~ age + group`
#' includes `age` and `group` as covariates on the baseline response; use
#' `Emax ~ 1` when no covariates are to be added for a parameter.
#' 
#' To fit a sigmoidal Emax model (estimating the Hill parameter), include a
#' formula for `logHill` in `covariate_model`, e.g. `logHill ~ 1`. Without
#' this term a hyperbolic model is fitted. Interaction terms in the covariate
#' model are not currently supported.
#' 
#' Starting values are constructed automatically via `emax_nls_init()` unless
#' the `init` argument is supplied manually. Three optimization algorithms are
#' available; see `emax_nls_options()` for details.
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
#' Constructs a settings object controlling the optimization algorithm and
#' other aspects of model fitting for `emax_nls()`. Pass the result to the
#' `opts` argument of `emax_nls()`.
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
#' if (require("minpack.lm", quietly = TRUE)) emax_nls_options(optim_method = "levenberg")
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
#' Constructs a data frame of starting values and parameter bounds for the
#' Emax NLS optimization, using heuristics derived from the data.
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
#' Returns `TRUE` if the model converged during fitting and `FALSE` otherwise.
#'
#' @param mod An `emaxnls` object
#'
#' @returns A logical value
#'
#' @export
emax_converged <- function(mod) {
  .is_converged(mod)
}

#' Add or remove a covariate term from an Emax regression
#'
#' Add or remove a single covariate term from an existing Emax regression
#' model, returning a new fitted model object.
#'
#' @param mod An `emaxnls` object
#' @param formula A formula such as E0 ~ AGE
#'
#' @details
#' These functions are not typically called directly; they underpin the
#' stepwise covariate modeling procedures that are very commonly used when
#' building Emax regressions.
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
#' Performs stepwise covariate modeling by forward addition
#' (`emax_scm_forward()`), backward elimination (`emax_scm_backward()`), or
#' both in sequence. Use `emax_scm_history()` to retrieve the history of all
#' models tested during the procedure.
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
#' The `candidates` argument must be a named list whose names correspond to
#' structural parameters (e.g. `E0`, `Emax`) and whose values are character
#' vectors of covariate names to consider. See the examples for an
#' illustration.
#' 
#' At present, covariate selection uses p-values as the criterion: a term is
#' added if its p-value falls below `threshold` (forward) or removed if its
#' p-value exceeds `threshold` (backward). Selection on AIC or other criteria
#' may be supported in future.
#' 
#' The `seed` argument controls the RNG state for any stochastic components of
#' the procedure. It is currently experimental and may be removed in future
#' releases.
#' 
#' Every model tested during the procedure is stored internally in the returned
#' object. Use `emax_scm_history()` to extract this record.
#' 
#' @seealso `emax_nls()`
#' 
#' @examples
#' base_model <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1, 
#'   covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
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
#' # example using binary outcomes
#' base_model_logistic <- emax_nls(
#'   structural_model = rsp_2 ~ exp_1, 
#'   covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#' forward_model_logistic <- emax_scm_forward(
#'   mod = base_model_logistic,
#'   candidates = covariate_list, 
#'   threshold = .01
#' )
#' final_model_logistic <- emax_scm_backward(
#'   mod = forward_model_logistic,
#'   candidates = covariate_list, 
#'   threshold = .001
#' )
#' 
#' final_model_logistic
#' 
#' emax_scm_history(final_model_logistic)
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
#' Extracts a customizable prediction function from a fitted Emax model,
#' allowing predictions to be evaluated at arbitrary data and parameter values.
#'
#' @param mod An `emaxnls` or `emaxlogistic` object
#' @param ... Ignored
#'
#' @details
#' The extracted function accepts `data` and `param` arguments. Both default
#' to the values used when fitting the model. When supplying custom values,
#' `data` must contain all variables used by the model, and `param` must be a
#' named numeric vector whose names exactly match those returned by `coef(mod)`.
#'
#' **Scale of predictions.** For `emaxnls` objects the returned function
#' produces predictions on the response scale (the same scale as the outcome
#' variable). For `emaxlogistic` objects the structural Emax model is
#' parameterized on the logit scale — `logit(p) = E0 + Emax * x / (x + EC50)`
#' — but `emax_fun()` applies the inverse-logit transformation before
#' returning, so predictions are on the probability scale. This is consistent
#' with the default behavior of `fitted()` and `predict()` for `emaxlogistic`
#' objects. If you need the linear predictor (logit scale) directly, use
#' `fitted(object, type = "link")` or `predict(object, type = "link")`.
#'
#' @returns A function with arguments `param` and `data` that evaluates the
#' Emax model at the supplied (or default) parameter values and data. For
#' `emaxnls` objects the return values are on the response scale; for
#' `emaxlogistic` objects they are predicted probabilities in \eqn{(0, 1)}.
#'
#' @seealso `emax_nls()`, `emax_logistic()`
#'
#' @export
#' @examples
#' 
#' mod_c <- emax_nls(
#'   structural_model = rsp_1 ~ exp_1, 
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
#'   data = emax_df
#' )
#' 
#' if (emax_converged(mod_c)) {
#' 
#'   par <- coef(mod_c)
#'   mod_fn <- emax_fun(mod_c)
#'   
#'   # apply the function to a few rows of the original data
#'   mod_fn(data = emax_df[120:125, ], param = par)
#'   
#'   # adjust the parameters and re-evaluate
#'   new_par <- par
#'   new_par["E0_Intercept"] <- 0
#'   mod_fn(data = emax_df[120:125, ], param = new_par)
#' 
#' }
#' 
#' # for emaxlogistic, the returned function gives probabilities
#' mod_b <- emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#' 
#' if (emax_converged(mod_b)) {
#'   mod_fn_b <- emax_fun(mod_b)
#'   mod_fn_b(data = emax_df[120:125, ])
#' }
emax_fun <- function(mod, ...) {
  UseMethod("emax_fun")
}


#' Estimate parameters for a logistic Emax regression model
#'
#' Fits a logistic Emax regression model for a binary response variable using
#' iterative reweighted least squares (IRLS). For continuous outcomes, use
#' `emax_nls()` instead.
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each specifying a 
#' covariate model for a structural parameter
#' @param data A data frame that includes all relevant variables
#' @param init Initial values and bounds for parameters. See `emax_logistic_init()`
#' @param opts Model fitting and optimization options. See `emax_logistic_options()`
#'
#' @details
#' The structural Emax model is placed on the log-odds (logit) scale:
#' 
#' `logit(p) = E0 + Emax * x / (x + EC50)`  (hyperbolic)
#' 
#' `logit(p) = E0 + Emax * x^h / (x^h + EC50^h)`  (sigmoidal)
#' 
#' Estimation uses iterative reweighted least squares (IRLS). At each outer 
#' iteration a weighted NLS problem is solved using working weights and a working
#' response derived from the current parameter estimates. This is equivalent to
#' Fisher scoring and produces maximum likelihood estimates at convergence.
#' 
#' The interface mirrors `emax_nls()` exactly: the `structural_model` and 
#' `covariate_model` arguments have the same specification, including support
#' for sigmoidal models via a `logHill` term. The response variable in 
#' `structural_model` must be a binary (0/1) numeric vector.
#' 
#' @returns An object of class `emaxlogistic` (which also inherits from `emaxnls`)
#'
#' @seealso `emax_logistic_options()`, `emax_logistic_init()`, `emax_nls()`
#'
#' @examples
#' emax_logistic(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#'
#' @export
emax_logistic <- function(structural_model,
                          covariate_model,
                          data,
                          init = NULL,
                          opts = NULL) {
  .emax_logistic(
    structural_model = structural_model,
    covariate_model  = covariate_model,
    data             = data,
    init             = init,
    opts             = opts
  )
}


#' Settings used to estimate a logistic Emax model
#'
#' Constructs a settings object controlling the NLS optimizer and IRLS
#' convergence for `emax_logistic()`. Pass the result to the `opts` argument
#' of `emax_logistic()`.
#'
#' @param optim_method Character string specifying the algorithm used for the
#' weighted NLS step within each IRLS iteration. Supported options are 
#' `"gauss"` (default), `"port"`, and `"levenberg"`. See `emax_nls_options()` 
#' for details on each.
#' @param optim_control A list of arguments controlling the NLS optimizer.
#' @param quiet When `TRUE`, convergence warnings are suppressed.
#' @param na.action How should missing values in the data be handled?
#' @param max_iter Maximum number of IRLS outer iterations (default 25).
#' @param tol Convergence tolerance: IRLS stops when the change in binomial
#' deviance between successive iterations falls below `tol` (default 1e-6).
#'
#' @returns A list of settings
#'
#' @seealso `emax_logistic()`, `emax_logistic_init()`
#'
#' @examples
#' # default options
#' emax_logistic_options()
#'
#' # increase maximum IRLS iterations
#' emax_logistic_options(max_iter = 50)
#'
#' @export
emax_logistic_options <- function(optim_method = "gauss",
                                  optim_control = NULL,
                                  quiet = FALSE,
                                  na.action = options("na.action"),
                                  max_iter = 25,
                                  tol = 1e-6) {
  .emax_logistic_options(
    optim_method  = optim_method,
    optim_control = optim_control,
    quiet         = quiet,
    na.action     = na.action,
    max_iter      = max_iter,
    tol           = tol
  )
}


#' Construct an initial guess for logistic Emax model parameters
#'
#' Constructs a data frame of starting values and parameter bounds for the
#' logistic Emax model, using the same heuristic approach as `emax_nls_init()`
#' applied to the empirical logit scale rather than the raw response.
#'
#' @param structural_model A two-sided formula of the form response ~ exposure
#' @param covariate_model A list of two-sided formulas, each specifying a 
#' covariate model for a structural parameter
#' @param data A data frame
#'
#' @returns A data frame with columns `parameter`, `covariate`, `start`, 
#' `lower`, and `upper`
#'
#' @seealso `emax_logistic()`, `emax_logistic_options()`
#'
#' @examples
#' emax_logistic_init(
#'   structural_model = rsp_2 ~ exp_1,
#'   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
#'   data = emax_df
#' )
#'
#' @export
emax_logistic_init <- function(structural_model, covariate_model, data) {
  .emax_logistic_init(structural_model, covariate_model, data)
}