
.emax_logistic <- function(structural_model,
                           covariate_model,
                           data,
                           init,
                           opts) {

  .validate_structural_formula(structural_model, names(data))
  .validate_covariate_formula(covariate_model, names(data))

  names(covariate_model) <- .map_chr(covariate_model, function(x) as.character(x[[2]]))

  if (is.null(opts)) opts <- emax_logistic_options()

  tmp <- .construct_design(structural_model, covariate_model, data)

  obj <- list(
    formula = list(
      structural = structural_model,
      covariate  = covariate_model,
      expanded   = NULL
    ),
    data = data,
    info = list(
      opts       = opts,
      init       = NULL,
      design     = tmp$design,
      model_type = .construct_model_type(covariate_model),
      variables  = .construct_variables(structural_model, covariate_model, tmp$lookup)
    )
  )

  if (is.null(init)) init <- .guess_init_logistic(obj$info$variables, obj$info$design)
  obj$info$init <- init
  obj$formula$expanded <- .construct_expanded_formula(obj$info$variables)
  obj$env <- .construct_env(obj)

  # run IRLS
  irls_out <- .irls_loop(obj, opts)

  # store results
  obj$env$model <- irls_out$result
  obj$env$error <- irls_out$error
  obj$env$irls  <- irls_out$irls

  if (!is.null(obj$env$error) & !opts$quiet) {
    rlang::warn("`nls()` did not converge during IRLS", class = "emaxnls_warning")
  } else if (!obj$env$irls$converged & !opts$quiet) {
    rlang::warn(
      paste0("IRLS did not converge within ", opts$max_iter, " iterations"),
      class = "emaxnls_warning"
    )
  }

  return(structure(obj, class = c("emaxlogistic", "emaxnls")))
}


# IRLS outer loop -------------------------------------------------------

.irls_loop <- function(obj, opts) {

  rsp_var <- .get_response_name(obj)
  y       <- obj$env$design[[rsp_var]]

  # GLM-style initialisation: mu = (y + 0.5) / 2 avoids degenerate weights
  # at y = 0 or y = 1 (same approach as stats::glm() for binomial)
  mu  <- (y + 0.5) / 2
  eta <- .logit(mu)

  converged <- FALSE
  result    <- NULL
  error     <- NULL

  for (iter in seq_len(opts$max_iter)) {

    mu <- .expit(eta)
    w  <- mu * (1 - mu)
    z  <- eta + (y - mu) / w

    # update working response and weights in the env
    obj$env$design[[rsp_var]] <- z
    obj$env$weights <- w

    tmp <- evalq(
      .nls_call(
        formula   = formula,
        data      = design,
        start     = start,
        control   = control,
        algorithm = algorithm,
        lower     = lower,
        upper     = upper,
        weights   = weights
      ),
      envir = obj$env
    )

    if (!is.null(tmp$error)) {
      error <- tmp$error
      break
    }

    result <- tmp$result

    # warm-start next iteration from current estimates
    obj$env$start <- stats::coef(result)

    eta_new <- stats::fitted(result)
    dev_change <- abs(
      .binomial_deviance(y, .expit(eta_new)) - .binomial_deviance(y, mu)
    )
    eta <- eta_new

    if (dev_change < opts$tol) {
      converged <- TRUE
      break
    }
  }

  list(
    result = result,
    error  = error,
    irls   = list(iter = iter, converged = converged)
  )
}


# helper functions -------------------------------------------------------

.expit <- function(x) {
  1 / (1 + exp(-x))
}

.logit <- function(p) {
  log(p / (1 - p))
}

.clamp <- function(x, lo, hi) {
  pmin(pmax(x, lo), hi)
}

.binomial_deviance <- function(y, mu) {
  eps <- .Machine$double.eps
  mu  <- pmin(pmax(mu, eps), 1 - eps)
  -2 * sum(y * log(mu) + (1 - y) * log(1 - mu))
}
