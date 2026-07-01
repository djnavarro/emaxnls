
# coef, vcov, confint, nobs are inherited from emaxnls without override.
# All delegate to the final nls object stored in obj$env$model, which
# is valid because IRLS/Fisher scoring gives the MLE at convergence and
# the NLS Jacobian at convergence gives the correct asymptotic vcov.


#' @rdname df.residual
#' @exportS3Method stats::df.residual
df.residual.emaxlogistic <- function(object, ...) {
  if (!.is_converged(object)) return(.nls_null())
  stats::nobs(object) - length(stats::coef(object))
}


#' @rdname fitted
#' @exportS3Method stats::fitted
fitted.emaxlogistic <- function(object, type = c("response", "link"), ...) {
  if (!.is_converged(object)) return(.nls_null())
  type <- match.arg(type)
  eta <- evalq(stats::fitted(model), envir = object$env)
  if (type == "link") return(eta)
  .expit(eta)
}


#' @rdname residuals
#' @exportS3Method stats::residuals
residuals.emaxlogistic <- function(object, type = c("pearson", "deviance"), ...) {
  if (!.is_converged(object)) return(.nls_null())
  type <- match.arg(type)

  rsp_var <- .get_response_name(object)
  y   <- object$data[[rsp_var]]
  mu  <- stats::fitted(object, type = "response")

  if (type == "pearson") {
    return((y - mu) / sqrt(mu * (1 - mu)))
  }
  # deviance residuals: signed sqrt of per-observation deviance contribution
  sign(y - mu) * sqrt(-2 * (y * log(mu) + (1 - y) * log(1 - mu)))
}


#' @rdname logLik
#' @exportS3Method stats::logLik
logLik.emaxlogistic <- function(object, REML = FALSE, ...) {
  if (!.is_converged(object)) return(.nls_null())
  rsp_var <- .get_response_name(object)
  y   <- object$data[[rsp_var]]
  mu  <- stats::fitted(object, type = "response")
  ll  <- sum(y * log(mu) + (1 - y) * log(1 - mu))
  npar <- length(stats::coef(object))
  structure(ll, df = npar, nobs = length(y), class = "logLik")
}


#' @rdname deviance
#' @exportS3Method stats::deviance
deviance.emaxlogistic <- function(object, ...) {
  if (!.is_converged(object)) return(.nls_null())
  as.numeric(-2 * stats::logLik(object))
}


#' @rdname AIC
#' @exportS3Method stats::AIC
AIC.emaxlogistic <- function(object, ..., k = 2) {
  emaxlogistic_mods <- list(object, ...)
  x <- match.call()
  mod_names <- unlist(unname(lapply(as.list(x[-1]), as.character)))
  mod_names <- mod_names[seq_along(emaxlogistic_mods)]
  if (length(emaxlogistic_mods) == 1L & !.is_converged(object)) return(.nls_null())
  converged <- unlist(.map(emaxlogistic_mods, .is_converged))
  if (!all(converged)) {
    .warn("dropping non-converging models")
    emaxlogistic_mods <- emaxlogistic_mods[converged]
    mod_names <- mod_names[converged]
  }
  aic_vals <- unlist(.map(
    .x = emaxlogistic_mods,
    .f = function(mm) {
      ll   <- stats::logLik(mm)
      npar <- attr(ll, "df")
      as.numeric(-2 * ll + k * npar)
    }
  ))
  if (length(emaxlogistic_mods) == 1L) return(aic_vals[[1L]])
  df_vals <- unlist(.map(
    .x = emaxlogistic_mods,
    .f = function(mm) attr(stats::logLik(mm), "df")
  ))
  out <- data.frame(df = df_vals, AIC = aic_vals, row.names = mod_names)
  out
}


#' @rdname AIC
#' @exportS3Method stats::BIC
BIC.emaxlogistic <- function(object, ...) {
  emaxlogistic_mods <- list(object, ...)
  x <- match.call()
  mod_names <- unlist(unname(lapply(as.list(x[-1]), as.character)))
  mod_names <- mod_names[seq_along(emaxlogistic_mods)]
  if (length(emaxlogistic_mods) == 1L & !.is_converged(object)) return(.nls_null())
  converged <- unlist(.map(emaxlogistic_mods, .is_converged))
  if (!all(converged)) {
    .warn("dropping non-converging models")
    emaxlogistic_mods <- emaxlogistic_mods[converged]
    mod_names <- mod_names[converged]
  }
  bic_vals <- unlist(.map(
    .x = emaxlogistic_mods,
    .f = function(mm) {
      ll   <- stats::logLik(mm)
      npar <- attr(ll, "df")
      n    <- attr(ll, "nobs")
      as.numeric(-2 * ll + log(n) * npar)
    }
  ))
  if (length(emaxlogistic_mods) == 1L) return(bic_vals[[1L]])
  df_vals <- unlist(.map(
    .x = emaxlogistic_mods,
    .f = function(mm) attr(stats::logLik(mm), "df")
  ))
  out <- data.frame(df = df_vals, BIC = bic_vals, row.names = mod_names)
  out
}


#' @rdname anova
#' @exportS3Method stats::anova
anova.emaxlogistic <- function(object, ...) {
  emaxlogistic_mods <- list(object, ...)
  converged <- unlist(.map(emaxlogistic_mods, .is_converged))
  if (!all(converged)) {
    .warn("dropping non-converging models")
    emaxlogistic_mods <- emaxlogistic_mods[converged]
  }
  if (length(emaxlogistic_mods) < 2L) {
    .warn("anova is only defined for sequences of models")
    return(NULL)
  }

  deviances <- unlist(.map(emaxlogistic_mods, stats::deviance))
  npar      <- unlist(.map(emaxlogistic_mods, function(m) attr(stats::logLik(m), "df")))

  df_diff  <- c(NA_integer_, diff(npar))
  lrt      <- c(NA_real_, -diff(deviances))
  p_values <- c(NA_real_, stats::pchisq(lrt[-1], df = df_diff[-1], lower.tail = FALSE))

  out <- data.frame(
    Df        = npar,
    Deviance  = deviances,
    Df_diff   = df_diff,
    LRT       = lrt,
    `Pr(>Chi)` = p_values,
    check.names = FALSE
  )
  class(out) <- c("anova", "data.frame")
  out
}


#' @rdname predict
#' @exportS3Method stats::predict
predict.emaxlogistic <- function(object,
                                 newdata = NULL,
                                 type = c("response", "link"),
                                 se.fit = FALSE,
                                 interval = "none",
                                 level = 0.95,
                                 ...) {
  if (!.is_converged(object)) return(.nls_null())
  type <- match.arg(type)

  # Coerce to base data.frame: .fgrad() uses [i, v] subsetting which returns
  # a 1x1 tibble (not a scalar) for tibble inputs, breaking formula evaluation
  if (!is.null(newdata)) newdata <- as.data.frame(newdata)

  out <- .predict_nls(
    object$env$model,
    newdata,
    se.fit,
    interval,
    level,
    ...
  )

  if (type == "link") return(out)

  # transform to probability scale.
  # .predict_nls() has four possible return types depending on se.fit/interval:
  #   - numeric vector      (se.fit=FALSE, interval="none")
  #   - data.frame fit/lwr/upr  (se.fit=FALSE, interval="confidence")
  #   - list(fit, se.fit, df) with numeric fit  (se.fit=TRUE, interval="none")
  #   - list(fit, se.fit, df) with data.frame fit  (se.fit=TRUE, interval="confidence")
  # The data.frame case must be checked before is.list() because data.frames are lists.
  if (is.data.frame(out)) {
    # all three columns (fit, lwr, upr) are on the link scale
    for (col in names(out)) out[[col]] <- .expit(out[[col]])
    return(out)
  }
  if (is.list(out)) {
    # $fit may be a numeric vector (no interval) or a data.frame (with interval);
    # $se.fit remains on the link scale in both cases
    if (is.data.frame(out$fit)) {
      for (col in names(out$fit)) out$fit[[col]] <- .expit(out$fit[[col]])
    } else {
      out$fit <- .expit(out$fit)
    }
    return(out)
  }
  .expit(out)
}


#' @rdname simulate
#' @exportS3Method stats::simulate
simulate.emaxlogistic <- function(object, nsim = 1, seed = NULL, ...) {
  if (!.is_converged(object)) return(.nls_null())
  .emax_logistic_resample(mod = object, nsim = nsim, seed = seed)
}

.emax_logistic_resample <- function(mod, nsim, seed) {
  rlang::check_installed(
    pkg = "mvtnorm",
    reason = "`simulate()` for logistic Emax models requires the mvtnorm package"
  )
  if (!is.null(seed)) set.seed(seed)

  mu_hat  <- stats::coef(mod)
  sigma   <- stats::vcov(mod)
  draws   <- mvtnorm::rmvnorm(n = nsim, mean = mu_hat, sigma = sigma)

  mod_fn  <- emax_fun(mod)
  n       <- stats::nobs(mod)
  out     <- matrix(NA_real_, nrow = n, ncol = nsim)
  colnames(out) <- paste0("sim_", seq_len(nsim))

  for (i in seq_len(nsim)) {
    eta_i    <- mod_fn(param = draws[i, ])
    prob_i   <- .expit(eta_i)
    out[, i] <- as.numeric(stats::runif(n) < prob_i)
  }
  as.data.frame(out)
}
