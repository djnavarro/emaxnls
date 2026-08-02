
.emax_scm_forward <- function(mod, candidates, threshold, criterion = "p-value", seed = NULL) {
  .assert(.is_emaxnls(mod), "`mod` must be an emaxnls object")
  .assert(.is_scalar_num(threshold), "`threshold` must be a single number")
  .assert(.is_scalar_chr(criterion), "`criterion` must be a single character string")
  .assert(criterion %in% c("p-value", "aic", "bic"), '`criterion` must be "p-value", "aic", or "bic"')
  .assert(.is_scalar_num(seed) | is.null(seed), "`seed` must be NULL or a single number")
  .validate_candidate_list(candidates, names(mod$data))

  if (!is.null(seed)) set.seed(seed)
  finished <- FALSE
  while(!finished) {
    old_mod <- mod
    mod <- .emax_once_forward(mod, candidates, threshold, criterion)
    if (.is_same(mod, old_mod)) finished <- TRUE
  }
  return(mod)
}

.emax_scm_backward <- function(mod, candidates, threshold, criterion = "p-value", seed = NULL) {
  .assert(.is_emaxnls(mod), "`mod` must be an emaxnls object")
  .assert(.is_scalar_num(threshold), "`threshold` must be a single number")
  .assert(.is_scalar_chr(criterion), "`criterion` must be a single character string")
  .assert(criterion %in% c("p-value", "aic", "bic"), '`criterion` must be "p-value", "aic", or "bic"')
  .assert(.is_scalar_num(seed) | is.null(seed), "`seed` must be NULL or a single number")
  .validate_candidate_list(candidates, names(mod$data))

  if (!is.null(seed)) set.seed(seed)
  finished <- FALSE
  while(!finished) {
    old_mod <- mod
    mod <- .emax_once_backward(mod, candidates, threshold, criterion)
    if (.is_same(mod, old_mod)) finished <- TRUE
  }
  return(mod)
}


# stepwise add/remove functions -------------------------------------------

.emax_scm_history <- function(mod, is_final = FALSE) {
  .assert(.is_emaxnls(mod), "`mod` must be an emaxnls object")
  .assert(.is_scalar_lgl(is_final), "`is_final` must be a single logical value")

  history <- .get_scm_history(mod)
  if (is.null(history)) {
    history <- .tibble(
      iteration = 0L,
      attempt = 0L,
      step = "base model",
      criterion = NA_character_,
      action = NA_character_,
      term_tested = NA_character_,
      model_tested = .get_short_formula(mod),
      model_converged = .is_converged(mod),
      convergence_reason = .convergence_reason(mod),
      term_p_value = NA_real_,
      model_aic = as.numeric(stats::AIC(mod)), # coercion for emaxnls_null cases
      model_bic = as.numeric(stats::BIC(mod)),
      model_updated = NA
    )
  }
  if (is_final) {
    history <- .add_row(
      history,
      iteration = max(history$iteration) + 1L,
      attempt = max(history$attempt) + 1L,
      step = "final model",
      criterion = NA_character_,
      action = NA_character_,
      term_tested = NA_character_,
      model_tested = .get_short_formula(mod),
      model_converged = .is_converged(mod),
      convergence_reason = .convergence_reason(mod),
      term_p_value = NA_real_,
      model_aic = as.numeric(stats::AIC(mod)), # coercion for emaxnls_null cases
      model_bic = as.numeric(stats::BIC(mod)),
      model_updated = NA
    )
  }
  return(history)
}

.emax_once_forward <- function(mod, candidates, threshold, criterion = "p-value") {

  quiet <- TRUE
  history <- TRUE

  # note: checking is limited here. in future, throw an error if
  # candidates implies a sigmoidal model but mod is hyperbolic or
  # vice versa
  terms <- .emax_extract_terms(candidates)
  terms <- sample(terms)

  if (history) {
    scm_history <- .emax_scm_history(mod)
    iter <- max(scm_history$iteration) + 1L
    attm <- max(scm_history$attempt)
  }

  use_ic <- criterion %in% c("aic", "bic")
  ic_fn <- if (criterion == "bic") stats::BIC else stats::AIC

  # best_metric tracks the selection criterion across candidates:
  # for "p-value": the lowest p-value seen so far (initialised at threshold)
  # for "aic"/"bic": the lowest IC seen so far (initialised at current model IC)
  best_metric <- if (use_ic) as.numeric(ic_fn(mod)) else threshold
  best_mod <- mod
  best_mod_attm <- NA_integer_
  new_term <- NULL

  for(t in terms) {
    candidate_mod <- .emax_add_term(mod, formula = t, quiet = TRUE)
    if (!.is_same(mod, candidate_mod)) { # don't compare to self
      attm <- attm + 1L
      p <- NA_real_
      converge <- .is_converged(candidate_mod)
      converge_reason <- .convergence_reason(candidate_mod)
      if (!quiet) .inform("try add: ", deparse(t))
      if (converge) {  # skip if nls() fails
        if (use_ic) {
          candidate_ic <- as.numeric(ic_fn(candidate_mod))
          if (candidate_ic < best_metric) {
            best_mod <- candidate_mod
            best_mod_attm <- attm
            new_term <- t
            best_metric <- candidate_ic
          }
        } else {
          p <- .anova_p(mod, candidate_mod)
          if (p < best_metric) {
            best_mod <- candidate_mod
            best_mod_attm <- attm
            new_term <- t
            best_metric <- p
          }
        }
      }
      if (history) {
        scm_history <- .add_row(
          scm_history,
          iteration = iter,
          attempt = attm,
          step = "forward",
          criterion = criterion,
          action = "add",
          term_tested = deparse(t),
          model_tested = .get_short_formula(candidate_mod),
          model_converged = converge,
          convergence_reason = converge_reason,
          term_p_value = p,
          model_aic = as.numeric(stats::AIC(candidate_mod)),
          model_bic = as.numeric(stats::BIC(candidate_mod)),
          model_updated = FALSE # default
        )
      }
    }
  }

  if (history) {
    scm_history$model_updated <- with(scm_history, .case_when(
        iteration == iter & attempt == best_mod_attm ~ TRUE,
        TRUE ~ model_updated
    ))
    best_mod <- .set_scm_history(best_mod, scm_history)
  }

  return(best_mod)
}

.emax_once_backward <- function(mod, candidates, threshold, criterion = "p-value") {

  quiet <- TRUE
  history <- TRUE

  terms <- .emax_extract_terms(candidates)
  terms <- sample(terms)

  if (history) {
    scm_history <- .emax_scm_history(mod)
    iter <- max(scm_history$iteration) + 1L
    attm <- max(scm_history$attempt)
  }

  use_ic <- criterion %in% c("aic", "bic")
  ic_fn <- if (criterion == "bic") stats::BIC else stats::AIC

  # best_metric tracks the selection criterion across candidates:
  # for "p-value": the highest p-value seen so far (initialised at threshold)
  # for "aic"/"bic": the lowest IC seen so far (initialised at current model IC)
  best_metric <- if (use_ic) as.numeric(ic_fn(mod)) else threshold
  best_mod <- mod
  best_mod_attm <- NA_integer_
  new_term <- NULL

  for(t in terms) {
    candidate_mod <- .emax_remove_term(mod, formula = t, quiet = TRUE)
    if (!.is_same(mod, candidate_mod)) { # don't compare to self
      attm <- attm + 1L
      p <- NA_real_
      converge <- .is_converged(candidate_mod)
      converge_reason <- .convergence_reason(candidate_mod)
      if (!quiet) .inform("try remove: ", deparse(t))
      if (converge) {  # skip if nls() fails
        if (use_ic) {
          candidate_ic <- as.numeric(ic_fn(candidate_mod))
          if (candidate_ic < best_metric) {
            best_mod <- candidate_mod
            best_mod_attm <- attm
            new_term <- t
            best_metric <- candidate_ic
          }
        } else {
          p <- .anova_p(candidate_mod, mod)
          if (p > best_metric) {
            best_mod <- candidate_mod
            best_mod_attm <- attm
            new_term <- t
            best_metric <- p
          }
        }
      }
      if (history) {
        scm_history <- .add_row(
          scm_history,
          iteration = iter,
          attempt = attm,
          step = "backward",
          criterion = criterion,
          action = "remove",
          term_tested = deparse(t),
          model_tested = .get_short_formula(candidate_mod),
          model_converged = converge,
          convergence_reason = converge_reason,
          term_p_value = p,
          model_aic = as.numeric(stats::AIC(candidate_mod)),
          model_bic = as.numeric(stats::BIC(candidate_mod)),
          model_updated = FALSE # default
        )
      }
    }
  }

  if (history) {
    scm_history$model_updated <- with(scm_history, .case_when(
        iteration == iter & attempt == best_mod_attm ~ TRUE,
        TRUE ~ model_updated
    ))
    best_mod <- .set_scm_history(best_mod, scm_history)
  }

  return(best_mod)
}


# list of all possible terms that could be considered
.emax_extract_terms <- function(candidates) {
  cc <- unlist(.imap(
    .x = candidates,
    .f = function(x, l) paste(l, x, sep = "~")
  ))
  unname(.map(
    .x = cc,
    .f = stats::as.formula
  ))
}


# model comparison functions ----------------------------------------------

.anova_p <- function(obj1, obj2) {
  a <- stats::anova(obj1, obj2)
  if (.is_emaxlogistic(obj1)) {
    return(a$`Pr(>Chi)`[2L])
  }
  return(a$`Pr(>F)`[2L])
}

.aic_diff <- function(obj1, obj2) {
  aic1 <- as.numeric(stats::AIC(obj1))
  aic2 <- as.numeric(stats::AIC(obj2))
  return(aic1 - aic2)
}
