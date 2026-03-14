
.emax_scm_forward <- function(mod, candidates, threshold, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  finished <- FALSE
  while(!finished) {
    old_mod <- mod
    mod <- .emax_once_forward(mod, candidates, threshold)
    if (.is_same(mod, old_mod)) finished <- TRUE
  }
  return(mod)
}

.emax_scm_backward <- function(mod, candidates, threshold, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  finished <- FALSE
  while(!finished) {
    old_mod <- mod
    mod <- .emax_once_backward(mod, candidates, threshold)
    if (.is_same(mod, old_mod)) finished <- TRUE
  }
  return(mod)
}


# stepwise add/remove functions -------------------------------------------

.emax_scm_history <- function(mod, is_final = FALSE) {
  history <- .get_scm_history(mod)
  if (is.null(history)) {
    history <- tibble::tibble(
      iteration = 0L,
      attempt = 0L,
      step = "base model",
      action = NA_character_,
      term_tested = NA_character_,
      model_tested = .get_short_formula(mod),
      model_converged = !is.null(.get_nls(mod)),
      term_p_value = NA_real_,
      model_aic = stats::AIC(mod),
      model_bic = stats::BIC(mod),
      model_updated = NA
    )
  }
  if (is_final) {
    history <- tibble::add_row(
      history,
      iteration = max(history$iteration) + 1L,
      attempt = max(history$attempt) + 1L,
      step = "final model",
      action = NA_character_,
      term_tested = NA_character_,
      model_tested = .get_short_formula(mod),
      model_converged = !is.null(.get_nls(mod)),
      term_p_value = NA_real_,
      model_aic = stats::AIC(mod),
      model_bic = stats::BIC(mod),
      model_updated = NA
    )
  }
  return(history)
}

.emax_once_forward <- function(mod, candidates, threshold) {

  quiet <- TRUE
  history <- TRUE

  # note: checking is limited here. in future, throw an error if
  # candidates implies a sigmoidal model but mod is hyperbolic or
  # vice versa
  .assert(inherits(mod, "emaxnls"))
  .validate_candidate_list(candidates, names(mod$data))
  terms <- .emax_extract_terms(candidates)
  terms <- sample(terms)

  if (history) {
    scm_history <- .emax_scm_history(mod)
    iter <- max(scm_history$iteration) + 1L
    attm <- max(scm_history$attempt)
  }
 
  # note for future development: this implementation hard-codes the
  # assumption that selection is based on p-values
  lowest_p <- threshold
  best_mod <- mod
  best_mod_attm <- NA_integer_
  new_term <- NULL
  for(t in terms) {
    candidate_mod <- .emax_add_term(mod, formula = t, quiet = TRUE)
    if (!.is_same(mod, candidate_mod)) { # don't compare to self
      attm <- attm + 1L
      p <- NA_real_
      converge <- !is.null(.get_nls(candidate_mod))
      if (!quiet) .inform("try add: ", deparse(t))
      if (converge) {  # skip if nls() fails
        p <- .anova_p(mod, candidate_mod)
        if (p < lowest_p) {
          best_mod <- candidate_mod
          best_mod_attm <- attm
          new_term <- t
          lowest_p <- p
        }
      }
      if (history) {
        scm_history <- tibble::add_row(
          scm_history,
          iteration = iter,
          attempt = attm,
          step = "forward",
          action = "add",
          term_tested = deparse(t),
          model_tested = .get_short_formula(candidate_mod),
          model_converged = converge,
          term_p_value = p,
          model_aic = stats::AIC(candidate_mod),
          model_bic = stats::BIC(candidate_mod),
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

.emax_once_backward <- function(mod, candidates, threshold) {

  quiet <- TRUE
  history <- TRUE

  # note: checking is limited here. in future, throw an error if
  # candidates implies a sigmoidal model but mod is hyperbolic or
  # vice versa
  .assert(inherits(mod, "emaxnls"))
  .validate_candidate_list(candidates, names(mod$data))
  terms <- .emax_extract_terms(candidates)
  terms <- sample(terms)

  if (history) {
    scm_history <- .emax_scm_history(mod)
    iter <- max(scm_history$iteration) + 1L
    attm <- max(scm_history$attempt)
  }

  # note for future development: this implementation hard-codes the
  # assumption that selection is based on p-values
  highest_p <- threshold
  best_mod <- mod
  best_mod_attm <- NA_integer_
  new_term <- NULL
  for(t in terms) {
    candidate_mod <- .emax_remove_term(mod, formula = t, quiet = TRUE)
    if (!.is_same(mod, candidate_mod)) { # don't compare to self
      attm <- attm + 1L
      p <- NA_real_
      converge <- !is.null(.get_nls(candidate_mod))
      if (!quiet) .inform("try remove: ", deparse(t))
      if (converge) {  # skip if nls() fails
        p <- .anova_p(candidate_mod, mod)
        if (p > highest_p) {
          best_mod <- candidate_mod
          best_mod_attm <- attm
          new_term <- t
          highest_p <- p
        }
      }
      if (history) {
        scm_history <- tibble::add_row(
          scm_history,
          iteration = iter,
          attempt = attm,
          step = "backward",
          action = "remove",
          term_tested = deparse(t),
          model_tested = .get_short_formula(candidate_mod),
          model_converged = converge,
          term_p_value = p,
          model_aic = stats::AIC(candidate_mod),
          model_bic = stats::BIC(candidate_mod),
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
  return(a$`Pr(>F)`[2])
}

.aic_diff <- function(obj1, obj2) {
  aic1 <- stats::AIC(obj1)
  aic2 <- stats::AIC(obj2)
  return(aic1 - aic2)
}

