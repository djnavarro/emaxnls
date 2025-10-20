

# model comparison functions ----------------------------------------------

.anova_p <- function(obj1, obj2) {
  a <- stats::anova(obj1$result, obj2$result)
  return(a$`Pr(>F)`[2])
}

.aic_diff <- function(obj1, obj2) {
  aic1 <- stats::AIC(obj1$result)
  aic2 <- stats::AIC(obj2$result)
  return(aic1 - aic2)
}

.show_p <- function(p_value) {
  f <- scales::label_pvalue()
  f(p_value)
}

# stepwise add/remove functions -------------------------------------------


# list of all possible terms that could be considered
.emax_extract_terms <- function(candidates) {
  candidates |>
    purrr::imap(\(x, l) paste(l, x, sep = "~")) |>
    unlist() |>
    purrr::map(stats::as.formula) |>
    unname()
}

.emax_history <- function(mod) {
  history <- attr(mod, "history")
  if (is.null(history)) {
    history <- tibble::tibble(
      iteration = 0L,
      step = "initial",
      action = NA_character_,
      term = NA_character_,
      p_value = NA_real_
    )
  }
  return(history)
}

.emax_once_forward <- function(mod,
                              candidates,
                              threshold = .01,
                              quiet = FALSE,
                              history = TRUE) {

  # note: checking is limited here. in future, throw an error if
  # candidates implies a sigmoidal model but mod is hyperbolic or
  # vice versa
  .assert(inherits(mod, "emaxnls"))
  .validate_candidate_list(candidates, names(mod$data))
  terms <- .emax_extract_terms(candidates)
  terms <- sample(terms)

  if (history) scm_history <- .emax_history(mod)

  # note for future development: this implementation hard-codes the
  # assumption that selection is based on p-values
  lowest_p <- threshold
  best_mod <- mod
  new_term <- NULL
  for(t in terms) {
    candidate_mod <- .emax_add_term(mod, formula = t, quiet = TRUE)
    if (!.emax_identical(mod, candidate_mod)) { # don't compare to self
      if (!quiet) message("try add: ", deparse(t))
      if (!is.null(candidate_mod$result)) {  # skip if nls() fails
        p <- .anova_p(mod, candidate_mod)
        if (p < lowest_p) {
          best_mod <- candidate_mod
          new_term <- t
          lowest_p <- p
        }
      }
    }
  }

  if (!quiet & !is.null(new_term)) {
    message("addition: ", deparse(new_term), " p: ", .show_p(lowest_p))
  }
  if (!quiet & is.null(new_term)) {
    message("no improvements found")
  }

  if (history & !is.null(new_term)) {
    iteration <- max(scm_history$iteration) + 1L
    scm_history <- scm_history |>
      tibble::add_row(
        iteration = iteration,
        step = "forward",
        action = "add",
        term = deparse(new_term),
        p_value = lowest_p
      )
    attr(best_mod, "history") <- scm_history
  }

  return(best_mod)
}

.emax_once_backward <- function(mod,
                               candidates,
                               threshold = .001,
                               quiet = FALSE,
                               history = TRUE) {

  # note: checking is limited here. in future, throw an error if
  # candidates implies a sigmoidal model but mod is hyperbolic or
  # vice versa
  .assert(inherits(mod, "emaxnls"))
  .validate_candidate_list(candidates, names(mod$data))
  terms <- .emax_extract_terms(candidates)
  terms <- sample(terms)

  if (history) scm_history <- .emax_history(mod)

  # note for future development: this implementation hard-codes the
  # assumption that selection is based on p-values
  highest_p <- threshold
  best_mod <- mod
  new_term <- NULL
  for(t in terms) {
    candidate_mod <- .emax_remove_term(mod, formula = t, quiet = TRUE)
    if (!.emax_identical(mod, candidate_mod)) { # don't compare to self
      nls_fail <- is.null(candidate_mod$result)
      if (!nls_fail) {  # skip if nls() fails
        p <- .anova_p(candidate_mod, mod)
        if (p > highest_p) {
          best_mod <- candidate_mod
          new_term <- t
          highest_p <- p
        }
      }
      if (!quiet & nls_fail) {
        message("try remove: ", deparse(t), " [nls fail]")
      }
      if (!quiet & !nls_fail) {
        message("try remove: ", deparse(t), " p: ", .show_p(p))
      }
    }
  }

  if (!quiet & !is.null(new_term)) {
    message("removal: ", deparse(new_term), " p: ", .show_p(highest_p))
  }
  if (!quiet & is.null(new_term)) {
    message("no improvements found")
  }

  if (history & !is.null(new_term)) {
    iteration <- max(scm_history$iteration) + 1L
    scm_history <- scm_history |>
      tibble::add_row(
        iteration = iteration,
        step = "backward",
        action = "remove",
        term = deparse(new_term),
        p_value = highest_p
      )
    attr(best_mod, "history") <- scm_history
  }

  return(best_mod)
}

.emax_forward <- function(mod,
                         candidates,
                         threshold = .01,
                         quiet = FALSE,
                         history = TRUE,
                         seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  finished <- FALSE
  while(!finished) {

    old_mod <- mod
    mod <- .emax_once_forward(mod, candidates, threshold, quiet, history)
    if (.emax_identical(mod, old_mod)) finished <- TRUE

  }

  return(mod)
}

.emax_backward <- function(mod,
                          candidates,
                          threshold = .001,
                          quiet = FALSE,
                          history = TRUE,
                          seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  finished <- FALSE
  while(!finished) {

    old_mod <- mod
    mod <- .emax_once_backward(mod, candidates, threshold, quiet, history)
    if (.emax_identical(mod, old_mod)) finished <- TRUE

  }

  return(mod)
}
