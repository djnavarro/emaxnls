
# add/remove terms --------------------------------------------------------

#' @param object An emaxnls object
#' @param formula A formula such as E0 ~ AGE
#' @param quiet When quiet=TRUE messages and warnings are suppressed
#' @noRd
.emax_add_term <- function(object, formula, quiet = FALSE) {

  # assertions
  .assert(inherits(object, "emaxnls"))
  .assert(inherits(formula, "formula"))
  .assert(length(formula) == 3)

  # components
  str_param <- as.character(formula[[2]])
  cov_param <- as.character(formula[[3]])

  # more assertions
  if (.extract_model_type(object) == "hyperbolic") {
    .assert(str_param %in% c("E0", "Emax", "logEC50"))
  }
  if (.extract_model_type(object) == "sigmoidal") {
    .assert(str_param %in% c("E0", "Emax", "logEC50", "logHill"))
  }
  .assert(cov_param %in% names(.extract_data(object)))

  # stop if covariate is already included
  if (cov_param %in% all.vars(.extract_covariate_formula(object, str_param)[[3]])) {
    if (!quiet) {
      msg <- paste0("cannot add: `", deparse(formula), "` is already in the model")
      rlang::inform(msg, class = "emaxnls_message")
    }
    return(object)
  }

  # update covariate model
  covariate_model <- .extract_covariate_formula(object)
  old <- deparse(covariate_model[[str_param]])
  new <- stats::as.formula(paste(old, cov_param, sep = " + "))
  covariate_model[[str_param]] <- new

  # re-run
  updated <- .emax_nls(
    structural_model = .extract_structural_formula(object),
    covariate_model = covariate_model,
    data = .extract_data(object),
    quiet = quiet
  )

  return(updated)
}

#' @param object An emaxnls object
#' @param formula A formula such as E0 ~ AGE
#' @param quiet When quiet=TRUE messages and warnings are suppressed
#' @noRd
.emax_remove_term <- function(object, formula, quiet = FALSE) {

  # assertions
  .assert(inherits(object, "emaxnls"))
  .assert(inherits(formula, "formula"))
  .assert(length(formula) == 3)

  # components
  str_param <- as.character(formula[[2]])
  cov_param <- as.character(formula[[3]])

  # more assertions
  if(.extract_model_type(object) == "hyperbolic") {
    .assert(str_param %in% c("E0", "Emax", "logEC50"))
  }
  if(.extract_model_type(object) == "sigmoidal") {
    .assert(str_param %in% c("E0", "Emax", "logEC50", "logHill"))
  }
  .assert(cov_param %in% names(.extract_data(object)))

  # stop if covariate is not already included
  if (!(cov_param %in% all.vars(.extract_covariate_formula(object, str_param)[[3]]))) {
    if (!quiet) {
      msg <- paste0("cannot remove: `", deparse(formula), "` is not in the model")
      rlang::inform(msg, class = "emaxnls_message")
    }
    return(object)
  }

  # update covariate model
  covariate_model <- .extract_covariate_formula(object)
  old_vars <- all.vars(covariate_model[[str_param]][[3]])
  new_vars <- setdiff(old_vars, cov_param)
  if (length(new_vars) == 0) new_vars <- "1"
  new <- stats::as.formula(paste(
    str_param, "~", paste(new_vars, collapse = " + ")
  ))
  covariate_model[[str_param]] <- new

  # re-run
  updated <- .emax_nls(
    structural_model = .extract_structural_formula(object),
    covariate_model = covariate_model,
    data = .extract_data(object),
    quiet = quiet
  )

  return(updated)
}
