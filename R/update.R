
# add/remove terms --------------------------------------------------------

#' @param object An emaxnls_fit object
#' @param formula A formula such as E0 ~ AGE
#' @param quiet When quiet=TRUE messages and warnings are suppressed
#' @noRd
.emax_add_term <- function(object, formula, quiet = FALSE) {

  # assertions
  .assert(inherits(object, "emaxnls_fit"))
  .assert(inherits(formula, "formula"))
  .assert(length(formula) == 3)

  # components
  str_param <- as.character(formula[[2]])
  cov_param <- as.character(formula[[3]])

  # more assertions
  if (object$model_type == "hyperbolic") {
    .assert(str_param %in% c("E0", "Emax", "logEC50"))
  }
  if (object$model_type == "sigmoidal") {
    .assert(str_param %in% c("E0", "Emax", "logEC50", "logHill"))
  }
  .assert(cov_param %in% names(object$data))

  # stop if covariate is already included
  if (cov_param %in% all.vars(object$covariate_model[[str_param]][[3]])) {
    if (!quiet) {
      message("cannot add: `", deparse(formula), "` is already in the model")
    }
    return(object)
  }

  # update covariate model
  covariate_model <- object$covariate_model
  old <- deparse(covariate_model[[str_param]])
  new <- stats::as.formula(paste(old, cov_param, sep = " + "))
  covariate_model[[str_param]] <- new

  # re-run
  updated <- .emax_nls(
    structural_model = object$structural_model,
    covariate_model = covariate_model,
    data = object$data,
    quiet = quiet
  )

  return(updated)
}

#' @param object An emaxnls_fit object
#' @param formula A formula such as E0 ~ AGE
#' @param quiet When quiet=TRUE messages and warnings are suppressed
#' @noRd
.emax_remove_term <- function(object, formula, quiet = FALSE) {

  # assertions
  .assert(inherits(object, "emaxnls_fit"))
  .assert(inherits(formula, "formula"))
  .assert(length(formula) == 3)

  # components
  str_param <- as.character(formula[[2]])
  cov_param <- as.character(formula[[3]])

  # more assertions
  if(object$model_type == "hyperbolic") {
    .assert(str_param %in% c("E0", "Emax", "logEC50"))
  }
  if(object$model_type == "sigmoidal") {
    .assert(str_param %in% c("E0", "Emax", "logEC50", "logHill"))
  }
  .assert(cov_param %in% names(object$data))

  # stop if covariate is not already included
  if (!(cov_param %in% all.vars(object$covariate_model[[str_param]][[3]]))) {
    if (!quiet) {
      message("cannot remove: `", deparse(formula), "` is not in the model")
    }
    return(object)
  }

  # update covariate model
  covariate_model <- object$covariate_model
  old_vars <- all.vars(covariate_model[[str_param]][[3]])
  new_vars <- setdiff(old_vars, cov_param)
  if (length(new_vars) == 0) new_vars <- "1"
  new <- stats::as.formula(paste(
    str_param, "~", paste(new_vars, collapse = " + ")
  ))
  covariate_model[[str_param]] <- new

  # re-run
  updated <- .emax_nls(
    structural_model = object$structural_model,
    covariate_model = covariate_model,
    data = object$data,
    quiet = quiet
  )

  return(updated)
}
