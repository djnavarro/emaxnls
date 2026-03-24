
# add/remove terms --------------------------------------------------------

#' @param mod An emaxnls object
#' @param formula A formula such as E0 ~ AGE
#' @param quiet Logical
#' @noRd
.emax_add_term <- function(mod, formula, quiet = NULL) {

  if (is.null(quiet)) quiet <- .get_options(mod)$quiet

  .assert(.is_scalar_lgl(quiet), "`quiet` must be a single logical value")
  .assert(.is_emaxnls(mod), "`mod` must be an emaxnls object")
  .validate_term_formula(formula, .get_model_type(mod), names(.get_data(mod)))

  str_param <- as.character(formula[[2]])
  cov_param <- as.character(formula[[3]])

  # stop if covariate is already included
  if (cov_param %in% all.vars(.get_covariate_formula(mod, str_param)[[3]])) {
    if (!quiet) {
      msg <- paste0("cannot add: `", deparse(formula), "` is already in the model")
      .inform(msg)
    }
    return(mod)
  }

  # update covariate model
  covariate_model <- .get_covariate_formula(mod)
  structural_model <- .get_structural_formula(mod)
  old <- deparse(covariate_model[[str_param]])
  new <- stats::as.formula(paste(old, cov_param, sep = " + "))
  covariate_model[[str_param]] <- new

  # initial parameter guess for updated model
  tmp <- .construct_design(structural_model, covariate_model, .get_data(mod))
  init <- .guess_init(
    variables = .construct_variables(structural_model, covariate_model, tmp$lookup),
    design = tmp$design
  )

  # re-run
  updated <- .emax_nls(
    structural_model = structural_model,
    covariate_model = covariate_model,
    data = .get_data(mod),
    init = init,
    opts = .get_options(mod)
  )

  return(updated)
}

#' @param mod An emaxnls object
#' @param formula A formula such as E0 ~ AGE
#' @param quiet Logical
#' @noRd
.emax_remove_term <- function(mod, formula, quiet = NULL) {

  if (is.null(quiet)) quiet <- .get_options(mod)$quiet

  .assert(.is_scalar_lgl(quiet), "`quiet` must be a single logical value")
  .assert(.is_emaxnls(mod), "`mod` must be an emaxnls object")
  .validate_term_formula(formula, .get_model_type(mod), names(.get_data(mod)))

  str_param <- as.character(formula[[2]])
  cov_param <- as.character(formula[[3]])

  # stop if covariate is not already included
  if (!(cov_param %in% all.vars(.get_covariate_formula(mod, str_param)[[3]]))) {
    if (!quiet) {
      msg <- paste0("cannot remove: `", deparse(formula), "` is not in the model")
      .inform(msg)
    }
    return(mod)
  }

  # update covariate model
  covariate_model <- .get_covariate_formula(mod)
  structural_model <- .get_structural_formula(mod)
  old_vars <- all.vars(covariate_model[[str_param]][[3]])
  new_vars <- setdiff(old_vars, cov_param)
  if (length(new_vars) == 0) new_vars <- "1"
  new <- stats::as.formula(paste(
    str_param, "~", paste(new_vars, collapse = " + ")
  ))
  covariate_model[[str_param]] <- new

  # initial parameter guess for updated model
  tmp <- .construct_design(structural_model, covariate_model, .get_data(mod))
  init <- .guess_init(
    variables = .construct_variables(structural_model, covariate_model, tmp$lookup),
    design = tmp$design
  )

  # re-run
  updated <- .emax_nls(
    structural_model = structural_model,
    covariate_model = covariate_model,
    data = .get_data(mod),
    init = init,
    opts = .get_options(mod)
  )

  return(updated)
}
