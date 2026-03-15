
.validate_structural_formula <- function(formula, names = NULL) {

  errmsg <- "`structural_formula` must be a two-sided formula of the form `response ~ exposure`"
  .assert(inherits(formula, "formula"), errmsg)
  .assert(length(formula) == 3, errmsg)
  .assert(length(all.vars(formula)) == 2, errmsg)

  errmsg <- "variables in `structural_formula` must exist in the data"
  if (!is.null(names)) .assert(all(all.vars(formula) %in% names), errmsg)

}

.validate_covariate_formula <- function(formula, names = NULL) {

  errmsg <- "`covariate_formula` must be a list of 3 or 4 two-sided formulas"
  .assert(inherits(formula, "list"), errmsg)
  .assert(length(formula) %in% 3:4, errmsg)
  .assert(all(.map_lgl(formula, function(x) .is_formula(x, sides = 2))), errmsg)

  errmsg <- "variables on the left of `covariate_formula` must refer to structural parameters (E0, Emax, logEC50, logHill)"
  lhs_names <- unname(sort(.map_chr(formula, function(x) all.vars(x[[2]]))))
  if (length(lhs_names) == 3) .assert(lhs_names == c("E0", "Emax", "logEC50"), errmsg)
  if (length(lhs_names) == 4) .assert(lhs_names == c("E0", "Emax", "logEC50", "logHill"), errmsg)

  if (!is.null(names)) {
    errmsg <- "variables on the right of `covariate_formula` must exist in the data"
    rhs_names <- unique(unlist(.map(
      .x = formula, 
      .f = function(x) all.vars(x[[3]])
    )))
    .assert(all(rhs_names %in% names), errmsg)
  }
}

.validate_candidate_list <- function(candidates, names) {

  errmsg <- "`candidates` must be a named list, with unique names in c('E0', 'Emax', 'logEC50', 'logHill')"
  .assert(inherits(candidates, "list"), errmsg)
  .assert(length(candidates) <= 4, errmsg)
  .assert(all(names(candidates) %in% c("E0", "Emax", "logEC50", "logHill")), errmsg)
  .assert(length(unique(names(candidates))) == length(names(candidates)), errmsg)

  errmsg <- "variables listed in `candidates` must exist in the data"
  .assert(all(.map_lgl(candidates, is.character)), errmsg)
  .assert(all(unlist(candidates) %in% names), errmsg)

}

.validate_optim_method <- function(optim_method) {
  .assert(
    expr = optim_method %in% c("gauss", "port", "levenberg"),
    message = "`optim_method` must be one of 'gauss', 'port', or 'levenberg'"
  )
  if (optim_method == "levenberg") {
    rlang::check_installed(
      pkg = "minpack.lm",
      reason = "`optim_method = 'levenberg' requires the minpack.lm package" 
    )
  }
}

.is_same <- function(mod1, mod2) {
  identical(
    .get_coefficient_names(mod1), 
    .get_coefficient_names(mod2)
  )
}

.is_formula <- function(x, sides = NULL) {
  out <- inherits(x, "formula")
  if (!is.null(sides)) {
    if (sides == 1) out <- out && length(x) == 2
    if (sides == 2) out <- out && length(x) == 3
  }
  out
}

.is_sigmoidal <- function(covariate_model) {
  length(covariate_model) == 4
}

.is_hyperbolic <- function(covariate_model) {
  length(covariate_model) == 3
}