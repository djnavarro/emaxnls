
.validate_structural_formula <- function(formula, names = NULL) {

  # check that it is a two-sided formula with one variable on each side
  .assert(inherits(formula, "formula"))
  .assert(length(formula) == 3)
  .assert(length(all.vars(formula)) == 2)

  # check that the variables refer to known variable names
  if (!is.null(names)) .assert(all(all.vars(formula) %in% names))

}

.validate_covariate_formula <- function(formula, names = NULL) {

  # check that "formula" is a list of 3 or 4 two-sided formulas
  .assert(inherits(formula, "list"))
  .assert(length(formula) %in% 3:4)
  .assert(all(.map_lgl(formula, function(x) .is_formula(x, sides = 2))))

  # check that the LHS names correspond to the structural parameters
  lhs_names <- unname(sort(.map_chr(formula, function(x) all.vars(x[[2]]))))
  if (length(lhs_names) == 3) .assert(lhs_names == c("E0", "Emax", "logEC50"))
  if (length(lhs_names) == 4) .assert(lhs_names == c("E0", "Emax", "logEC50", "logHill"))

  # check that RHS names refer to known variables
  if (!is.null(names)) {
    rhs_names <- unique(unlist(.map(
      .x = formula, 
      .f = function(x) all.vars(x[[3]])
    )))
    .assert(all(rhs_names %in% names))
  }
}

.validate_candidate_list <- function(candidates, names) {

  .assert(inherits(candidates, "list"))
  .assert(length(candidates) <= 4)
  .assert(all(names(candidates) %in% c("E0", "Emax", "logEC50", "logHill")))
  .assert(length(unique(names(candidates))) == length(names(candidates)))
  .assert(all(.map_lgl(candidates, is.character)))
  .assert(all(unlist(candidates) %in% names))

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