
# catch errors if nls throws them
.nls_safe <- purrr::safely(stats::nls)


# are these two emax models the same?
# note: this should probably have a "strictness" argument governing how
# deeply we want to push the comparison
.emax_identical <- function(mod1, mod2) {
  identical(
    .extract_parameter_names(mod1), 
    .extract_parameter_names(mod2)
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

.get_model_type <- function(covariate_model) {
  if (.is_hyperbolic(covariate_model)) return("hyperbolic")
  if (.is_sigmoidal(covariate_model)) return("sigmoidal")
  stop("invalid covariate model", call. = FALSE)
}

# only of interest for expanding the model matrix
.emax_flat_formula <- function(structural_model, covariate_model) {
  s <- deparse(structural_model)
  c <- covariate_model |>
    purrr::map(\(x) all.vars(x[[3]])) |>
    unlist() |>
    paste(collapse = "+")
  if (nchar(c) == 0) {
    f <- stats::as.formula(s)
  } else {
    f <- stats::as.formula(paste(s, c, sep = "+"))
  }
  return(f)
}

.emax_design <- function(structural_model, covariate_model, data) {

  ff <- .emax_flat_formula(structural_model, covariate_model)
  mm <- stats::model.matrix(ff, data)

  preds <- all.vars(ff)        # variables required, including response
  terms <- colnames(mm)[-1]    # terms in the model, dropping intercept
  terms <- c(preds[1], terms)  # but keep the response
  terms <- stringr::str_remove_all(terms, " ")
  index <- attr(mm, "assign")[-1]
  index <- c(1, index + 1)     # index into preds

  lookup <- tibble::tibble(variable = preds[index], term = terms)
  design <- mm |> tibble::as_tibble() |> stats::setNames(terms)
  design[[preds[1]]] <- data[[preds[1]]]

  return(list(lookup = lookup, design = design))
}

utils::globalVariables(c(
  "label",
  "Estimate",
  "Std. Error",
  "t value",
  "Pr(>|t|)",
  "response_1",
  "response_2", 
  "exposure_1",  
  "exposure_2",
  "bin_pred",
  "bin_prob",
  "cnt_a",
  "cnt_b",
  "cnt_c",
  "bin_d",
  "bin_e",
  "term",
  "vcov",
  "coef",
  "mu",
  "residuals",
  "variable",
  "formula",
  "algorithm",
  "control",
  "design", 
  "model", 
  "predict_args"
))