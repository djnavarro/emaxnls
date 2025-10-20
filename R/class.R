
.emax_nls <- function(structural_model,
                     covariate_model,
                     data,
                     quiet = FALSE) {

  .validate_structural_formula(structural_model, names(data))
  .validate_covariate_formula(covariate_model, names(data))

  model_type <- .get_model_type(covariate_model)

  names(covariate_model) <- covariate_model |>
    purrr::map_chr(\(x) all.vars(x[[2]]))

  tmp <- .emax_design(structural_model, covariate_model, data)
  lookup <- tmp$lookup
  design <- tmp$design

  variables <- list(
    response = as.character(structural_model[[2]]),
    exposure = as.character(structural_model[[3]]),
    E0 = as.character(all.vars(covariate_model[["E0"]][[3]])),
    Emax = as.character(all.vars(covariate_model[["Emax"]][[3]])),
    logEC50 = as.character(all.vars(covariate_model[["logEC50"]][[3]]))
  )
  if (model_type == "sigmoidal") {
    variables$logHill <- as.character(all.vars(covariate_model[["logHill"]][[3]]))
  }

  terms <- variables |>
    purrr::map(
      \(x) lookup |>
        dplyr::filter(variable %in% x) |>
        dplyr::pull(term)
    )

  if (model_type == "sigmoidal") {
    coefficients <- list(
      E0 = terms$E0,
      Emax = terms$Emax,
      logEC50 = terms$logEC50,
      logHill = terms$logHill
    )
  }
  if (model_type == "hyperbolic") {
    coefficients <- list(
      E0 = terms$E0,
      Emax = terms$Emax,
      logEC50 = terms$logEC50
    )
  }
  coefficients <- coefficients |>
    purrr::map(\(x) c("Intercept", x)) |>
    purrr::imap(\(x, l) paste(l, x, sep = "_"))

  .make_cov_terms <- function(terms, coefficients) {
    s <- paste(c("1", terms), coefficients, sep = " * ", collapse = " + ")
    stringr::str_remove_all(s, stringr::fixed("1 * "))
  }

  covariates <- list(
    E0      = .make_cov_terms(terms$E0, coefficients$E0),
    Emax    = .make_cov_terms(terms$Emax, coefficients$Emax),
    logEC50 = .make_cov_terms(terms$logEC50, coefficients$logEC50)
  )
  if (model_type == "sigmoidal") {
    covariates <- c(
      covariates,
      logHill = .make_cov_terms(terms$logHill, coefficients$logHill)
    )
  }

  if (model_type == "hyperbolic") {
    formula <- stats::as.formula(paste0(
      variables$response, " ~ (", covariates$E0, ") + ", variables$exposure,
      " * (", covariates$Emax, ") / (", variables$exposure,
      " + exp(", covariates$logEC50, "))"
    ))
  }
  if (model_type == "sigmoidal") {
    formula <- stats::as.formula(paste0(
      variables$response,
      " ~ (", covariates$E0, ") + ",
      variables$exposure, "^ exp(", covariates$logHill, ")",
      " * (", covariates$Emax, ") / (",
      variables$exposure, "^ exp(", covariates$logHill, ")",
      " + exp(", covariates$logEC50, ")", "^ exp(", covariates$logHill, ")",
      ")"
    ))
  }

  coefficients <- unname(unlist(coefficients))
  coefficient_settings <- .get_coefficient_settings(coefficients)

  start <- coefficient_settings$start
  lower <- coefficient_settings$lower
  upper <- coefficient_settings$upper
  names(start) <- coefficients
  names(lower) <- coefficients
  names(upper) <- coefficients

  # bundle all the setup into a list
  obj <- list(

    # input args
    structural_model = structural_model,
    covariate_model = covariate_model,
    data = data,

    # model specification
    formula = formula,
    model_type = model_type,
    variables = variables,
    coefficients = coefficients,
    design = design,
    lookup = lookup,

    # nls settings: for future development, these should
    # be user-customisable values
    start = start,
    lower = lower,
    upper = upper,
    algorithm = "port",
    control = list(
      tol = 1e-8,
      minFactor = 1024^-4,
      maxiter = 200000,
      scaleOffset = 1,
      warnOnly = FALSE
    )
  )

  # call nls() safely
  out <- .nls_safe(
    formula   = obj$formula,
    data      = obj$design,
    start     = obj$start,
    algorithm = obj$algorithm,
    control   = obj$control,
    lower     = obj$lower,
    upper     = obj$upper
  )

  # warn if convergence fails
  if (!is.null(out$error) & !quiet) {
    rlang::warn("`nls()` did not converge", class = "emaxnls_warning")
  }

  # append to object and return
  obj$error  <- out$error
  obj$result <- out$result

  return(structure(obj, class = "emaxnls"))
}
