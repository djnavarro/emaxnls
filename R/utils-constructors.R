
# internal constructors for emaxnls object constituents ------

.store_inputs <- function(covariate_model, structural_model, data) {
  names(covariate_model) <- covariate_model |> purrr::map_chr(\(x) all.vars(x[[2]]))
  store <- list(
    structural_model = structural_model,
    covariate_model = covariate_model, 
    data = data
  )
  return(store)
}
  
.store_modeltype <- function(store) {
  if (.is_hyperbolic(store$covariate_model)) {
    store$model_type <- "hyperbolic"
    return(store)
  }
  if (.is_sigmoidal(store$covariate_model)) {
    store$model_type <- "sigmoidal"
    return(store)
  }
  stop("invalid covariate model", call. = FALSE)
}

.store_design <- function(store) {

  # construct flat formula 
  ss <- deparse(store$structural_model)
  cc <- store$covariate_model |>
    purrr::map(\(x) all.vars(x[[3]])) |>
    unlist() |>
    paste(collapse = "+")
  if (nchar(cc) == 0) {
    ff <- stats::as.formula(ss)
  } else {
    ff <- stats::as.formula(paste(ss, cc, sep = "+"))
  }

  # model matrix
  mm <- stats::model.matrix(ff, store$data)

  preds <- all.vars(ff)        # variables required, including response
  terms <- colnames(mm)[-1]    # terms in the model, dropping intercept
  terms <- c(preds[1], terms)  # but keep the response
  terms <- gsub(" ", "", terms)
  index <- attr(mm, "assign")[-1]
  index <- c(1, index + 1)     # index into preds

  lookup <- tibble::tibble(variable = preds[index], term = terms)
  design <- mm |> tibble::as_tibble() |> stats::setNames(terms)
  design[[preds[1]]] <- store$data[[preds[1]]]

  store$lookup <- lookup
  store$design <- design
     
  return(store)
}

.store_variables <- function(store) {
  variables <- list(
    response = as.character(store$structural_model[[2]]),
    exposure = as.character(store$structural_model[[3]]),
    E0       = as.character(all.vars(store$covariate_model[["E0"]][[3]])),
    Emax     = as.character(all.vars(store$covariate_model[["Emax"]][[3]])),
    logEC50  = as.character(all.vars(store$covariate_model[["logEC50"]][[3]]))
  )
  if (store$model_type == "sigmoidal") {
    variables$logHill <- as.character(all.vars(store$covariate_model[["logHill"]][[3]]))
  }

  store$variables <- variables
  return(store)
}

.store_terms <- function(store) {
  store$terms <- store$variables |>
    purrr::map(
      \(x) store$lookup |>
        dplyr::filter(variable %in% x) |>
        dplyr::pull(term)
    )
  
  return(store)
}

.store_coefficients <- function(store) {

  if (store$model_type == "sigmoidal") {
    coefficients <- list(
      E0      = store$terms$E0,
      Emax    = store$terms$Emax,
      logEC50 = store$terms$logEC50,
      logHill = store$terms$logHill
    )
  }
  if (store$model_type == "hyperbolic") {
    coefficients <- list(
      E0      = store$terms$E0,
      Emax    = store$terms$Emax,
      logEC50 = store$terms$logEC50
    )
  }
  store$coefficients <- coefficients |>
    purrr::map(\(x) c("Intercept", x)) |>
    purrr::imap(\(x, l) paste(l, x, sep = "_"))

  return(store)
}

.make_cov_terms <- function(tt, cc) {
  ss <- paste(c("1", tt), cc, sep = " * ", collapse = " + ")
  ss <- gsub("1 * ", "", ss, fixed = TRUE)
  ss
}

.store_covariates <- function(store) {
  covariates <- list(
    E0      = .make_cov_terms(store$terms$E0, store$coefficients$E0),
    Emax    = .make_cov_terms(store$terms$Emax, store$coefficients$Emax),
    logEC50 = .make_cov_terms(store$terms$logEC50, store$coefficients$logEC50)
  )
  if (store$model_type == "sigmoidal") {
    covariates <- c(
      covariates,
      logHill = .make_cov_terms(store$terms$logHill, store$coefficients$logHill)
    )
  }
  store$covariates <- covariates
  return(store)
}

.store_nlsformula <- function(store) {

  if (store$model_type == "hyperbolic") {
    nls_formula <- stats::as.formula(paste0(
      store$variables$response, " ~ (", store$covariates$E0, ") + ", store$variables$exposure,
      " * (", store$covariates$Emax, ") / (", store$variables$exposure,
      " + exp(", store$covariates$logEC50, "))"
    ))
  }
  if (store$model_type == "sigmoidal") {
    nls_formula <- stats::as.formula(paste0(
      store$variables$response,
      " ~ (", store$covariates$E0, ") + ",
      store$variables$exposure, "^ exp(", store$covariates$logHill, ")",
      " * (", store$covariates$Emax, ") / (",
      store$variables$exposure, "^ exp(", store$covariates$logHill, ")",
      " + exp(", store$covariates$logEC50, ")", "^ exp(", store$covariates$logHill, ")",
      ")"
    ))
  }

  store$nls_formula <- nls_formula
  return(store)
}

# settings are hard-coded for the moment ------

.get_settings <- function(coefficients) {

  coefficient_vec <- unname(unlist(coefficients))

  coefficient_table <- tibble::tibble(
    parameter = gsub("_.*$", "", coefficient_vec),
    covariate = gsub("^[^_]*_", "", coefficient_vec)
  )

  coefficient_settings <- coefficient_table |>
    dplyr::mutate(
      start = dplyr::case_when(
        covariate != "Intercept" ~ 0,
        covariate == "Intercept" & parameter == "E0"      ~ -2,
        covariate == "Intercept" & parameter == "Emax"    ~ -1,
        covariate == "Intercept" & parameter == "logEC50" ~ 5,
        covariate == "Intercept" & parameter == "logHill" ~ 0,
      ),
      lower = dplyr::case_when(
        parameter == "E0"      ~ -10,
        parameter == "Emax"    ~ -10,
        covariate == "Intercept" & parameter == "logEC50" ~ 1,
        covariate != "Intercept" & parameter == "logEC50" ~ 0,
        parameter == "logHill" ~ -2
      ),
      upper = dplyr::case_when(
        parameter == "E0"      ~ 20,
        parameter == "Emax"    ~ 20,
        parameter == "logEC50" ~ 10,
        parameter == "logHill" ~ 4
      )
    )
  
  names(coefficient_settings$start) <- coefficient_vec
  names(coefficient_settings$lower) <- coefficient_vec
  names(coefficient_settings$upper) <- coefficient_vec
  
  settings <- list(
    coefficient = coefficient_settings,
    algorithm = "port",
    control = list(
      tol = 1e-8,
      minFactor = 1024^-4,
      maxiter = 200000,
      scaleOffset = 1,
      warnOnly = FALSE
    )
  )

  return(settings)
}

