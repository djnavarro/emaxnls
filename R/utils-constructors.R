
# internal constructors used to build up emaxnls object constituents ------

.store <- function(covariate_model, structural_model, data) {
  store <- .store_inputs(covariate_model, structural_model, data)   
  store <- .store_modeltype(store)
  store <- .store_design(store)
  store <- .store_variables(store)
  store <- .store_terms(store)
  store <- .store_coefficients(store) 
  store <- .store_covariates(store)
  store <- .store_nlsformula(store)
}

.store_inputs <- function(covariate_model, structural_model, data) {
  names(covariate_model) <- purrr::map_chr(
    .x = covariate_model, 
    .f = function(x) all.vars(x[[2]])
  )
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
  cc <- unlist(purrr::map(
    .x = store$covariate_model,
    .f = function(x) all.vars(x[[3]])
  ))
  cc <- paste(cc, collapse = "+")
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
  design <- stats::setNames(tibble::as_tibble(mm), terms)
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
  store$terms <- purrr::map(
    .x = store$variables,
    .f = function(vv) {
      dd <- store$lookup
      ll <- .filter(dd, variable %in% vv)
      ll$term
    }
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
  cc <- purrr::map(
    .x = coefficients,
    .f = function(x) c("Intercept", x)
  )
  cc <- purrr::imap(
    .x = cc,
    .f = function(x, l) paste(l, x, sep = "_")
  )
  store$coefficients <- cc
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

