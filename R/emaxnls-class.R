
.emax_nls <- function(structural_model,
                      covariate_model,
                      data,
                      quiet = FALSE) {

  .validate_structural_formula(structural_model, names(data))
  .validate_covariate_formula(covariate_model, names(data))

  store <- .store_inputs(covariate_model, structural_model, data)   
  store <- .store_modeltype(store)
  store <- .store_design(store)
  store <- .store_variables(store)
  store <- .store_terms(store)
  store <- .store_coefficients(store) 
  store <- .store_covariates(store)
  store <- .store_nlsformula(store)
  
  settings <- .get_settings(store$coefficients)

  obj <- list(
    formula = list(
      structural = store$structural_model,
      covariate  = store$covariate_model,
      nls        = store$nls_formula 
    ),      
    data = store$data,
    info = list(
      model_type   = store$model_type,
      variables    = store$variables,
      coefficients = unname(unlist(store$coefficients))
    ),
    env = rlang::new_environment(
      data = list(
        formula   = store$nls_formula, 
        design    = store$design, 
        lookup    = store$lookup,
        start     = settings$coefficient$start, 
        algorithm = settings$algorithm, 
        control   = settings$control, 
        lower     = settings$coefficient$lower, 
        upper     = settings$coefficient$upper
      ), 
      parent = parent.frame()
    )
  )  

  tmp <- evalq(
    .nls_safe(
      formula   = formula,
      data      = design,
      start     = start,
      algorithm = algorithm,
      control   = control,
      lower     = lower,
      upper     = upper
    ),
    envir = obj$env
  )
  obj$env$model <- tmp$result 
  obj$env$error <- tmp$error

  if (!is.null(obj$env$error) & !quiet) {
    rlang::warn("`nls()` did not converge", class = "emaxnls_warning")
  }

  return(structure(obj, class = "emaxnls"))
}

