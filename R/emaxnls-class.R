
.emax_nls <- function(structural_model,
                      covariate_model,
                      data,
                      settings, 
                      quiet) {

  .validate_structural_formula(structural_model, names(data))
  .validate_covariate_formula(covariate_model, names(data))

  # compute and store everything in temporary object
  store <- .store(covariate_model, structural_model, data) 
  
  if (is.null(settings$init)) settings$init <- .guess_init(store)

  # organise into emaxnls structure
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
      coefficients = unname(unlist(store$coefficients)),
      settings = settings
    ),
    env = rlang::new_environment(
      data = list(
        formula   = store$nls_formula, 
        design    = store$design, 
        lookup    = store$lookup,
        start     = settings$init$start, 
        control   = settings$control, 
        algorithm = settings$algorithm, 
        lower     = settings$init$lower, 
        upper     = settings$init$upper
      ), 
      parent = parent.frame()
    )
  )  

  # estimate the nls model
  tmp <- evalq(
    .nls_safe(
      formula   = formula,
      data      = design,
      start     = start,
      control   = control,
      algorithm = algorithm,
      lower     = lower,
      upper     = upper
    ),
    envir = obj$env
  )
  obj$env$model <- tmp$result 
  obj$env$error <- tmp$error

  # message user if needed and return
  if (!is.null(obj$env$error) & !quiet) {
    rlang::warn("`nls()` did not converge", class = "emaxnls_warning")
  }
  return(structure(obj, class = "emaxnls"))
}

