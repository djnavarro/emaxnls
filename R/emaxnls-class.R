
.emax_nls <- function(structural_model,
                      covariate_model,
                      data,
                      quiet = FALSE) {

  .validate_structural_formula(structural_model, names(data))
  .validate_covariate_formula(covariate_model, names(data))

  # compute and store everything in temporary object
  store <- .store(covariate_model, structural_model, data) 
  
  # hard code this for now
  settings <- list(
    algorithm = "port",
    control = list(
      tol = 1e-8,
      minFactor = 1024^-4,
      maxiter = 200000,
      scaleOffset = 1,
      warnOnly = FALSE
    )
  )

  ini <- .guess_init(store)

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
      init = ini
    ),
    env = rlang::new_environment(
      data = list(
        formula   = store$nls_formula, 
        design    = store$design, 
        lookup    = store$lookup,
        start     = ini$start, 
        control   = settings$control, 
        algorithm = settings$algorithm, 
        lower     = ini$lower, 
        upper     = ini$upper
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

