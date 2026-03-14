
.emax_nls <- function(structural_model,
                      covariate_model,
                      data,
                      init,
                      opts) {

  .validate_structural_formula(structural_model, names(data))
  .validate_covariate_formula(covariate_model, names(data))

  store <- .store(covariate_model, structural_model, data)
  
  if (is.null(opts)) opts <- emax_nls_options()
  if (is.null(init)) init <- .guess_init(store)

  # organise into emaxnls structure
  obj <- list(
    formula = list(
      structural = store$structural_model,
      covariate  = store$covariate_model,
      nls        = store$nls_formula 
    ),      
    data = store$data,
    opts = opts,
    init = init,
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
        start     = init$start, 
        control   = opts$optim_control, 
        algorithm = .nls_method(opts$optim_method), 
        lower     = init$lower,
        upper     = init$upper
      ), 
      parent = parent.frame()
    )
  )  

  # estimate the nls model
  tmp <- evalq(
    .nls_call(
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
  if (!is.null(obj$env$error) & !opts$quiet) {
    rlang::warn("`nls()` did not converge", class = "emaxnls_warning")
  }
  return(structure(obj, class = "emaxnls"))
}

.nls_call <- function(formula, data, start, control, algorithm, lower, upper) {
  if (algorithm %in% c("default", "plinear")) {
    return(.nls_safe(
      formula = formula,
      data = data,
      start = start,
      control = control,
      algorithm = algorithm
    ))
  }
  if (algorithm == "port") {
    return(.nls_safe(
      formula = formula,
      data = data,
      start = start,
      control = control,
      algorithm = algorithm,
      lower = lower,
      upper = upper
    ))
  }
  if (algorithm == "LM") {
    .validate_optim_method("levenberg")
    return(.nls_lm_safe(
      formula = formula,
      data = data,
      start = start,
      control = control,
      algorithm = algorithm
    ))
  } 
}

.nls_method <- function(optim_method) {
  if (optim_method == "gauss") return("default")
  if (optim_method == "port") return("port")
  if (optim_method == "levenberg") return("LM")
  .abort("optim_method must be one of 'gauss', 'port', or 'levenberg'")
}
