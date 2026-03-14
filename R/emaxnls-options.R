.emax_nls_options <- function(optim_method, 
                              optim_control, 
                              quiet, 
                              weights, 
                              na.action) {
  #dots <- list(...)
  #if (!is.null(init))
  #if (is.null(init))
  #.validate_settings_args(init, algorithm, control, dots)
  
  # temporary: supply better defaults later
  if (is.null(optim_control)) {
    optim_control <- list(
      tol = 1e-8,
      minFactor = 1024^-4,
      maxiter = 200000,
      scaleOffset = 1,
      warnOnly = FALSE
    )
  }
  opts <- list(
    optim_method = optim_method,
    optim_control = optim_control,
    quiet = quiet,
    weights = weights,
    na.action = na.action
  )
  return(opts)
}