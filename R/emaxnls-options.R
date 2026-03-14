.emax_nls_options <- function(optim_method, 
                              optim_control, 
                              quiet, 
                              weights, 
                              na.action) {
  
  .validate_optim_method(optim_method)
  
  if (is.null(optim_control)) {

    if (optim_method == "gauss") {
      optim_control <- list(
        tol = 1e-8,
        minFactor = 1024^-4,
        maxiter = 200000,
        scaleOffset = 1,
        warnOnly = FALSE
      )
    }
   
    if (optim_method == "port") {
      optim_control <- list(
        tol = 1e-8,
        minFactor = 1024^-4,
        maxiter = 200000,
        scaleOffset = 1,
        warnOnly = FALSE
      )
    }

     if (optim_method == "levenberg") {
      optim_control <- list(
        tol = 1e-8,
        minFactor = 1024^-4,
        maxiter = 1024,
        scaleOffset = 1,
        warnOnly = FALSE
      )
    }
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