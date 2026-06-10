.emax_nls_options <- function(optim_method, 
                              optim_control, 
                              quiet, 
                              weights, 
                              na.action) {
  
  .validate_optim_method(optim_method)
  
  if (is.null(optim_control)) {
    if (optim_method == "gauss") optim_control <- stats::nls.control()  
    if (optim_method == "port") optim_control <- stats::nls.control()
    if (optim_method == "levenberg") optim_control <- minpack.lm::nls.lm.control()
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