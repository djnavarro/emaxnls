.emax_nls_options <- function(optim_method, 
                              optim_control, 
                              quiet, 
                              weights, 
                              na.action,
                              max_time) {
  
  .validate_optim_method(optim_method)
  .validate_max_time(max_time)

  if (is.null(optim_control)) {
    if (optim_method == "gauss") optim_control <- stats::nls.control()  
    if (optim_method == "port") optim_control <- stats::nls.control()
    if (optim_method == "levenberg") optim_control <- minpack.lm::nls.lm.control()
  }

  # Resolve na.action to a function: accept a character name or a function
  if (is.character(na.action)) na.action <- match.fun(na.action)

  opts <- list(
    optim_method = optim_method,
    optim_control = optim_control,
    quiet = quiet,
    weights = weights,
    na.action = na.action,
    max_time = max_time
  )
  return(opts)
}