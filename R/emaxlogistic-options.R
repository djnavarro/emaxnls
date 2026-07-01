
.emax_logistic_options <- function(optim_method,
                                   optim_control,
                                   quiet,
                                   na.action,
                                   max_iter,
                                   tol) {

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
    weights = NULL,   # always NULL for logistic; computed internally by IRLS
    na.action = na.action,
    max_iter = max_iter,
    tol = tol
  )
  return(opts)
}
