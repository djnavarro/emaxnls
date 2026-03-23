

# resampling code ---------------------------------------------------------

# safely construct a prediction function for emax model mod, with
# user-customisable parameter values; function is evaluated in 
# cloned environment to prevent modification to the original model.
# expected use case is for users to vary the parameters, so data
# is the second argument not the first. (the data argument to 
# .emax_fun is not user facing, i.e., not in the api version: 
# possibly remove in future, or alternatively have param in the
# internal version if that turns out to be useful)
.emax_fun <- function(mod, data = mod$data) {
  .assert(.is_emaxnls(mod), "`mod` must be an emaxnls object")
  .assert(inherits(data, "data.frame"), "`data` must be a data frame")
  function(param = NULL, data = NULL) {
    .validate_emax_fun_args(data, param, mod)
    if(is.null(data)) data <- mod$data
    if(is.null(param)) param <- coef(mod)
    old_env <- .get_nls(mod)$m$getEnv()
    new_env <- rlang::env_clone(env = old_env)
    .iwalk(data, function(x, lbl) assign(lbl, x, envir = new_env))
    .iwalk(param, function(x, lbl) assign(lbl, x, envir = new_env))
    eval(.get_nls_formula(mod)[[3]], envir = new_env)
  }
}

.emax_resample <- function(mod, nsim, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  cov <- vcov(mod)
  est <- coef(mod)
  lbl <- names(coef(mod))
  sig <- summary(.get_nls(mod))$sigma
  nr <- nrow(mod$data)

  var <- unique(unlist(mod$variables))
  dat <- mod$data[,var]
  dat$dat_id <- 1L:nr

  par <- mvtnorm::rmvnorm(nsim, mean = est, sigma = cov)
  colnames(par) <- lbl

  .f <- .emax_fun(mod)

  sim <- list()
  for (ss in 1L:nsim) {
    sim[[ss]] <- .tibble(
      dat_id = 1L:nr,
      sim_id = ss,
      mu = .f(param = par[ss,]),
      val = mu + stats::rnorm(nr, 0, sd = sig)
    )
  }
  sim <- do.call(rbind, sim)
  par <- .as_tibble(par)
  par$sim_id <- 1L:nsim

  out <- .left_join(sim, par, by = "sim_id") 
  out <- .left_join(out, dat, by = "dat_id")
  out <- .as_tibble(out)

  return(out)
}
