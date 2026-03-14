

# resampling code ---------------------------------------------------------

# safely construct a prediction function for emax model mod, with
# user-customisable parameter values; function is evaluated in 
# cloned environment to prevent modification to the original model
.emax_fn <- function(mod, data = mod$data) {
  function(data = NULL, param = NULL) {
    if(is.null(data)) data <- mod$data
    if(is.null(param)) param <- coef(mod)
    old_env <- .get_nls(mod)$m$getEnv()
    new_env <- rlang::env_clone(env = old_env)
    purrr::iwalk(data, function(x, lbl) assign(lbl, x, envir = new_env))
    purrr::iwalk(param, function(x, lbl) assign(lbl, x, envir = new_env))
    eval(mod$formula$nls[[3]], envir = new_env)
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

  .f <- .emax_fn(mod)

  sim <- list()
  for (ss in 1L:nsim) {
    sim[[ss]] <- tibble::tibble(
      dat_id = 1L:nr,
      sim_id = ss,
      mu = .f(par[ss,]),
      val = mu + stats::rnorm(nr, 0, sd = sig)
    )
  }
  sim <- do.call(rbind, sim)
  par <- tibble::as_tibble(par)
  par$sim_id <- 1L:nsim

  out <- .left_join(sim, par, by = "sim_id") 
  out <- .left_join(out, dat, by = "dat_id")

  return(out)
}
