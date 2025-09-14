

# resampling code ---------------------------------------------------------

# safely construct a prediction function for emax model mod, with
# user-customisable parameter values
emax_fn <- function(mod, data = mod$data) {
  function(param = NULL) {
    if(is.null(param)) {
      param <- coef(mod)$estimate
      names(param) <- coef(mod)$label
    }
    old_env <- mod$result$m$getEnv()
    new_env <- rlang::env_clone(env = old_env)
    eval(mod$formula[[3]], envir = new_env)
  }
}

emax_resample <- function(mod, n = 100, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  cov <- vcov(mod)
  est <- coef(mod)$estimate
  lbl <- coef(mod)$label
  sig <- summary(mod$result)$sigma
  nr <- nrow(mod$data)

  var <- unique(unlist(mod$variables))
  dat <- mod$data[,var]
  dat$dat_id <- 1L:nr

  par <- mvtnorm::rmvnorm(n, mean = est, sigma = cov)
  colnames(par) <- lbl

  f <- emax_fn(mod)

  sim <- list()
  for (ss in 1L:n) {
    sim[[ss]] <- tibble::tibble(
      dat_id = 1L:nr,
      sim_id = ss,
      mu = f(par[ss,]),
      val = mu + rnorm(nr, 0, sd = sig)
    )
  }
  sim <- dplyr::bind_rows(sim)
  par <- tibble::as_tibble(par)
  par$sim_id <- 1L:n

  out <- sim |>
    dplyr::left_join(par, by = "sim_id") |>
    dplyr::left_join(dat, by = "dat_id")

  return(out)
}
