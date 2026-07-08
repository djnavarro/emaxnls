
# test fixtures ------------------------------------------------------------

mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model  = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df
)

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model  = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df
)

# expected column layout for both model types: the four simulation columns,
# then the parameter draw columns, then the original data columns
sim_cols_c <- c(
  "dat_id", "sim_id", "mu", "val",
  "E0_cnt_a", "E0_Intercept", "Emax_Intercept", "logEC50_Intercept",
  "rsp_1", "exp_1", "cnt_a"
)

sim_cols_b <- c(
  "dat_id", "sim_id", "mu", "val",
  "E0_cnt_a", "E0_Intercept", "Emax_Intercept", "logEC50_Intercept",
  "rsp_2", "exp_1", "cnt_a"
)

n_obs <- nrow(emax_df)


# simulate() – emaxnls -----------------------------------------------------

test_that("simulate() returns a data frame with correct structure for emaxnls", {
  skip_if(!.is_converged(mod_c), "Skip if convergence fails on this architecture")

  sim <- simulate(mod_c, nsim = 3, seed = 1)

  expect_s3_class(sim, "data.frame")
  expect_named(sim, sim_cols_c)
  expect_equal(nrow(sim), 3L * n_obs)
  expect_equal(sort(unique(sim$sim_id)), 1:3)
})

test_that("simulate() dat_id indexes into the original data for emaxnls", {
  skip_if(!.is_converged(mod_c), "Skip if convergence fails on this architecture")

  sim <- simulate(mod_c, nsim = 1, seed = 1)

  expect_equal(sort(unique(sim$dat_id)), 1:n_obs)
})

test_that("simulate() is reproducible given the same seed for emaxnls", {
  skip_if(!.is_converged(mod_c), "Skip if convergence fails on this architecture")

  sim1 <- simulate(mod_c, nsim = 2, seed = 42)
  sim2 <- simulate(mod_c, nsim = 2, seed = 42)

  expect_equal(sim1, sim2)
})


# simulate() – emaxlogistic ------------------------------------------------

test_that("simulate() returns a data frame with correct structure for emaxlogistic", {
  skip_if(!.is_converged(mod_b), "Skip if convergence fails on this architecture")

  sim <- simulate(mod_b, nsim = 3, seed = 1)

  expect_s3_class(sim, "data.frame")
  expect_named(sim, sim_cols_b)
  expect_equal(nrow(sim), 3L * n_obs)
  expect_equal(sort(unique(sim$sim_id)), 1:3)
})

test_that("simulate() mu column is on probability scale for emaxlogistic", {
  skip_if(!.is_converged(mod_b), "Skip if convergence fails on this architecture")

  sim <- simulate(mod_b, nsim = 2, seed = 1)

  expect_true(all(sim$mu > 0 & sim$mu < 1))
})

test_that("simulate() val column is binary for emaxlogistic", {
  skip_if(!.is_converged(mod_b), "Skip if convergence fails on this architecture")

  sim <- simulate(mod_b, nsim = 2, seed = 1)

  expect_true(all(sim$val %in% c(0, 1)))
})

test_that("simulate() is reproducible given the same seed for emaxlogistic", {
  skip_if(!.is_converged(mod_b), "Skip if convergence fails on this architecture")

  sim1 <- simulate(mod_b, nsim = 2, seed = 42)
  sim2 <- simulate(mod_b, nsim = 2, seed = 42)

  expect_equal(sim1, sim2)
})


# emax_fun() – emaxnls -----------------------------------------------------

test_that("emax_fun() returns a function with correct formals for emaxnls", {
  skip_if(!.is_converged(mod_c), "Skip if convergence fails on this architecture")

  fn <- emax_fun(mod_c)

  expect_type(fn, "closure")
  expect_equal(formals(fn), pairlist(param = NULL, data = NULL))
})

test_that("emax_fun() default output matches predict() for emaxnls", {
  skip_if(!.is_converged(mod_c), "Skip if convergence fails on this architecture")

  fn  <- emax_fun(mod_c)
  out <- as.numeric(fn())
  ref <- as.numeric(predict(mod_c))

  expect_equal(out, ref)
})

test_that("emax_fun() respects custom param for emaxnls", {
  skip_if(!.is_converged(mod_c), "Skip if convergence fails on this architecture")

  fn   <- emax_fun(mod_c)
  pp   <- coef(mod_c)
  e0   <- pp["E0_Intercept"]
  pp["E0_Intercept"] <- 0.0

  out1 <- fn(data = emax_df[120L:125L, ])
  out2 <- fn(data = emax_df[120L:125L, ], param = pp)

  expect_length(out1, 6L)
  expect_length(out2, 6L)
  expect_equal(as.numeric(out1), as.numeric(out2 + e0))
})

test_that("emax_fun() respects custom data for emaxnls", {
  skip_if(!.is_converged(mod_c), "Skip if convergence fails on this architecture")

  fn  <- emax_fun(mod_c)
  out <- fn(data = emax_df[1L:10L, ])

  expect_length(out, 10L)
  expect_true(is.numeric(out))
})


# emax_fun() – emaxlogistic ------------------------------------------------

test_that("emax_fun() returns a function with correct formals for emaxlogistic", {
  skip_if(!.is_converged(mod_b), "Skip if convergence fails on this architecture")

  fn <- emax_fun(mod_b)

  expect_type(fn, "closure")
  expect_equal(formals(fn), pairlist(param = NULL, data = NULL))
})

test_that("emax_fun() output is on the probability scale for emaxlogistic", {
  skip_if(!.is_converged(mod_b), "Skip if convergence fails on this architecture")

  fn  <- emax_fun(mod_b)
  out <- fn()

  expect_true(all(out > 0 & out < 1))
})

test_that("emax_fun() default output matches fitted(type='response') for emaxlogistic", {
  skip_if(!.is_converged(mod_b), "Skip if convergence fails on this architecture")

  fn  <- emax_fun(mod_b)
  out <- as.numeric(fn())
  ref <- as.numeric(fitted(mod_b, type = "response"))

  expect_equal(out, ref)
})

test_that("emax_fun() output is not on the logit scale for emaxlogistic", {
  skip_if(!.is_converged(mod_b), "Skip if convergence fails on this architecture")

  fn       <- emax_fun(mod_b)
  out      <- as.numeric(fn())
  logit_ref <- as.numeric(fitted(mod_b, type = "link"))

  expect_false(isTRUE(all.equal(out, logit_ref)))
})

test_that("emax_fun() respects custom param for emaxlogistic", {
  skip_if(!.is_converged(mod_b), "Skip if convergence fails on this architecture")

  fn <- emax_fun(mod_b)
  pp <- coef(mod_b)

  # shifting E0 far upward drives all probabilities toward 1
  pp_high <- pp
  pp_high["E0_Intercept"] <- pp["E0_Intercept"] + 100

  out_default <- fn()
  out_shifted <- fn(param = pp_high)

  expect_true(all(out_shifted > out_default))
})

test_that("emax_fun() respects custom data for emaxlogistic", {
  skip_if(!.is_converged(mod_b), "Skip if convergence fails on this architecture")

  fn  <- emax_fun(mod_b)
  out <- fn(data = emax_df[1L:10L, ])

  expect_length(out, 10L)
  expect_true(all(out > 0 & out < 1))
})
