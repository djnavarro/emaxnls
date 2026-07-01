
# test-parameter-recovery.R
#
# Tests that emax_nls() and emax_logistic() recover true parameters from
# data generated under a known model. Four scenarios are covered:
#
#   1. Continuous hyperbolic Emax model (E0, Emax, logEC50)
#   2. Continuous sigmoidal Emax model  (E0, Emax, logEC50, logHill)
#   3. Binary logistic Emax model       (E0, Emax, logEC50)
#   4. Continuous model with covariate on E0 (E0 intercept + slope, Emax, logEC50)
#
# Each scenario generates a fixed dataset (via set.seed), fits the model,
# and checks that point estimates are within a small relative tolerance of
# the true generating values.  Platform skips mirror those in other test
# files: clang and gcc15 toolchains can produce non-convergence that is
# unrelated to the package code itself.

# -------------------------------------------------------------------------
# 1. Continuous hyperbolic Emax model
# -------------------------------------------------------------------------

true_E0_cont      <- 1.0
true_Emax_cont    <- 5.0
true_EC50_cont    <- 100.0
true_logEC50_cont <- log(true_EC50_cont)  # ~4.605

set.seed(8241)
doses_cont <- rep(c(0, 10, 30, 100, 300, 1000), each = 80L)
mu_cont    <- true_E0_cont + true_Emax_cont * doses_cont / (doses_cont + true_EC50_cont)
sim_cont   <- data.frame(
  dose = doses_cont,
  rsp  = mu_cont + stats::rnorm(length(doses_cont), mean = 0, sd = 0.2)
)

mod_cont <- emax_nls(
  structural_model = rsp ~ dose,
  covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
  data             = sim_cont
)

test_that("hyperbolic emax_nls converges on simulated data", {
  skip_if(is_clang(), "Fails on clang due to toolchain issue unrelated to emaxnls code")
  skip_if(is_gcc15(), "Fails on gcc15 due to toolchain issue unrelated to emaxnls code")
  expect_true(emax_converged(mod_cont))
})

test_that("hyperbolic emax_nls recovers true parameters from simulated data", {
  skip_if(is_clang(), "Fails on clang due to toolchain issue unrelated to emaxnls code")
  skip_if(is_gcc15(), "Fails on gcc15 due to toolchain issue unrelated to emaxnls code")
  skip_if(!emax_converged(mod_cont))
  est <- coef(mod_cont)
  expect_equal(unname(est["E0_Intercept"]),      true_E0_cont,      tolerance = 0.05)
  expect_equal(unname(est["Emax_Intercept"]),    true_Emax_cont,    tolerance = 0.05)
  expect_equal(unname(est["logEC50_Intercept"]), true_logEC50_cont, tolerance = 0.05)
})

# -------------------------------------------------------------------------
# 2. Continuous sigmoidal Emax model
# -------------------------------------------------------------------------

true_E0_sig      <- 1.0
true_Emax_sig    <- 5.0
true_EC50_sig    <- 100.0
true_logEC50_sig <- log(true_EC50_sig)
true_Hill_sig    <- 2.0
true_logHill_sig <- log(true_Hill_sig)  # ~0.693

set.seed(3714)
doses_sig <- rep(c(0, 10, 30, 100, 300, 1000), each = 80L)
mu_sig    <- true_E0_sig +
  true_Emax_sig * doses_sig^true_Hill_sig /
  (doses_sig^true_Hill_sig + true_EC50_sig^true_Hill_sig)
sim_sig   <- data.frame(
  dose = doses_sig,
  rsp  = mu_sig + stats::rnorm(length(doses_sig), mean = 0, sd = 0.2)
)

mod_sig <- emax_nls(
  structural_model = rsp ~ dose,
  covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1, logHill ~ 1),
  data             = sim_sig
)

test_that("sigmoidal emax_nls converges on simulated data", {
  skip_if(is_clang(), "Fails on clang due to toolchain issue unrelated to emaxnls code")
  skip_if(is_gcc15(), "Fails on gcc15 due to toolchain issue unrelated to emaxnls code")
  expect_true(emax_converged(mod_sig))
})

test_that("sigmoidal emax_nls recovers true parameters from simulated data", {
  skip_if(is_clang(), "Fails on clang due to toolchain issue unrelated to emaxnls code")
  skip_if(is_gcc15(), "Fails on gcc15 due to toolchain issue unrelated to emaxnls code")
  skip_if(!emax_converged(mod_sig))
  est <- coef(mod_sig)
  expect_equal(unname(est["E0_Intercept"]),      true_E0_sig,      tolerance = 0.05)
  expect_equal(unname(est["Emax_Intercept"]),    true_Emax_sig,    tolerance = 0.05)
  expect_equal(unname(est["logEC50_Intercept"]), true_logEC50_sig, tolerance = 0.05)
  expect_equal(unname(est["logHill_Intercept"]), true_logHill_sig, tolerance = 0.05)
})

# -------------------------------------------------------------------------
# 3. Binary logistic Emax model
# -------------------------------------------------------------------------

true_E0_bin      <- -2.0   # logit scale (response prob ~0.12 at baseline)
true_Emax_bin    <-  4.0   # logit scale (response prob ~0.88 at saturation)
true_EC50_bin    <- 100.0
true_logEC50_bin <- log(true_EC50_bin)

set.seed(7823)
doses_bin <- rep(c(0, 10, 30, 100, 300, 1000), each = 500L)
eta_bin   <- true_E0_bin +
  true_Emax_bin * doses_bin / (doses_bin + true_EC50_bin)
p_bin     <- 1 / (1 + exp(-eta_bin))
sim_bin   <- data.frame(
  dose = doses_bin,
  rsp  = stats::rbinom(length(doses_bin), size = 1L, prob = p_bin)
)

mod_bin <- emax_logistic(
  structural_model = rsp ~ dose,
  covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
  data             = sim_bin
)

test_that("logistic emax_logistic converges on simulated data", {
  skip_if(is_clang(), "Fails on clang due to toolchain issue unrelated to emaxnls code")
  skip_if(is_gcc15(), "Fails on gcc15 due to toolchain issue unrelated to emaxnls code")
  expect_true(emax_converged(mod_bin))
})

test_that("logistic emax_logistic recovers true parameters from simulated data", {
  skip_if(is_clang(), "Fails on clang due to toolchain issue unrelated to emaxnls code")
  skip_if(is_gcc15(), "Fails on gcc15 due to toolchain issue unrelated to emaxnls code")
  skip_if(!emax_converged(mod_bin))
  est <- coef(mod_bin)
  expect_equal(unname(est["E0_Intercept"]),      true_E0_bin,      tolerance = 0.05)
  expect_equal(unname(est["Emax_Intercept"]),    true_Emax_bin,    tolerance = 0.05)
  expect_equal(unname(est["logEC50_Intercept"]), true_logEC50_bin, tolerance = 0.05)
})

# -------------------------------------------------------------------------
# 4. Continuous hyperbolic model with a covariate on E0
# -------------------------------------------------------------------------
#
# The full model is:
#
#   E0(cov) = true_E0_int + true_E0_cov * cov
#   rsp = E0(cov) + Emax * dose / (dose + EC50) + noise
#
# The covariate is standardised (mean 0, sd 1), so the intercept equals
# the expected E0 at the covariate mean, and the slope is directly
# interpretable as the change in E0 per unit increase in cov.

true_E0_cov_int  <- 1.0   # E0 at cov = 0
true_E0_cov_slp  <- 0.5   # change in E0 per unit of cov
true_Emax_cov    <- 5.0
true_EC50_cov    <- 100.0
true_logEC50_cov <- log(true_EC50_cov)

set.seed(4419)
n_cov   <- 900L
dose_cv <- rep(c(0, 10, 30, 100, 300, 1000), each = n_cov / 6L)
cov_cv  <- stats::rnorm(n_cov, mean = 0, sd = 1)

mu_cov  <- (true_E0_cov_int + true_E0_cov_slp * cov_cv) +
  true_Emax_cov * dose_cv / (dose_cv + true_EC50_cov)
sim_cov <- data.frame(
  dose = dose_cv,
  cov  = cov_cv,
  rsp  = mu_cov + stats::rnorm(n_cov, mean = 0, sd = 0.2)
)

mod_cov <- emax_nls(
  structural_model = rsp ~ dose,
  covariate_model  = list(E0 ~ cov, Emax ~ 1, logEC50 ~ 1),
  data             = sim_cov
)

test_that("covariate model emax_nls converges on simulated data", {
  skip_if(is_clang(), "Fails on clang due to toolchain issue unrelated to emaxnls code")
  skip_if(is_gcc15(), "Fails on gcc15 due to toolchain issue unrelated to emaxnls code")
  expect_true(emax_converged(mod_cov))
})

test_that("covariate model emax_nls recovers true parameters from simulated data", {
  skip_if(is_clang(), "Fails on clang due to toolchain issue unrelated to emaxnls code")
  skip_if(is_gcc15(), "Fails on gcc15 due to toolchain issue unrelated to emaxnls code")
  skip_if(!emax_converged(mod_cov))
  est <- coef(mod_cov)
  expect_equal(unname(est["E0_Intercept"]),      true_E0_cov_int,  tolerance = 0.05)
  expect_equal(unname(est["E0_cov"]),            true_E0_cov_slp,  tolerance = 0.05)
  expect_equal(unname(est["Emax_Intercept"]),    true_Emax_cov,    tolerance = 0.05)
  expect_equal(unname(est["logEC50_Intercept"]), true_logEC50_cov, tolerance = 0.05)
})
