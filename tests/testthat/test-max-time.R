
# Tests for the max_time option in emax_nls_options() and emax_logistic_options()

str_mod <- rsp_1 ~ exp_1
cov_mod <- list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1)


# --- options storage ---------------------------------------------------------

test_that("emax_nls_options() stores max_time = Inf by default", {
  opts <- emax_nls_options()
  expect_equal(opts$max_time, Inf)
})

test_that("emax_nls_options() stores a finite max_time correctly", {
  opts <- emax_nls_options(max_time = 10)
  expect_equal(opts$max_time, 10)
})

test_that("emax_logistic_options() stores max_time = Inf by default", {
  opts <- emax_logistic_options()
  expect_equal(opts$max_time, Inf)
})

test_that("emax_logistic_options() stores a finite max_time correctly", {
  opts <- emax_logistic_options(max_time = 10)
  expect_equal(opts$max_time, 10)
})

test_that("emax_nls_options() rejects invalid max_time values", {
  expect_error(emax_nls_options(max_time = -1),    class = "emaxnls_error")
  expect_error(emax_nls_options(max_time = 0),     class = "emaxnls_error")
  expect_error(emax_nls_options(max_time = "10"),  class = "emaxnls_error")
  expect_error(emax_nls_options(max_time = c(1, 2)), class = "emaxnls_error")
})

test_that("emax_logistic_options() rejects invalid max_time values", {
  expect_error(emax_logistic_options(max_time = -1),    class = "emaxnls_error")
  expect_error(emax_logistic_options(max_time = 0),     class = "emaxnls_error")
  expect_error(emax_logistic_options(max_time = "10"),  class = "emaxnls_error")
  expect_error(emax_logistic_options(max_time = c(1, 2)), class = "emaxnls_error")
})


# --- max_time stored in fitted model -----------------------------------------

test_that("max_time is stored in the fitted emax_nls model options", {
  mod <- suppressWarnings(emax_nls(
    structural_model = str_mod,
    covariate_model  = cov_mod,
    data             = emax_df,
    opts             = emax_nls_options(max_time = 30)
  ))
  expect_equal(.get_options(mod)$max_time, 30)
})

test_that("max_time is stored in the fitted emax_logistic model options", {
  mod <- suppressWarnings(emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = cov_mod,
    data             = emax_df,
    opts             = emax_logistic_options(max_time = 30)
  ))
  expect_equal(.get_options(mod)$max_time, 30)
})


# --- timeout causes non-convergence ------------------------------------------
#
# R's setTimeLimit() checking happens at user interrupt check points, which are
# frequent in R-level code but tied to system timer granularity (~5-10ms). We
# use a large dataset to ensure the fit genuinely exceeds the time limit, and
# max_time = 0.05 (50ms) to sit well above the timer resolution floor.

# helper: replicate emax_df to a size that makes fits reliably slow (> 400ms)
make_slow_data <- function() {
  do.call(rbind, replicate(500, emax_df, simplify = FALSE))
}

test_that("emax_nls() treats a timeout as non-convergence", {
  skip_on_cran()
  mod <- suppressWarnings(emax_nls(
    structural_model = str_mod,
    covariate_model  = cov_mod,
    data             = make_slow_data(),
    opts             = emax_nls_options(max_time = 0.05)
  ))
  expect_s3_class(mod, "emaxnls")
  expect_false(emax_converged(mod))
  expect_match(conditionMessage(mod$env$error), "time limit", ignore.case = TRUE)
})

test_that("emax_logistic() treats a timeout as non-convergence", {
  skip_on_cran()
  mod <- suppressWarnings(emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = cov_mod,
    data             = make_slow_data(),
    opts             = emax_logistic_options(max_time = 0.05)
  ))
  expect_s3_class(mod, "emaxlogistic")
  expect_false(emax_converged(mod))
  expect_match(conditionMessage(mod$env$error), "time limit", ignore.case = TRUE)
})


# --- time limit reset after fitting ------------------------------------------
#
# A finite max_time must not leave an active time limit in the session after
# the function returns, whether the fit completed normally or timed out. This
# matters especially in SCM loops where emax_nls() is called many times.

test_that("emax_nls() resets the time limit to Inf on normal exit", {
  skip_on_cran()
  suppressWarnings(emax_nls(
    structural_model = str_mod,
    covariate_model  = cov_mod,
    data             = emax_df,
    opts             = emax_nls_options(max_time = 30)
  ))
  # After return, Sys.sleep() must not trigger a time-limit error
  expect_no_error(Sys.sleep(0.01))
})

test_that("emax_nls() resets the time limit to Inf after a timeout", {
  skip_on_cran()
  suppressWarnings(emax_nls(
    structural_model = str_mod,
    covariate_model  = cov_mod,
    data             = make_slow_data(),
    opts             = emax_nls_options(max_time = 0.05)
  ))
  expect_no_error(Sys.sleep(0.01))
})

test_that("emax_logistic() resets the time limit to Inf on normal exit", {
  skip_on_cran()
  suppressWarnings(emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = cov_mod,
    data             = emax_df,
    opts             = emax_logistic_options(max_time = 30)
  ))
  expect_no_error(Sys.sleep(0.01))
})

test_that("emax_logistic() resets the time limit to Inf after a timeout", {
  skip_on_cran()
  suppressWarnings(emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = cov_mod,
    data             = make_slow_data(),
    opts             = emax_logistic_options(max_time = 0.05)
  ))
  expect_no_error(Sys.sleep(0.01))
})
