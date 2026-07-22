
# test-er-methods.R
# Tests for the erplots model interface: er_predict(), er_simulate(),
# er_summary() for emaxnls and emaxlogistic objects.
#
# All tests are skipped unless erplots is installed.

skip_if_not_installed("erplots")

# test fixtures ------------------------------------------------------------

# Continuous model without extra covariates (baseline case; E0/Emax/logEC50
# intercept-only so newdata needs no covariate fill-in)
mod_c_simple <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df,
  opts             = test_nls_opts()
)

# Continuous model with a covariate on E0 (exercises .emax_reference_data())
mod_c_cov <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model  = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df,
  opts             = test_nls_opts()
)

# Logistic model with a covariate (exercises emaxlogistic dispatch)
mod_b_cov <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model  = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df,
  opts             = test_logistic_opts()
)

# A minimal newdata with only the exposure variable (as erplots would pass)
newdata_exp <- data.frame(exp_1 = seq(0, 300, length.out = 20))


# er_predict() - simple continuous model -----------------------------------

test_that("er_predict() returns newdata with three extra columns for emaxnls", {
  skip_if_not_converged(mod_c_simple)
  result <- erplots::er_predict(mod_c_simple, newdata = newdata_exp)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(newdata_exp))
  expect_true(all(c("exp_1", "fit_resp", "ci_lower", "ci_upper") %in% names(result)))
})

test_that("er_predict() preserves original newdata columns", {
  skip_if_not_converged(mod_c_simple)
  result <- erplots::er_predict(mod_c_simple, newdata = newdata_exp)
  expect_equal(result$exp_1, newdata_exp$exp_1)
})

test_that("er_predict() intervals bracket point predictions for emaxnls", {
  skip_if_not_converged(mod_c_simple)
  result <- erplots::er_predict(mod_c_simple, newdata = newdata_exp)
  expect_true(all(result$ci_lower <= result$fit_resp))
  expect_true(all(result$fit_resp <= result$ci_upper))
})


# er_predict() - covariate fill-in via .emax_reference_data() -------------

test_that("er_predict() works when newdata lacks model covariates", {
  skip_if_not_converged(mod_c_cov)
  # newdata has only exp_1; the model uses cnt_a as a covariate on E0
  expect_no_error(
    result <- erplots::er_predict(mod_c_cov, newdata = newdata_exp)
  )
  expect_equal(nrow(result), nrow(newdata_exp))
  expect_true(all(c("fit_resp", "ci_lower", "ci_upper") %in% names(result)))
})

test_that("er_predict() does not leak filled-in covariate columns into output", {
  skip_if_not_converged(mod_c_cov)
  result <- erplots::er_predict(mod_c_cov, newdata = newdata_exp)
  # cnt_a must NOT appear in the returned data frame
  expect_false("cnt_a" %in% names(result))
  # only the original column plus the three contract columns should be present
  expect_setequal(names(result), c("exp_1", "fit_resp", "ci_lower", "ci_upper"))
})


# er_predict() - emaxlogistic dispatch ------------------------------------

test_that("er_predict() works for emaxlogistic and returns probabilities in [0, 1]", {
  skip_if_not_converged(mod_b_cov)
  result <- erplots::er_predict(mod_b_cov, newdata = newdata_exp)
  expect_equal(nrow(result), nrow(newdata_exp))
  expect_true(all(result$fit_resp >= 0 & result$fit_resp <= 1))
  expect_true(all(result$ci_lower >= 0 & result$ci_lower <= 1))
  expect_true(all(result$ci_upper >= 0 & result$ci_upper <= 1))
})

test_that("er_predict() does not leak covariate columns for emaxlogistic", {
  skip_if_not_converged(mod_b_cov)
  result <- erplots::er_predict(mod_b_cov, newdata = newdata_exp)
  expect_false("cnt_a" %in% names(result))
})


# er_simulate() -----------------------------------------------------------

test_that("er_simulate() returns nsim * nrow(newdata) rows", {
  skip_if_not_converged(mod_c_cov)
  nsim <- 10L
  result <- erplots::er_simulate(mod_c_cov, newdata = newdata_exp, nsim = nsim)
  expect_equal(nrow(result), nsim * nrow(newdata_exp))
})

test_that("er_simulate() has sim_id ranging 1:nsim", {
  skip_if_not_converged(mod_c_cov)
  nsim <- 10L
  result <- erplots::er_simulate(mod_c_cov, newdata = newdata_exp, nsim = nsim)
  expect_setequal(unique(result$sim_id), seq_len(nsim))
})

test_that("er_simulate() has a fit_resp column", {
  skip_if_not_converged(mod_c_cov)
  result <- erplots::er_simulate(mod_c_cov, newdata = newdata_exp, nsim = 5L)
  expect_true("fit_resp" %in% names(result))
  expect_type(result$fit_resp, "double")
})

test_that("er_simulate() is reproducible with the same seed", {
  skip_if_not_converged(mod_c_cov)
  r1 <- erplots::er_simulate(mod_c_cov, newdata = newdata_exp, nsim = 5L, seed = 42L)
  r2 <- erplots::er_simulate(mod_c_cov, newdata = newdata_exp, nsim = 5L, seed = 42L)
  expect_equal(r1$fit_resp, r2$fit_resp)
})

test_that("er_simulate() returns probabilities in [0, 1] for emaxlogistic", {
  skip_if_not_converged(mod_b_cov)
  result <- erplots::er_simulate(mod_b_cov, newdata = newdata_exp, nsim = 5L)
  expect_true(all(result$fit_resp >= 0 & result$fit_resp <= 1))
})

test_that("er_simulate() does not leak filled-in covariate columns", {
  skip_if_not_converged(mod_c_cov)
  result <- erplots::er_simulate(mod_c_cov, newdata = newdata_exp, nsim = 5L)
  expect_false("cnt_a" %in% names(result))
})


# er_summary() ------------------------------------------------------------

test_that("er_summary() returns a list with p_value, coefficients, glance", {
  skip_if_not_converged(mod_c_cov)
  result <- erplots::er_summary(mod_c_cov)
  expect_type(result, "list")
  expect_named(result, c("p_value", "coefficients", "glance"))
})

test_that("er_summary() always returns NULL for p_value", {
  skip_if_not_converged(mod_c_cov)
  result <- erplots::er_summary(mod_c_cov)
  expect_null(result$p_value)
})

test_that("er_summary() $coefficients has one row per parameter", {
  skip_if_not_converged(mod_c_cov)
  result <- erplots::er_summary(mod_c_cov)
  coefs  <- result$coefficients
  expect_s3_class(coefs, "data.frame")
  expect_true(all(c("term", "estimate", "std_error", "statistic",
                    "p_value", "conf_low", "conf_high") %in% names(coefs)))
  expect_equal(nrow(coefs), length(coef(mod_c_cov)))
})

test_that("er_summary() $glance has expected columns", {
  skip_if_not_converged(mod_c_cov)
  result <- erplots::er_summary(mod_c_cov)
  gl     <- result$glance
  expect_s3_class(gl, "data.frame")
  expect_equal(nrow(gl), 1L)
  expect_true(all(c("n", "df_residual", "logLik", "aic", "bic",
                    "deviance", "r_squared", "converged") %in% names(gl)))
})

test_that("er_summary() r_squared is non-NA for emaxnls", {
  skip_if_not_converged(mod_c_cov)
  result <- erplots::er_summary(mod_c_cov)
  expect_false(is.na(result$glance$r_squared))
  expect_true(result$glance$r_squared >= 0 & result$glance$r_squared <= 1)
})

test_that("er_summary() r_squared is NA for emaxlogistic", {
  skip_if_not_converged(mod_b_cov)
  result <- erplots::er_summary(mod_b_cov)
  expect_true(is.na(result$glance$r_squared))
})

test_that("er_summary() p_value is NULL for emaxlogistic", {
  skip_if_not_converged(mod_b_cov)
  result <- erplots::er_summary(mod_b_cov)
  expect_null(result$p_value)
})


# End-to-end smoke test ----------------------------------------------------

test_that("er_plot pipeline completes without error for continuous model", {
  skip_if_not_converged(mod_c_cov)
  p <- erplots::er_plot(data = emax_df, exposure = exp_1, response = rsp_1) |>
    erplots::er_plot_add_model(model = mod_c_cov) |>
    erplots::er_plot_add_summary(model = mod_c_cov) |>
    erplots::er_plot_build()
  # just confirm it produced something without error
  expect_false(is.null(p))
})

test_that("er_plot pipeline completes without error for logistic model", {
  skip_if_not_converged(mod_b_cov)
  p <- erplots::er_plot(data = emax_df, exposure = exp_1, response = rsp_2) |>
    erplots::er_plot_add_model(model = mod_b_cov) |>
    erplots::er_plot_add_summary(model = mod_b_cov) |>
    erplots::er_plot_build()
  expect_false(is.null(p))
})
