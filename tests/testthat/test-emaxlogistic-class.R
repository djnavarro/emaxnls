
# Tests for the emaxlogistic class constructor and IRLS fitting

mod <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df
)

mod_cov <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model  = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df
)

test_that("emax_logistic() returns an emaxlogistic object", {
  expect_s3_class(mod, "emaxlogistic")
  expect_s3_class(mod, "emaxnls")  # inherits
})

test_that("emax_logistic() object has correct structure", {
  expect_named(mod, c("formula", "data", "info", "env"))
  expect_named(mod$formula, c("structural", "covariate", "expanded"))
  expect_named(mod$info, c("opts", "init", "design", "model_type", "variables"))
  expect_true(!is.null(mod$env$model))
  expect_true(!is.null(mod$env$irls))
})

test_that("emax_converged() works for emaxlogistic objects", {
  expect_true(emax_converged(mod))
  expect_true(emax_converged(mod_cov))
})

test_that("IRLS diagnostics are stored correctly", {
  irls <- .get_irls(mod)
  expect_true(irls$converged)
  expect_true(irls$iter >= 1L)
  expect_true(irls$iter <= 25L)  # within max_iter default
})

test_that("emax_logistic() handles non-convergence gracefully", {
  # Limiting the NLS inner step to 1 iteration forces non-convergence
  mod_bad <- suppressWarnings(emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
    data             = emax_df,
    opts             = emax_logistic_options(
      optim_control = stats::nls.control(maxiter = 1),
      quiet         = TRUE
    )
  ))
  expect_false(emax_converged(mod_bad))
})

test_that("emax_logistic() issues a warning when IRLS nls step fails", {
  expect_warning(
    emax_logistic(
      structural_model = rsp_2 ~ exp_1,
      covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
      data             = emax_df,
      opts             = emax_logistic_options(
        optim_control = stats::nls.control(maxiter = 1)
      )
    ),
    class = "emaxnls_warning"
  )
})

test_that("emax_logistic_options() configures correctly", {
  opts <- emax_logistic_options(max_iter = 10, tol = 1e-4, quiet = TRUE)
  expect_equal(opts$max_iter, 10)
  expect_equal(opts$tol, 1e-4)
  expect_true(opts$quiet)
  expect_null(opts$weights)  # always NULL for logistic
})

test_that("sigmoidal logistic Emax model fits successfully", {
  mod_sig <- emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1, logHill ~ 1),
    data             = emax_df
  )
  expect_true(emax_converged(mod_sig))
  expect_equal(.get_model_type(mod_sig), "sigmoidal")
  expect_length(coef(mod_sig), 4L)
})

test_that("emax_logistic() correctly identifies model type", {
  expect_equal(.get_model_type(mod), "hyperbolic")
})
