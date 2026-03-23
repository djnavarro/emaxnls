mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

test_that("emax_nls object has expected top-level structure", {
  expect_named(mod, c("formula", "data", "info", "env"))
  expect_true(inherits(mod$formula, "list"))
  expect_true(inherits(mod$data, "data.frame"))
  expect_true(inherits(mod$info, "list"))
  expect_true(inherits(mod$env, "environment"))
})
