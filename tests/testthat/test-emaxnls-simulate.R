mod <- emax_nls(
   structural_model = response_1 ~ exposure_1, 
   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
   data = emax_df
 )

test_that("simulate() returns a data frame", {
  expect_no_error(simulate(mod))
  sim <- simulate(mod)
  expect_s3_class(sim, "data.frame")
})

test_that("emax_fn() returns a function", {
  expect_no_error(emax_fn(mod))
  fn <- emax_fn(mod)
  expect_equal(formals(fn), pairlist(data = NULL, param = NULL))
})

test_that("emax_fn() allows custom data and params", {
  fn <- emax_fn(mod)
  pp <- coef(mod)
  e0 <- pp["E0_Intercept"]
  pp["E0_Intercept"] <- 0.0
  out1 <- fn(data = emax_df[120L:125L, ])
  out2 <- fn(data = emax_df[120L:125L, ], param = pp)
  expect_length(out1, 6L)
  expect_length(out2, 6L)
  expect_true(is.numeric(out1))
  expect_true(is.numeric(out2))
  expect_equal(out1, out2 + e0)
})
