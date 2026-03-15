mod <- emax_nls(
   structural_model = rsp_1 ~ exp_1, 
   covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
   data = emax_df
 )

test_that("simulate() returns a data frame", {
  expect_no_error(simulate(mod))
  sim <- simulate(mod)
  expect_s3_class(sim, "data.frame")
})

test_that("emax_fun() returns a function", {
  expect_no_error(emax_fun(mod))
  fn <- emax_fun(mod)
  expect_equal(formals(fn), pairlist(param = NULL, data = NULL))
})

test_that("emax_fun() allows custom data and params", {
  fn <- emax_fun(mod)
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

test_that("emax_fun() mirrors predict() by default", {
  fn <- emax_fun(mod)
  val_f <- fn()
  val_p <- predict(mod)
  expect_equal(val_f, val_p)
})

