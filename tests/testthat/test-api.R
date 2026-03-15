
test_that("emax_nls works with test data", {
  expect_no_error(emax_nls(
    structural_model = rsp_1 ~ exp_1, 
    covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
    data = emax_df
  ))
})

str_mod <- rsp_1 ~ exp_1
cov_mod <- list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1)
mod <- emax_nls(
  structural_model = str_mod, 
  covariate_model = cov_mod, 
  data = emax_df
)

test_that("emax_nls_init works with test data", {
  expect_no_error(emax_nls_init(str_mod, cov_mod, emax_df)) 
  gg <- emax_nls_init(str_mod, cov_mod, emax_df)
  expect_s3_class(gg, "data.frame")
})

test_that("emax_nls_options returns a list", {
  expect_no_error(emax_nls_options())
  oo <- emax_nls_options()
  expect_type(oo, "list")
})

test_that("emax_add_term returns an emaxnls object", {
  expect_no_error(emax_add_term(mod, E0 ~ cnt_b))
  mm <- emax_add_term(mod, E0 ~ cnt_b)
  expect_s3_class(mm, "emaxnls")
})

test_that("emax_remove_term returns an emaxnls object", {
  expect_no_error(emax_remove_term(mod, E0 ~ cnt_a))
  mm <- emax_remove_term(mod, E0 ~ cnt_a)
  expect_s3_class(mm, "emaxnls")
})

candidates <- list(E0 = c("cnt_b", "cnt_c"))

test_that("emax_scm_forward returns an emaxnls object", {
  expect_no_error(emax_scm_forward(mod, candidates))
  mm <- emax_scm_forward(mod, candidates)
  expect_s3_class(mm, "emaxnls")
})

candidates <- list(E0 = c("cnt_a"))

test_that("emax_scm_backward returns an emaxnls object", {
  expect_no_error(emax_scm_backward(mod, candidates))
  mm <- emax_scm_backward(mod, candidates)
  expect_s3_class(mm, "emaxnls")
})

mod2 <- emax_scm_backward(mod, candidates)

test_that("emax_scm_history returns a data frame", {
  expect_no_error(emax_scm_history(mod2))
  hh <- emax_scm_history(mod2)
  expect_s3_class(hh, "data.frame")
})

test_that("emax_fun() returns a function", {
  expect_no_error(emax_fun(mod))
  fn <- emax_fun(mod)
  expect_equal(formals(fn), pairlist(param = NULL, data = NULL))
})


