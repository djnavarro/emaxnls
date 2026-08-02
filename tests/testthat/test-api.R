
test_that("emax_nls works with test data", {
  expect_no_error(emax_nls(
    structural_model = rsp_1 ~ exp_1, 
    covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
    data = emax_df,
    opts = test_nls_opts()
  ))
})

str_mod <- rsp_1 ~ exp_1
cov_mod <- list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1)
mod <- emax_nls(
  structural_model = str_mod, 
  covariate_model = cov_mod, 
  data = emax_df,
  opts = test_nls_opts()
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


# emax_converged() returns a named logical -----------------------------------

test_that("emax_converged() returns a logical scalar", {
  skip_if_not_converged(mod)
  result <- emax_converged(mod)
  expect_type(result, "logical")
  expect_length(result, 1L)
})

test_that("emax_converged() returns TRUE with name 'converged' for a converged model", {
  skip_if_not_converged(mod)
  result <- emax_converged(mod)
  expect_true(result)
  expect_equal(names(result), "converged")
})

test_that("emax_converged() returns FALSE with an informative name for a non-converged model", {
  # Force non-convergence via an absurdly tight iteration budget
  failed_mod <- suppressWarnings(emax_nls(
    structural_model = rsp_1 ~ exp_1,
    covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
    data             = emax_df,
    opts             = emax_nls_options(optim_control = list(maxiter = 1L))
  ))
  skip_if(.is_converged(failed_mod), "Model unexpectedly converged in 1 iteration")
  result <- emax_converged(failed_mod)
  expect_false(result)
  expect_false(is.null(names(result)))
  expect_true(nzchar(names(result)))
})


# NULL covariate_model default (issue #69) ------------------------------------

test_that("emax_nls() accepts NULL covariate_model and returns emaxnls object", {
  expect_no_error(emax_nls(
    structural_model = rsp_1 ~ exp_1,
    data = emax_df,
    opts = test_nls_opts()
  ))
  mm <- emax_nls(
    structural_model = rsp_1 ~ exp_1,
    data = emax_df,
    opts = test_nls_opts()
  )
  expect_s3_class(mm, "emaxnls")
})

test_that("emax_nls() NULL default matches explicit intercept-only list", {
  mod_null <- emax_nls(
    structural_model = rsp_1 ~ exp_1,
    data = emax_df,
    opts = test_nls_opts()
  )
  mod_explicit <- emax_nls(
    structural_model = rsp_1 ~ exp_1,
    covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
    data = emax_df,
    opts = test_nls_opts()
  )
  skip_if_not_converged(mod_null)
  skip_if_not_converged(mod_explicit)
  expect_equal(coef(mod_null), coef(mod_explicit))
})

test_that("emax_nls_init() accepts NULL covariate_model", {
  expect_no_error(emax_nls_init(
    structural_model = rsp_1 ~ exp_1,
    data = emax_df
  ))
  init_null <- emax_nls_init(structural_model = rsp_1 ~ exp_1, data = emax_df)
  init_explicit <- emax_nls_init(
    structural_model = rsp_1 ~ exp_1,
    covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
    data = emax_df
  )
  expect_equal(init_null, init_explicit)
})

test_that("emax_logistic() accepts NULL covariate_model and returns emaxlogistic object", {
  expect_no_error(emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    data = emax_df,
    opts = test_logistic_opts()
  ))
  mm <- emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    data = emax_df,
    opts = test_logistic_opts()
  )
  expect_s3_class(mm, "emaxlogistic")
})

test_that("emax_logistic() NULL default matches explicit intercept-only list", {
  mod_null <- emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    data = emax_df,
    opts = test_logistic_opts()
  )
  mod_explicit <- emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
    data = emax_df,
    opts = test_logistic_opts()
  )
  skip_if_not_converged(mod_null)
  skip_if_not_converged(mod_explicit)
  expect_equal(coef(mod_null), coef(mod_explicit))
})

test_that("emax_logistic_init() accepts NULL covariate_model", {
  expect_no_error(emax_logistic_init(
    structural_model = rsp_2 ~ exp_1,
    data = emax_df
  ))
  init_null <- emax_logistic_init(structural_model = rsp_2 ~ exp_1, data = emax_df)
  init_explicit <- emax_logistic_init(
    structural_model = rsp_2 ~ exp_1,
    covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
    data = emax_df
  )
  expect_equal(init_null, init_explicit)
})


