mod <- emax_nls(
  structural_model = response_1 ~ exposure_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

test_that("basic use of .emax_add_term and .emax_remove_term does not error", {
  expect_no_error(.emax_add_term(mod, E0 ~ bin_d))
  expect_no_error(.emax_remove_term(mod, E0 ~ cnt_a))
})

test_that(".emax_add_term and .emax_remove_term update the covariate model", {
  mod_add <- .emax_add_term(mod, E0 ~ bin_d)
  mod_del <- .emax_remove_term(mod, E0 ~ cnt_a)
  # check the coefficient names
  expect_true("E0_bin_d" %in% .extract_coefficient_names(mod_add))
  expect_false("E0_cnt_a" %in% .extract_coefficient_names(mod_del))
  # check the covariate formula
  expect_true("bin_d" %in% all.vars(.extract_covariate_formula(mod_add, "E0")))
  expect_false("cnt_a" %in% all.vars(.extract_covariate_formula(mod_del, "E0")))
  # check the internal parameters
  expect_length(.extract_nls(mod_add)$m$getPars(), 5L)
  expect_length(.extract_nls(mod_del)$m$getPars(), 3L)
})

test_that("adding a term and later removing leaves the model substantively unchanged", {
  mod_add <- .emax_add_term(mod, E0 ~ bin_d)
  mod_del <- .emax_remove_term(mod_add, E0 ~ bin_d)
  expect_equal(.extract_coefficient_names(mod_del), .extract_coefficient_names(mod))
  expect_equal(.extract_variable_names(mod_del), .extract_variable_names(mod))
  expect_equal(.extract_covariate_formula(mod_del), .extract_covariate_formula(mod), ignore_attr = TRUE)
  expect_equal(.extract_nls(mod)$m$getPars(), .extract_nls(mod_del)$m$getPars())
})

test_that("adding already-existing covariate messages user and returns original object", {
  expect_message(.emax_add_term(mod, E0 ~ cnt_a), class = "emaxnls_message")
  expect_equal(.emax_add_term(mod, E0 ~ cnt_a, quiet = TRUE), mod)
})

test_that("removing already-existing covariate messages user and returns original object", {
  expect_message(.emax_remove_term(mod, E0 ~ cnt_b), class = "emaxnls_message")
  expect_equal(.emax_remove_term(mod, E0 ~ cnt_b, quiet = TRUE), mod)
})

