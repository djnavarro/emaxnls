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

test_that("emax_nls internal formula object has expected structure", {
  fml <- mod$formula

  # top-level structure of fml
  expect_length(fml, 3L)
  expect_named(fml, c("structural", "covariate", "expanded"))
  expect_s3_class(fml$structural, "formula")
  expect_s3_class(fml$expanded, "formula")
  expect_true(inherits(fml$covariate, "list"))

  # expected internal structure of fml$covariate
  expect_true(length(fml$covariate) %in% 3:4)
  expect_true(all(names(fml$covariate) %in% c("E0", "Emax", "logEC50", "logHill")))
  expect_true(all(vapply(fml$covariate, function(x) inherits(x, "formula"), logical(1L))))
})

test_that("emax_nls works with categorical covariates", {
  expect_no_error(emax_nls(
    structural_model = rsp_1 ~ exp_1, 
    covariate_model = list(E0 ~ cnt_a, Emax ~ cat_f, logEC50 ~ 1), 
    data = emax_df
  ))
})
