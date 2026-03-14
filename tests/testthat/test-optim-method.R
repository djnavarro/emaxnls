
# tests for base model with no covariates: gauss, port, levenberg

str_mod <- response_1 ~ exposure_1
cov_mod <- list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1)

test_that("example base model converges with 'gauss'", {
  mm <- "gauss"
  aa <- "default"
  expect_no_error(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  expect_no_warning(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  mod <- emax_nls(
    structural_model = str_mod, 
    covariate_model = cov_mod, 
    data = emax_df,
    opts = emax_nls_options(optim_method = mm)
  )
  expect_equal(mod$opts$optim_method, mm)
  expect_equal(mod$env$algorithm, aa)
  expect_null(mod$env$error)
  expect_s3_class(mod$env$model, "nls")
  expect_true(mod$env$model$convInfo$isConv)
})

test_that("example base model converges with 'port'", {
  mm <- "port"
  aa <- "port"
  expect_no_error(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  expect_no_warning(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  mod <- emax_nls(
    structural_model = str_mod, 
    covariate_model = cov_mod, 
    data = emax_df,
    opts = emax_nls_options(optim_method = mm)
  )
  expect_equal(mod$opts$optim_method, mm)
  expect_equal(mod$env$algorithm, aa)
  expect_null(mod$env$error)
  expect_s3_class(mod$env$model, "nls")
  expect_true(mod$env$model$convInfo$isConv)
})

test_that("example base model converges with 'levenberg'", {
  skip_if_not_installed("minpack.lm")
  mm <- "levenberg"
  aa <- "LM"
  expect_no_error(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  expect_no_warning(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  mod <- emax_nls(
    structural_model = str_mod, 
    covariate_model = cov_mod, 
    data = emax_df,
    opts = emax_nls_options(optim_method = mm)
  )
  expect_equal(mod$opts$optim_method, mm)
  expect_equal(mod$env$algorithm, aa)
  expect_null(mod$env$error)
  expect_s3_class(mod$env$model, "nls")
  expect_true(mod$env$model$convInfo$isConv)
})


# tests for sensible model with one covariate: gauss, port, levenberg

str_mod <- response_1 ~ exposure_1
cov_mod <- list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1)

test_that("example covariate model converges with 'gauss'", {
  mm <- "gauss"
  aa <- "default"
  expect_no_error(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  expect_no_warning(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  mod <- emax_nls(
    structural_model = str_mod, 
    covariate_model = cov_mod, 
    data = emax_df,
    opts = emax_nls_options(optim_method = mm)
  )
  expect_equal(mod$opts$optim_method, mm)
  expect_equal(mod$env$algorithm, aa)
  expect_null(mod$env$error)
  expect_s3_class(mod$env$model, "nls")
  expect_true(mod$env$model$convInfo$isConv)
})

test_that("example covariate model converges with 'port'", {
  mm <- "port"
  aa <- "port"
  expect_no_error(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  expect_no_warning(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  mod <- emax_nls(
    structural_model = str_mod, 
    covariate_model = cov_mod, 
    data = emax_df,
    opts = emax_nls_options(optim_method = mm)
  )
  expect_equal(mod$opts$optim_method, mm)
  expect_equal(mod$env$algorithm, aa)
  expect_null(mod$env$error)
  expect_s3_class(mod$env$model, "nls")
  expect_true(mod$env$model$convInfo$isConv)
})

test_that("example covariate model converges with 'levenberg'", {
  skip_if_not_installed("minpack.lm")
  mm <- "levenberg"
  aa <- "LM"
  expect_no_error(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  expect_no_warning(
    emax_nls(
      structural_model = str_mod, 
      covariate_model = cov_mod, 
      data = emax_df,
      opts = emax_nls_options(optim_method = mm)
    )
  )
  mod <- emax_nls(
    structural_model = str_mod, 
    covariate_model = cov_mod, 
    data = emax_df,
    opts = emax_nls_options(optim_method = mm)
  )
  expect_equal(mod$opts$optim_method, mm)
  expect_equal(mod$env$algorithm, aa)
  expect_null(mod$env$error)
  expect_s3_class(mod$env$model, "nls")
  expect_true(mod$env$model$convInfo$isConv)
})

test_that("emax_nls errors for unknown optim_method", {
  invalid_optim_methods <- list("garbage", "plinear", "default", 1, TRUE)
  for (mm in invalid_optim_methods) {
    expect_error(
      emax_nls(
        structural_model = str_mod, 
        covariate_model = cov_mod, 
        data = emax_df,
        opts = emax_nls_options(optim_method = "mm")
      ),
      class = "emaxnls_error"
    )
  }
})
