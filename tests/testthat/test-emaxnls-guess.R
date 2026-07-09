
str_model <- rsp_1 ~ exp_1 
cov_model <- list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1)

dfs <- list(
  unmodified = emax_df,
  shifts_exp = within(emax_df, exp_1 <- exp_1 * 100),
  shifts_rsp = within(emax_df, rsp_1 <- rsp_1 * 100),
  shifts_cov = within(emax_df, cnt_a <- cnt_a * 100)
)

test_that(".emax_nls_init works for basic model", {
  expect_no_error(.emax_nls_init(str_model, cov_model, emax_df))
})

test_that(".emax_nls_init returns expected data frame", {
  for (df in dfs) {
    gg <- .emax_nls_init(str_model, cov_model, emax_df)
    expect_true(inherits(gg, "data.frame"))
    expect_equal(names(gg), c("parameter", "covariate", "start", "lower", "upper"))
    expect_true(is.numeric(gg$start))
    expect_true(is.numeric(gg$lower))
    expect_true(is.numeric(gg$upper))
    expect_true(all(gg$start > gg$lower))
    expect_true(all(gg$start < gg$upper))
  }
})

test_that(".emax_nls_init parameter and covariate columns encode correct names", {
  # The coefficient names are encoded in the parameter and covariate columns.
  # Base R data.frame strips element names from numeric columns, so the
  # parameter/covariate columns are the authoritative source. See issue #57.
  gg <- .emax_nls_init(
    rsp_1 ~ exp_1,
    list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
    emax_df
  )
  coef_names <- paste(gg$parameter, gg$covariate, sep = "_")
  expect_equal(coef_names, c("E0_Intercept", "Emax_Intercept", "logEC50_Intercept"))
})

test_that(".emax_nls_init handles sigmoidal models: logHill row present with start = 0", {
  gg <- .emax_nls_init(
    rsp_1 ~ exp_1,
    list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1, logHill ~ 1),
    emax_df
  )
  expect_equal(nrow(gg), 4L)
  loghill_row <- gg[gg$parameter == "logHill" & gg$covariate == "Intercept", ]
  expect_equal(nrow(loghill_row), 1L)
  expect_equal(loghill_row$start, 0)
})

test_that(".guess_init scales logEC50 with exposure", {

  gg1 <- .emax_nls_init(str_model, cov_model, dfs$unmodified)
  gg2 <- .emax_nls_init(str_model, cov_model, dfs$shifts_exp)

  # no difference for other parameters
  expect_equal(
    .filter(gg1, parameter != "logEC50"),
    .filter(gg2, parameter != "logEC50")
  )

  # guesses for the logEC50 parameters
  gg1_exppars <- .filter(gg1, parameter == "logEC50")
  gg2_exppars <- .filter(gg2, parameter == "logEC50")

  # multiplying exposures by 100 should increase all guesses by log(100) 
  dev_start <- gg2_exppars$start - gg1_exppars$start - log(100)
  dev_upper <- gg2_exppars$upper - gg1_exppars$upper - log(100)
  dev_lower <- gg2_exppars$lower - gg1_exppars$lower - log(100)

  expect_true(all(abs(dev_start) < .0000001))
  expect_true(all(abs(dev_upper) < .0000001))
  expect_true(all(abs(dev_lower) < .0000001))

})


test_that(".guess_init scales E0 and Emax with response", {

  gg1 <- .emax_nls_init(str_model, cov_model, dfs$unmodified)
  gg2 <- .emax_nls_init(str_model, cov_model, dfs$shifts_rsp)

  # no difference for other parameters
  expect_equal(
    .filter(gg1, !(parameter %in% c("E0", "Emax"))),
    .filter(gg2, !(parameter %in% c("E0", "Emax")))
  )

  # guesses for the E0 and Emax parameters
  gg1_rsppars <- .filter(gg1, parameter %in% c("E0", "Emax"))
  gg2_rsppars <- .filter(gg2, parameter %in% c("E0", "Emax"))

  # E0 and Emax are linear with response: multiplying by 100 should 
  # scale the initial guesses accordingly
  dev_start <- gg2_rsppars$start - (100 * gg1_rsppars$start)
  dev_upper <- gg2_rsppars$upper - (100 * gg1_rsppars$upper)
  dev_lower <- gg2_rsppars$lower - (100 * gg1_rsppars$lower)

  expect_true(all(abs(dev_start) < .0000001))
  expect_true(all(abs(dev_upper) < .0000001))
  expect_true(all(abs(dev_lower) < .0000001))

})

test_that(".guess_init scales coefficient with covariate", {

  gg1 <- .emax_nls_init(str_model, cov_model, dfs$unmodified)
  gg2 <- .emax_nls_init(str_model, cov_model, dfs$shifts_cov)
  
  # no difference for other parameters
  expect_equal(
    .filter(gg1, covariate != "cnt_a"),
    .filter(gg2, covariate != "cnt_a")
  )

  # guesses for the E0 and Emax parameters
  gg1_covpars <- .filter(gg1, covariate == "cnt_a")
  gg2_covpars <- .filter(gg2, covariate == "cnt_a")

  # covariate coefficients scale inverse linearly
  dev_start <- gg2_covpars$start - (gg1_covpars$start / 100)
  dev_upper <- gg2_covpars$upper - (gg1_covpars$upper / 100)
  dev_lower <- gg2_covpars$lower - (gg1_covpars$lower / 100)

  expect_true(all(abs(dev_start) < .0000001))
  expect_true(all(abs(dev_upper) < .0000001))
  expect_true(all(abs(dev_lower) < .0000001))

})



