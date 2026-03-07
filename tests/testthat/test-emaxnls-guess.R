
str_model <- response_1 ~ exposure_1 
cov_model <- list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1)

dfs <- list(
  unmodified = emax_df,
  shifts_exp = within(emax_df, exposure_1 <- exposure_1 * 100),
  shifts_rsp = within(emax_df, response_1 <- response_1 * 100),
  shifts_cov = within(emax_df, cnt_a <- cnt_a * 100)
)

test_that(".guess_init works for basic model", {
  ss <- .store(cov_model, str_model, emax_df)
  expect_no_error(.guess_init(ss))
})

test_that(".guess_init returns expected data frame", {
  for (df in dfs) {
    ss <- .store(cov_model, str_model, df)
    gg <- .guess_init(ss)
    expect_true(inherits(gg, "data.frame"))
    expect_equal(names(gg), c("parameter", "covariate", "start", "lower", "upper"))
    cc <- unname(unlist(ss$coefficients))
    expect_equal(nrow(gg), length(cc))
    expect_equal(paste(gg$parameter, gg$covariate, sep = "_"), cc)
    expect_true(is.numeric(gg$start))
    expect_true(is.numeric(gg$lower))
    expect_true(is.numeric(gg$upper))
    expect_true(all(gg$start > gg$lower))
    expect_true(all(gg$start < gg$upper))
  }
})

test_that(".guess_init scales logEC50 with exposure", {

  ss1 <- .store(cov_model, str_model, dfs$unmodified)
  gg1 <- .guess_init(ss1)

  ss2 <- .store(cov_model, str_model, dfs$shifts_exp)
  gg2 <- .guess_init(ss2)

  # no difference for other parameters
  expect_equal(
    dplyr::filter(gg1, parameter != "logEC50"),
    dplyr::filter(gg2, parameter != "logEC50")
  )

  # guesses for the logEC50 parameters
  gg1_exppars <- dplyr::filter(gg1, parameter == "logEC50")
  gg2_exppars <- dplyr::filter(gg2, parameter == "logEC50")

  # multiplying exposures by 100 should increase all guesses by log(100) 
  dev_start <- gg2_exppars$start - gg1_exppars$start - log(100)
  dev_upper <- gg2_exppars$upper - gg1_exppars$upper - log(100)
  dev_lower <- gg2_exppars$lower - gg1_exppars$lower - log(100)

  expect_true(all(abs(dev_start) < .0000001))
  expect_true(all(abs(dev_upper) < .0000001))
  expect_true(all(abs(dev_lower) < .0000001))

})


test_that(".guess_init scales E0 and Emax with response", {

  ss1 <- .store(cov_model, str_model, dfs$unmodified)
  gg1 <- .guess_init(ss1)

  ss2 <- .store(cov_model, str_model, dfs$shifts_rsp)
  gg2 <- .guess_init(ss2)

  # no difference for other parameters
  expect_equal(
    dplyr::filter(gg1, !(parameter %in% c("E0", "Emax"))),
    dplyr::filter(gg2, !(parameter %in% c("E0", "Emax")))
  )

  # guesses for the E0 and Emax parameters
  gg1_rsppars <- dplyr::filter(gg1, parameter %in% c("E0", "Emax"))
  gg2_rsppars <- dplyr::filter(gg2, parameter %in% c("E0", "Emax"))

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

  ss1 <- .store(cov_model, str_model, dfs$unmodified)
  gg1 <- .guess_init(ss1)

  ss2 <- .store(cov_model, str_model, dfs$shifts_cov)
  gg2 <- .guess_init(ss2)

  # no difference for other parameters
  expect_equal(
    dplyr::filter(gg1, covariate != "cnt_a"),
    dplyr::filter(gg2, covariate != "cnt_a")
  )

  # guesses for the E0 and Emax parameters
  gg1_covpars <- dplyr::filter(gg1, covariate == "cnt_a")
  gg2_covpars <- dplyr::filter(gg2, covariate == "cnt_a")

  # covariate coefficients scale inverse linearly
  dev_start <- gg2_covpars$start - (gg1_covpars$start / 100)
  dev_upper <- gg2_covpars$upper - (gg1_covpars$upper / 100)
  dev_lower <- gg2_covpars$lower - (gg1_covpars$lower / 100)

  expect_true(all(abs(dev_start) < .0000001))
  expect_true(all(abs(dev_upper) < .0000001))
  expect_true(all(abs(dev_lower) < .0000001))

})



