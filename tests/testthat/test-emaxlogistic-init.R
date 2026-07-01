
# Tests for logistic Emax model starting value heuristics

test_that("emax_logistic_init() returns the expected structure", {
  init <- emax_logistic_init(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
    data             = emax_df
  )
  expect_s3_class(init, "data.frame")
  expect_named(init, c("parameter", "covariate", "start", "lower", "upper"))
  expect_equal(nrow(init), 3L)  # E0, Emax, logEC50 intercepts
})

test_that("emax_logistic_init() start values are within lower/upper bounds", {
  init <- emax_logistic_init(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
    data             = emax_df
  )
  expect_true(all(init$start >= init$lower))
  expect_true(all(init$start <= init$upper))
})

test_that("emax_logistic_init() start values are named correctly", {
  init <- emax_logistic_init(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
    data             = emax_df
  )
  coef_names <- paste(init$parameter, init$covariate, sep = "_")
  expect_equal(names(init$start), coef_names)
})

test_that("emax_logistic_init() handles sigmoidal models", {
  init <- emax_logistic_init(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1, logHill ~ 1),
    data             = emax_df
  )
  expect_equal(nrow(init), 4L)
  expect_true("logHill_Intercept" %in% names(init$start))
  expect_equal(init$start["logHill_Intercept"], 0, ignore_attr = TRUE)
})

test_that("emax_logistic_init() validates inputs", {
  expect_error(
    emax_logistic_init(rsp_2 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
                       data = emax_df[, -which(names(emax_df) == "rsp_2")])
  )
})

test_that("emax_logistic_init() and emax_nls_init() return same structure", {
  init_nls <- emax_nls_init(rsp_1 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
  init_log <- emax_logistic_init(rsp_2 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
  expect_equal(names(init_nls), names(init_log))
  expect_equal(init_nls$parameter, init_log$parameter)
  expect_equal(init_nls$covariate, init_log$covariate)
})
