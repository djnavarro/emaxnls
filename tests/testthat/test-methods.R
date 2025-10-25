mod <- emax_nls(
  structural_model = response_1 ~ exposure_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
lbl <- .extract_parameter_names(mod)

mod_base <- emax_nls(
  structural_model = response_1 ~ exposure_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

test_that("methods do not throw errors with basic use", {
  expect_no_error(coef(mod))
  expect_no_error(vcov(mod))
  expect_no_error(residuals(mod))
  expect_no_error(print(mod))
  expect_no_error(simulate(mod))
  expect_no_error(logLik(mod))
  expect_no_error(AIC(mod))
  expect_no_error(BIC(mod))
  expect_no_error(predict(mod))
  expect_no_error(confint(mod))
  expect_no_error(nobs(mod))
  expect_no_error(sigma(mod))
  expect_no_error(deviance(mod))
  expect_no_error(fitted(mod))
  expect_no_error(df.residual(mod))
})

test_that("AIC(), BIC(), and anova() can take multiple objects", {
  expect_no_error(AIC(mod_base, mod))
  expect_no_error(BIC(mod_base, mod))
  expect_no_error(anova(mod_base, mod))
})

test_that("coef() returns data frame with the expected structure", {
  cc <- coef(mod)
  expect_s3_class(cc, class = "data.frame")
  expect_named(cc, c("label", "estimate", "std_error", "t_statistic", "p_value", "ci_lower", "ci_upper"))
  expect_equal(cc$label, lbl)
})

test_that("vcov() returns symmetric numeric matrix with correct row/col names", {
  vv <- vcov(mod)
  expect_true(is.matrix(vv))
  expect_true(is.numeric(vv))
  expect_equal(nrow(vv), length(lbl))
  expect_equal(ncol(vv), length(lbl))
  expect_equal(rownames(vv), lbl)
  expect_equal(colnames(vv), lbl)
  expect_equal(vv, t(vv))
})

test_that("residuals() returns numeric vector of the same size as the data", {
  rr <- residuals(mod)
  expect_true(is.numeric(rr))
  expect_length(rr, nrow(emax_df))
})

test_that("print() writes expected message to console and returns object invisibly", {
  con <- textConnection("text_connection", "w")
  sink(con)
  val <- print(mod)
  sink()
  msg <- textConnectionValue(con)
  close(con)
  expect_equal(val, mod)
  expect_true(any(grepl("^Structural model:$", msg)))
  expect_true(any(grepl("^Covariate model:$", msg)))
  expect_true(any(grepl("^Coefficient table:$", msg)))
  expect_true(any(grepl("^Variance-covariance matrix:$", msg)))
})
