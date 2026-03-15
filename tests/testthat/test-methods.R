mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
lbl <- .get_coefficient_names(mod)

mod_base <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
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

test_that("coef() returns numeric vector with correct length and names", {
  cc <- coef(mod)
  expect_true(is.vector(cc))
  expect_true(is.numeric(cc))
  expect_length(cc, length(lbl))
  expect_equal(names(cc), lbl)
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

