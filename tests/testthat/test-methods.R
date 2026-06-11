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

# does not converge
mod_bad <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df,
  opts = emax_nls_options(
    optim_method = "gauss",
    optim_control = stats::nls.control(maxiter = 1)
  )
)

test_that("methods return emaxnls_null for models that do not converge", {
  expect_s3_class(coef(mod_bad), "emaxnls_null")
  expect_s3_class(vcov(mod_bad), "emaxnls_null")
  expect_s3_class(residuals(mod_bad), "emaxnls_null")
  expect_s3_class(simulate(mod_bad), "emaxnls_null")
  expect_s3_class(logLik(mod_bad), "emaxnls_null")
  expect_s3_class(AIC(mod_bad), "emaxnls_null")
  expect_s3_class(BIC(mod_bad), "emaxnls_null")
  expect_s3_class(predict(mod_bad), "emaxnls_null")
  expect_s3_class(confint(mod_bad), "emaxnls_null")
  expect_s3_class(nobs(mod_bad), "emaxnls_null")
  expect_s3_class(sigma(mod_bad), "emaxnls_null")
  expect_s3_class(deviance(mod_bad), "emaxnls_null")
  expect_s3_class(fitted(mod_bad), "emaxnls_null")
  expect_s3_class(df.residual(mod_bad), "emaxnls_null")
})

test_that("AIC(), BIC(), and anova() handle cases where some models do not converge", {
  expect_no_error(AIC(mod_base, mod, mod_bad))
  expect_no_error(BIC(mod_base, mod, mod_bad))
  expect_no_error(anova(mod_base, mod, mod_bad))

  expect_warning(AIC(mod_base, mod, mod_bad))
  expect_warning(BIC(mod_base, mod, mod_bad))
  expect_warning(anova(mod_base, mod, mod_bad))

  expect_s3_class(AIC(mod_base, mod, mod_bad), "data.frame")
  expect_s3_class(BIC(mod_base, mod, mod_bad), "data.frame")
  expect_s3_class(anova(mod_base, mod, mod_bad), "data.frame")

})
