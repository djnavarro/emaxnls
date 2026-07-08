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
  if (!.is_converged(mod)) skip_on_ci()
  expect_no_error(coef(mod))
  expect_no_error(vcov(mod))
  expect_no_error(residuals(mod))
  expect_no_error(capture.output(print(mod)))
  if(requireNamespace("mvtnorm", quietly = TRUE)) expect_no_error(simulate(mod))
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
  if (!.is_converged(mod)) skip_on_ci()
  if (!.is_converged(mod_base)) skip_on_ci()
  expect_no_error(AIC(mod_base, mod))
  expect_no_error(BIC(mod_base, mod))
  expect_no_error(anova(mod_base, mod))
})

test_that("coef() returns numeric vector with correct length and names", {
  if (!.is_converged(mod)) skip_on_ci()
  cc <- coef(mod)
  expect_true(is.vector(cc))
  expect_true(is.numeric(cc))
  expect_length(cc, length(lbl))
  expect_equal(names(cc), lbl)
})

test_that("vcov() returns symmetric numeric matrix with correct row/col names", {
  if (!.is_converged(mod)) skip_on_ci()
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
  if (!.is_converged(mod)) skip_on_ci()
  rr <- residuals(mod)
  expect_true(is.numeric(rr))
  expect_length(rr, nrow(emax_df))
})

# does not converge
mod_bad <- suppressWarnings(emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df,
  opts = emax_nls_options(
    optim_method = "gauss",
    optim_control = stats::nls.control(maxiter = 1)
  )
))

test_that("methods return emaxnls_null for models that do not converge", {
  expect_s3_class(coef(mod_bad), "emaxnls_null")
  expect_s3_class(vcov(mod_bad), "emaxnls_null")
  expect_s3_class(residuals(mod_bad), "emaxnls_null")
  if(requireNamespace("mvtnorm", quietly = TRUE)) expect_s3_class(simulate(mod_bad), "emaxnls_null")
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
  if (!.is_converged(mod)) skip_on_ci()
  if (!.is_converged(mod_base)) skip_on_ci()

  expect_warning(AIC(mod_base, mod, mod_bad))
  expect_warning(BIC(mod_base, mod, mod_bad))
  expect_warning(anova(mod_base, mod, mod_bad))

  expect_s3_class(suppressWarnings(AIC(mod_base, mod, mod_bad)), "data.frame")
  expect_s3_class(suppressWarnings(BIC(mod_base, mod, mod_bad)), "data.frame")
  expect_s3_class(suppressWarnings(anova(mod_base, mod, mod_bad)), "data.frame")
})

test_that("summary() matches .coef_table()", {
  if (!.is_converged(mod)) skip_on_ci()
  expect_equal(summary(mod), .coef_table(mod, suppress_nonsensical = TRUE))
  expect_equal(summary(mod, conf_level = .99), .coef_table(mod, level = .99, suppress_nonsensical = TRUE))
  expect_equal(summary(mod, back_transform = TRUE), .coef_table(mod, back_transform = TRUE, suppress_nonsensical = TRUE))
})

test_that("back_transform works for confint()", {
  if (!.is_converged(mod)) skip_on_ci()
  ci1 <- confint(mod)
  ci2 <- confint(mod, back_transform = TRUE)
  expect_equal(ci1[-4,], ci2[-4,])
  expect_equal(unname(ci1[4,]), unname(log(ci2[4,])))
}) 

test_that("back_transform works for coef()", {
  if (!.is_converged(mod)) skip_on_ci()
  cc1 <- coef(mod)
  cc2 <- coef(mod, back_transform = TRUE)
  expect_equal(cc1[-4], cc2[-4])
  expect_equal(unname(cc1[4]), unname(log(cc2[4])))
}) 

test_that("confint(simultaneous = TRUE) matches summary(simultaneous = TRUE)", {
  if (!.is_converged(mod)) skip_on_ci()
  skip_if_not_installed("mvtnorm")
  # qmvnorm() uses randomised quasi-Monte Carlo, so the critical value varies
  # slightly between calls; compare with a tolerance rather than exactly.
  ci <- confint(mod, simultaneous = TRUE)
  s  <- summary(mod, simultaneous = TRUE)
  expect_equal(unname(ci[, 1]), unname(s$ci_lower), tolerance = 1e-2)
  expect_equal(unname(ci[, 2]), unname(s$ci_upper), tolerance = 1e-2)
})

test_that("confint(simultaneous = TRUE) gives wider intervals than pointwise", {
  if (!.is_converged(mod)) skip_on_ci()
  skip_if_not_installed("mvtnorm")
  ci_sim <- confint(mod, simultaneous = TRUE)
  est    <- stats::coef(mod)
  se     <- sqrt(diag(stats::vcov(mod)))
  t_crit <- stats::qt(0.975, df = stats::df.residual(mod))
  expect_true(all(ci_sim[, 1] <= est - t_crit * se))
  expect_true(all(ci_sim[, 2] >= est + t_crit * se))
})

test_that("confint(simultaneous = TRUE) respects parm and back_transform", {
  if (!.is_converged(mod)) skip_on_ci()
  skip_if_not_installed("mvtnorm")
  full <- confint(mod, simultaneous = TRUE)
  parm <- rownames(full)[1:2]
  sub  <- confint(mod, parm = parm, simultaneous = TRUE)
  expect_equal(nrow(sub), 2L)
  expect_equal(rownames(sub), parm)

  bt <- confint(mod, simultaneous = TRUE, back_transform = TRUE)
  # log-scale rows are exponentiated; row name loses its "log" prefix
  expect_true(any(grepl("^EC50", rownames(bt))))
  expect_false(any(grepl("^logEC50", rownames(bt))))
})

test_that("confint() falls back to Wald intervals with a warning for sigmoidal models", {
  mod_sig <- emax_nls(
    structural_model = rsp_1 ~ exp_1,
    covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1, logHill ~ 1),
    data             = emax_df
  )
  if (!.is_converged(mod_sig)) skip()
  # profile CI fails for this sigmoidal model; expect a warning and a valid result
  expect_warning(
    ci <- confint(mod_sig),
    "falling back to Wald intervals"
  )
  expect_true(is.matrix(ci))
  expect_equal(nrow(ci), length(coef(mod_sig)))
  expect_true(all(ci[, 1] < ci[, 2]))
})

