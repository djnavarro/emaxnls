
# Tests for S3 methods on emaxlogistic objects

mod_base <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df
)

mod_cov <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model  = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df
)


# coef() ------------------------------------------------------------------

test_that("coef() returns named numeric vector", {
  cc <- coef(mod_base)
  expect_true(is.numeric(cc))
  expect_named(cc)
  expect_length(cc, 3L)
})

test_that("coef(back_transform = TRUE) transforms log-scale parameters", {
  cc_raw   <- coef(mod_base)
  cc_trans <- coef(mod_base, back_transform = TRUE)
  expect_equal(cc_trans["EC50_Intercept"], exp(cc_raw["logEC50_Intercept"]),
               ignore_attr = TRUE)
})


# fitted() ----------------------------------------------------------------

test_that("fitted() returns probabilities by default", {
  f <- fitted(mod_base)
  expect_true(is.numeric(f))
  expect_length(f, nrow(emax_df))
  expect_true(all(f > 0 & f < 1))
})

test_that("fitted(type = 'link') returns logit-scale predictions", {
  f_link <- fitted(mod_base, type = "link")
  f_resp <- fitted(mod_base, type = "response")
  expect_equal(f_link, .logit(f_resp))
})


# residuals() -------------------------------------------------------------

test_that("residuals() returns Pearson residuals by default", {
  r <- residuals(mod_base)
  expect_true(is.numeric(r))
  expect_length(r, nrow(emax_df))
})

test_that("residuals(type = 'deviance') returns deviance residuals", {
  r_dev  <- residuals(mod_base, type = "deviance")
  r_pear <- residuals(mod_base, type = "pearson")
  # deviance and Pearson residuals should differ
  expect_false(isTRUE(all.equal(r_dev, r_pear)))
})

test_that("deviance residuals have correct sign", {
  y <- emax_df$rsp_2
  r <- residuals(mod_base, type = "deviance")
  # positive residuals where y=1, negative where y=0 (unless perfectly predicted)
  pos_obs <- which(y == 1 & fitted(mod_base) < 0.5)
  neg_obs <- which(y == 0 & fitted(mod_base) > 0.5)
  if (length(pos_obs) > 0) expect_true(all(r[pos_obs] > 0))
  if (length(neg_obs) > 0) expect_true(all(r[neg_obs] < 0))
})


# logLik() ----------------------------------------------------------------

test_that("logLik() returns a logLik object", {
  ll <- logLik(mod_base)
  expect_s3_class(ll, "logLik")
  expect_true(is.finite(as.numeric(ll)))
})

test_that("logLik() has correct df attribute", {
  ll <- logLik(mod_base)
  expect_equal(attr(ll, "df"), length(coef(mod_base)))
})

test_that("logLik() equals hand-computed binomial log-likelihood", {
  y  <- emax_df$rsp_2
  mu <- fitted(mod_base)
  expected <- sum(y * log(mu) + (1 - y) * log(1 - mu))
  expect_equal(as.numeric(logLik(mod_base)), expected, tolerance = 1e-8)
})


# deviance() --------------------------------------------------------------

test_that("deviance() equals -2 * logLik()", {
  expect_equal(deviance(mod_base), -2 * as.numeric(logLik(mod_base)), tolerance = 1e-8)
})


# AIC() and BIC() ---------------------------------------------------------

test_that("AIC() returns a numeric scalar", {
  expect_true(is.numeric(AIC(mod_base)))
  expect_length(AIC(mod_base), 1L)
})

test_that("BIC() returns a numeric scalar", {
  expect_true(is.numeric(BIC(mod_base)))
  expect_length(BIC(mod_base), 1L)
})

test_that("AIC() equals -2*logLik + 2*npar", {
  ll   <- logLik(mod_base)
  npar <- attr(ll, "df")
  expected <- -2 * as.numeric(ll) + 2 * npar
  expect_equal(AIC(mod_base), expected, tolerance = 1e-8)
})

test_that("model with covariate has lower AIC than base model", {
  expect_lt(AIC(mod_cov), AIC(mod_base))
})

test_that("AIC(mod1, mod2) returns a data frame", {
  out <- AIC(mod_base, mod_cov)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("df", "AIC"))
  expect_equal(nrow(out), 2L)
})

test_that("BIC(mod1, mod2) returns a data frame", {
  out <- BIC(mod_base, mod_cov)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("df", "BIC"))
})


# nobs() and df.residual() ------------------------------------------------

test_that("nobs() returns the number of observations", {
  expect_equal(nobs(mod_base), nrow(emax_df))
})

test_that("df.residual() is nobs - npar", {
  n    <- nobs(mod_base)
  npar <- length(coef(mod_base))
  expect_equal(df.residual(mod_base), n - npar)
})


# anova() -----------------------------------------------------------------

test_that("anova() performs LRT and returns correct structure", {
  a <- anova(mod_base, mod_cov)
  expect_s3_class(a, "data.frame")
  expect_true("Pr(>Chi)" %in% names(a))
  expect_true("LRT" %in% names(a))
  expect_true("Df_diff" %in% names(a))
})

test_that("anova() LRT p-value is significant when covariate improves fit", {
  a <- anova(mod_base, mod_cov)
  expect_lt(a$`Pr(>Chi)`[2], 0.05)
})

test_that("anova() LRT statistic equals deviance difference", {
  a <- anova(mod_base, mod_cov)
  expected_lrt <- deviance(mod_base) - deviance(mod_cov)
  expect_equal(a$LRT[2], expected_lrt, tolerance = 1e-8)
})


# predict() ---------------------------------------------------------------

test_that("predict() returns probabilities by default", {
  p <- predict(mod_base)
  expect_true(is.numeric(p))
  expect_true(all(p > 0 & p < 1))
  expect_length(p, nrow(emax_df))
})

test_that("predict() and fitted() agree on training data", {
  expect_equal(predict(mod_base), fitted(mod_base), tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("predict(type = 'link') returns logit-scale predictions", {
  p_link <- predict(mod_base, type = "link")
  p_resp <- predict(mod_base, type = "response")
  expect_equal(p_link, .logit(p_resp), tolerance = 1e-8)
})

test_that("predict() works with newdata", {
  new_dat <- emax_df[1:10, ]
  p <- predict(mod_base, newdata = new_dat)
  expect_length(p, 10L)
  expect_true(all(p > 0 & p < 1))
})


# vcov() and confint() ----------------------------------------------------

test_that("vcov() returns a square matrix", {
  v <- vcov(mod_base)
  n <- length(coef(mod_base))
  expect_equal(dim(v), c(n, n))
})

test_that("confint() returns a matrix with correct dimensions", {
  ci <- confint(mod_base)
  expect_equal(nrow(ci), length(coef(mod_base)))
  expect_equal(ncol(ci), 2L)
})

test_that("confint(back_transform = TRUE) transforms logEC50 to EC50 scale", {
  ci_raw   <- confint(mod_base)
  ci_trans <- confint(mod_base, back_transform = TRUE)
  expect_equal(ci_trans["EC50_Intercept", ], exp(ci_raw["logEC50_Intercept", ]),
               ignore_attr = TRUE)
})


# non-converged models ----------------------------------------------------

mod_bad <- suppressWarnings(emax_logistic(
  rsp_2 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df,
  opts = emax_logistic_options(optim_control = stats::nls.control(maxiter = 1), quiet = TRUE)
))

test_that("methods return null-like values for non-converged models", {
  expect_s3_class(logLik(mod_bad), "emaxnls_null")
  expect_s3_class(fitted(mod_bad), "emaxnls_null")
  expect_s3_class(residuals(mod_bad), "emaxnls_null")
  expect_s3_class(deviance(mod_bad), "emaxnls_null")
})


# emax_add_term / emax_remove_term ----------------------------------------

test_that("emax_add_term() preserves emaxlogistic class", {
  mod_updated <- emax_add_term(mod_base, E0 ~ cnt_a)
  expect_s3_class(mod_updated, "emaxlogistic")
  expect_s3_class(mod_updated, "emaxnls")
  expect_true(emax_converged(mod_updated))
})

test_that("emax_remove_term() preserves emaxlogistic class", {
  mod_updated <- emax_remove_term(mod_cov, E0 ~ cnt_a)
  expect_s3_class(mod_updated, "emaxlogistic")
  expect_s3_class(mod_updated, "emaxnls")
  expect_true(emax_converged(mod_updated))
})
