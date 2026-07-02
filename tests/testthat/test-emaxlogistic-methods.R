
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

test_that("emax_add_term() updates the covariate model and parameter count", {
  mod_added <- emax_add_term(mod_base, E0 ~ cnt_a)
  expect_true("E0_cnt_a" %in% names(coef(mod_added)))
  expect_equal(length(coef(mod_added)), length(coef(mod_base)) + 1L)
  expect_true("cnt_a" %in% all.vars(.get_covariate_formula(mod_added, "E0")))
})

test_that("emax_remove_term() updates the covariate model and parameter count", {
  mod_removed <- emax_remove_term(mod_cov, E0 ~ cnt_a)
  expect_false("E0_cnt_a" %in% names(coef(mod_removed)))
  expect_equal(length(coef(mod_removed)), length(coef(mod_cov)) - 1L)
  expect_false("cnt_a" %in% all.vars(.get_covariate_formula(mod_removed, "E0")))
})

test_that("adding and then removing a term leaves the model structure unchanged", {
  mod_add <- emax_add_term(mod_base, E0 ~ bin_d)
  mod_del <- emax_remove_term(mod_add, E0 ~ bin_d)
  expect_equal(names(coef(mod_del)), names(coef(mod_base)))
  expect_equal(
    .get_covariate_formula(mod_del),
    .get_covariate_formula(mod_base),
    ignore_attr = TRUE
  )
})

test_that("emax_add_term() messages when term already exists", {
  expect_message(emax_add_term(mod_cov, E0 ~ cnt_a), class = "emaxnls_message")
})

test_that("emax_remove_term() messages when term is not present", {
  expect_message(emax_remove_term(mod_base, E0 ~ cnt_a), class = "emaxnls_message")
})


# smoke test --------------------------------------------------------------

test_that("methods do not throw errors with basic use", {
  skip_if_not(requireNamespace("mvtnorm", quietly = TRUE), "mvtnorm not installed")
  expect_no_error(coef(mod_cov))
  expect_no_error(vcov(mod_cov))
  expect_no_error(residuals(mod_cov))
  expect_no_error(fitted(mod_cov))
  expect_no_error(capture.output(print(mod_cov)))
  expect_no_error(simulate(mod_cov))
  expect_no_error(logLik(mod_cov))
  expect_no_error(AIC(mod_cov))
  expect_no_error(BIC(mod_cov))
  expect_no_error(predict(mod_cov))
  expect_no_error(confint(mod_cov))
  expect_no_error(nobs(mod_cov))
  expect_no_error(deviance(mod_cov))
  expect_no_error(df.residual(mod_cov))
  expect_no_error(anova(mod_base, mod_cov))
})

test_that("AIC(), BIC(), and anova() can take multiple objects", {
  expect_no_error(AIC(mod_base, mod_cov))
  expect_no_error(BIC(mod_base, mod_cov))
  expect_no_error(anova(mod_base, mod_cov))
})

test_that("AIC(), BIC(), and anova() handle cases where some models do not converge", {
  expect_warning(AIC(mod_base, mod_cov, mod_bad))
  expect_warning(BIC(mod_base, mod_cov, mod_bad))
  expect_warning(anova(mod_base, mod_cov, mod_bad))

  expect_s3_class(suppressWarnings(AIC(mod_base, mod_cov, mod_bad)), "data.frame")
  expect_s3_class(suppressWarnings(BIC(mod_base, mod_cov, mod_bad)), "data.frame")
  expect_s3_class(suppressWarnings(anova(mod_base, mod_cov, mod_bad)), "data.frame")
})


# summary() ---------------------------------------------------------------

test_that("summary() matches .coef_table_logistic()", {
  expect_equal(summary(mod_base), .coef_table_logistic(mod_base, suppress_nonsensical = TRUE))
  expect_equal(summary(mod_base, conf_level = .99), .coef_table_logistic(mod_base, level = .99, suppress_nonsensical = TRUE))
  expect_equal(
    summary(mod_base, back_transform = TRUE),
    .coef_table_logistic(mod_base, back_transform = TRUE, suppress_nonsensical = TRUE)
  )
})


# vcov() ------------------------------------------------------------------

test_that("vcov() is symmetric and has correct dimension names", {
  v   <- vcov(mod_base)
  lbl <- names(coef(mod_base))
  expect_equal(rownames(v), lbl)
  expect_equal(colnames(v), lbl)
  expect_equal(v, t(v))
})


# simulate() --------------------------------------------------------------

test_that("simulate() returns a data frame with one row per observation", {
  skip_if_not(requireNamespace("mvtnorm", quietly = TRUE), "mvtnorm not installed")
  sim <- simulate(mod_base)
  expect_s3_class(sim, "data.frame")
  expect_equal(nrow(sim), nrow(emax_df))  # nsim = 1: long format gives n * 1 rows
})

test_that("simulate() respects the nsim argument", {
  skip_if_not(requireNamespace("mvtnorm", quietly = TRUE), "mvtnorm not installed")
  sim <- simulate(mod_base, nsim = 5L)
  # long format: one row per observation per simulation replicate
  expect_equal(nrow(sim), 5L * nrow(emax_df))
  expect_equal(sort(unique(sim$sim_id)), 1:5)
})

test_that("simulate() produces binary values in the val column", {
  skip_if_not(requireNamespace("mvtnorm", quietly = TRUE), "mvtnorm not installed")
  sim <- simulate(mod_base)
  expect_true(all(sim$val %in% c(0, 1)))
})


# predict() with se.fit and interval --------------------------------------

test_that("predict(se.fit = TRUE) returns a list with fit, se.fit, residual.scale, and df", {
  pr <- predict(mod_base, se.fit = TRUE)
  expect_type(pr, "list")
  expect_named(pr, c("fit", "se.fit", "residual.scale", "df"))
  expect_true(all(pr$fit > 0 & pr$fit < 1))
  expect_true(is.numeric(pr$se.fit))
  expect_length(pr$df, 1L)
  expect_equal(length(pr$se.fit), nrow(emax_df))
})

test_that("predict(interval = 'confidence') returns a data frame of probabilities", {
  pr <- predict(mod_base, interval = "confidence")
  expect_s3_class(pr, "data.frame")
  expect_named(pr, c("fit", "lwr", "upr"))
  expect_equal(nrow(pr), nrow(emax_df))
  expect_equal(pr$fit, predict(mod_base), tolerance = 1e-8, ignore_attr = TRUE)
  # all columns should be on the probability scale after expit transformation
  expect_true(all(pr$fit > 0 & pr$fit < 1))
  expect_true(all(pr$lwr > 0 & pr$lwr < 1))
  expect_true(all(pr$upr > 0 & pr$upr < 1))
  expect_true(all(pr$lwr <= pr$fit))
  expect_true(all(pr$fit <= pr$upr))
})

test_that("predict(se.fit = TRUE) fit matches predict() on response scale", {
  pr_vec <- predict(mod_base)
  pr_lst <- predict(mod_base, se.fit = TRUE)
  expect_equal(pr_lst$fit, pr_vec, tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("predict with tibble newdata gives identical results to data.frame", {
  nd_df  <- data.frame(exp_1 = emax_df$exp_1[1:5], cnt_a = emax_df$cnt_a[1:5])
  nd_tbl <- tibble::as_tibble(nd_df)

  p_df  <- predict(mod_cov, newdata = nd_df)
  p_tbl <- predict(mod_cov, newdata = nd_tbl)
  expect_type(p_tbl, "double")
  expect_equal(p_tbl, p_df)
})

test_that("confint() falls back to Wald intervals with a warning for sigmoidal models", {
  mod_sig <- emax_logistic(
    structural_model = rsp_2 ~ exp_1,
    covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1, logHill ~ 1),
    data             = emax_df
  )
  if (!.is_converged(mod_sig)) skip()
  # profile CI fails for this sigmoidal logistic model; expect a warning and a valid result
  expect_warning(
    ci <- confint(mod_sig),
    "falling back to Wald intervals"
  )
  expect_true(is.matrix(ci))
  expect_equal(nrow(ci), length(coef(mod_sig)))
  expect_true(all(ci[, 1] < ci[, 2]))
})
