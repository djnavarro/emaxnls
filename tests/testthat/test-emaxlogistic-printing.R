
# Tests for print and summary methods for emaxlogistic objects

mod <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model  = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df
)

mod_sig <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model  = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1, logHill ~ 1),
  data             = emax_df
)

mod_bad <- suppressWarnings(emax_logistic(
  rsp_2 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df,
  opts = emax_logistic_options(optim_control = stats::nls.control(maxiter = 1), quiet = TRUE)
))


# print() -----------------------------------------------------------------

test_that("print() writes expected section headers", {
  skip_if_not_converged(mod)
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("^Structural model:$", output)))
  expect_true(any(grepl("^Covariate model:$", output)))
  expect_true(any(grepl("^Model fit:$", output)))
  expect_true(any(grepl("^Coefficients", output)))
})

test_that("print() output includes response type line", {
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("binary \\(logit link\\)", output)))
})

test_that("print() output includes exposure and response names", {
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("exp_1", output)))
  expect_true(any(grepl("rsp_2", output)))
})

test_that("print() output includes covariate model lines", {
  skip_if_not_converged(mod)
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("E0.*cnt_a", output)))
})

test_that("print() includes fit statistics", {
  skip_if_not_converged(mod)
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("Deviance", output)))
  expect_true(any(grepl("AIC", output)))
  expect_true(any(grepl("Observations", output)))
})

test_that("print() does not include test statistics or p-values", {
  skip_if_not_converged(mod)
  output <- utils::capture.output(print(mod))
  expect_false(any(grepl("z_statistic", output)))
  expect_false(any(grepl("t_statistic", output)))
  expect_false(any(grepl("p_value", output)))
})

test_that("print() directs user to summary() for hypothesis tests", {
  skip_if_not_converged(mod)
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("summary\\(\\)", output)))
})

test_that("print() returns object invisibly", {
  capture.output(expect_invisible(print(mod)))
})

test_that("print() handles non-converged models", {
  output <- utils::capture.output(print(mod_bad))
  expect_true(any(grepl("does not converge", output)))
})


# summary() ---------------------------------------------------------------

test_that("summary() returns a data frame", {
  skip_if_not_converged(mod)
  s <- summary(mod)
  expect_s3_class(s, "data.frame")
})

test_that("summary() has the expected columns", {
  skip_if_not_converged(mod)
  s <- summary(mod)
  expect_named(s, c("label", "estimate", "std_error", "z_statistic",
                    "p_value", "ci_lower", "ci_upper"))
})

test_that("summary() has one row per parameter", {
  skip_if_not_converged(mod)
  s <- summary(mod)
  expect_equal(nrow(s), length(coef(mod)))
})

test_that("summary(back_transform = TRUE) transforms log-scaled labels", {
  skip_if_not_converged(mod)
  s <- summary(mod, back_transform = TRUE)
  expect_true("EC50_Intercept" %in% s$label)
  expect_false("logEC50_Intercept" %in% s$label)
})

test_that("summary() returns nls_null for non-converged models", {
  expect_s3_class(summary(mod_bad), "emaxnls_null")
})


# summary(): suppress_nonsensical -----------------------------------------

test_that("summary() suppresses logEC50_Intercept test statistic and p-value by default", {
  skip_if_not_converged(mod)
  s <- summary(mod)
  ec50_row <- s[s$label == "logEC50_Intercept", ]
  expect_true(is.na(ec50_row$z_statistic))
  expect_true(is.na(ec50_row$p_value))
})

test_that("summary() still reports CI for logEC50_Intercept when suppressed", {
  skip_if_not_converged(mod)
  s <- summary(mod)
  ec50_row <- s[s$label == "logEC50_Intercept", ]
  expect_false(is.na(ec50_row$ci_lower))
  expect_false(is.na(ec50_row$ci_upper))
})

test_that("summary() restores logEC50_Intercept test when suppress_nonsensical = FALSE", {
  skip_if_not_converged(mod)
  s <- summary(mod, suppress_nonsensical = FALSE)
  ec50_row <- s[s$label == "logEC50_Intercept", ]
  expect_false(is.na(ec50_row$z_statistic))
  expect_false(is.na(ec50_row$p_value))
})


# summary(): p_adjust -----------------------------------------------------

test_that("summary() adjusts p-values when p_adjust is set", {
  skip_if_not_converged(mod)
  s_none <- summary(mod, suppress_nonsensical = FALSE)
  s_bonf <- summary(mod, suppress_nonsensical = FALSE, p_adjust = "bonferroni")
  expect_true(all(s_bonf$p_value >= s_none$p_value))
})

test_that("summary() p_adjust excludes suppressed p-values from adjustment set", {
  skip_if_not_converged(mod)
  s <- summary(mod, p_adjust = "bonferroni")
  ec50_row <- s[s$label == "logEC50_Intercept", ]
  expect_true(is.na(ec50_row$p_value))
})


# summary(): simultaneous -------------------------------------------------

test_that("summary() simultaneous CIs are wider than pointwise Wald CIs", {
  skip_if_not_converged(mod)
  # Both are Wald-based, so the comparison is valid: simultaneous uses a
  # larger critical value from the joint MVN than the pointwise z-quantile.
  s_sim <- summary(mod, simultaneous = TRUE)
  est   <- stats::coef(mod)
  se    <- sqrt(diag(stats::vcov(mod)))
  pt_lower <- est - stats::qnorm(0.975) * se
  pt_upper <- est + stats::qnorm(0.975) * se
  expect_true(all(s_sim$ci_lower <= pt_lower))
  expect_true(all(s_sim$ci_upper >= pt_upper))
})


# .coef_table_logistic() --------------------------------------------------

test_that(".coef_table_logistic() returns a data frame with expected structure", {
  skip_if_not_converged(mod)
  cc <- .coef_table_logistic(mod)
  expect_s3_class(cc, "data.frame")
  expect_named(cc, c("label", "estimate", "std_error", "z_statistic",
                     "p_value", "ci_lower", "ci_upper"))
  expect_equal(cc$label, .get_coefficient_names(mod))
})


# sigmoidal model ---------------------------------------------------------

test_that("sigmoidal: logHill_Intercept is tested and logEC50_Intercept is suppressed", {
  skip_if_not_converged(mod_sig)
  s <- suppressWarnings(summary(mod_sig))

  hill_row <- s[s$label == "logHill_Intercept", ]
  ec50_row <- s[s$label == "logEC50_Intercept", ]

  # logHill_Intercept: H0: logHill = 0 <=> Hill = 1 (hyperbolic model) is meaningful
  expect_false(is.na(hill_row$z_statistic))
  expect_false(is.na(hill_row$p_value))

  # logEC50_Intercept: H0: logEC50 = 0 <=> EC50 = 1 on the concentration scale
  # has no pharmacometric meaning; test suppressed, CI retained
  expect_true(is.na(ec50_row$z_statistic))
  expect_true(is.na(ec50_row$p_value))
  expect_false(is.na(ec50_row$ci_lower))
  expect_false(is.na(ec50_row$ci_upper))
})
