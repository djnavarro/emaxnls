
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
lbl <- .get_coefficient_names(mod)

mod_sig <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1, logHill ~ 1),
  data = emax_df
)


# print() -----------------------------------------------------------------

test_that("print() writes expected section headers and returns object invisibly", {
  if (!.is_converged(mod)) skip()
  con <- textConnection("text_connection", "w")
  sink(con)
  val <- print(mod)
  sink()
  msg <- textConnectionValue(con)
  close(con)
  expect_equal(val, mod)
  expect_true(any(grepl("^Structural model:$", msg)))
  expect_true(any(grepl("^Covariate model:$", msg)))
  expect_true(any(grepl("^Model fit:$", msg)))
  expect_true(any(grepl("^Coefficients", msg)))
})

test_that("print() does not include test statistics or p-values", {
  if (!.is_converged(mod)) skip()
  output <- utils::capture.output(print(mod))
  expect_false(any(grepl("t_statistic", output)))
  expect_false(any(grepl("p_value", output)))
})

test_that("print() includes fit statistics", {
  if (!.is_converged(mod)) skip()
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("Observations", output)))
  expect_true(any(grepl("Residual", output)))
  expect_true(any(grepl("AIC", output)))
})

test_that("print() directs user to summary() for hypothesis tests", {
  if (!.is_converged(mod)) skip()
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("summary\\(\\)", output)))
})


# .coef_table() -----------------------------------------------------------

test_that(".coef_table() returns a data frame with the expected structure", {
  if (!.is_converged(mod)) skip()
  cc <- .coef_table(mod)
  expect_s3_class(cc, class = "data.frame")
  expect_named(cc, c("label", "estimate", "std_error", "t_statistic", "p_value",
                     "ci_lower", "ci_upper"))
  expect_equal(cc$label, lbl)
})


# summary(): suppress_nonsensical -----------------------------------------

test_that("summary() suppresses logEC50_Intercept test statistic and p-value by default", {
  if (!.is_converged(mod)) skip()
  s <- summary(mod)
  ec50_row <- s[s$label == "logEC50_Intercept", ]
  expect_true(is.na(ec50_row$t_statistic))
  expect_true(is.na(ec50_row$p_value))
})

test_that("summary() still reports CI for logEC50_Intercept when suppressed", {
  if (!.is_converged(mod)) skip()
  s <- summary(mod)
  ec50_row <- s[s$label == "logEC50_Intercept", ]
  expect_false(is.na(ec50_row$ci_lower))
  expect_false(is.na(ec50_row$ci_upper))
})

test_that("summary() restores logEC50_Intercept test when suppress_nonsensical = FALSE", {
  if (!.is_converged(mod)) skip()
  s <- summary(mod, suppress_nonsensical = FALSE)
  ec50_row <- s[s$label == "logEC50_Intercept", ]
  expect_false(is.na(ec50_row$t_statistic))
  expect_false(is.na(ec50_row$p_value))
})


# summary(): p_adjust -----------------------------------------------------

test_that("summary() adjusts p-values when p_adjust is set", {
  if (!.is_converged(mod)) skip()
  s_none <- summary(mod, suppress_nonsensical = FALSE)
  s_bonf <- summary(mod, suppress_nonsensical = FALSE, p_adjust = "bonferroni")
  expect_true(all(s_bonf$p_value >= s_none$p_value))
})

test_that("summary() p_adjust excludes suppressed p-values from adjustment set", {
  if (!.is_converged(mod)) skip()
  s_none <- summary(mod, p_adjust = "bonferroni")
  # logEC50_Intercept p-value should remain NA even after adjustment
  ec50_row <- s_none[s_none$label == "logEC50_Intercept", ]
  expect_true(is.na(ec50_row$p_value))
})


# summary(): simultaneous -------------------------------------------------

test_that("summary() simultaneous CIs are wider than pointwise Wald CIs", {
  if (!.is_converged(mod)) skip()
  skip_if_not(requireNamespace("mvtnorm", quietly = TRUE), "mvtnorm not available")
  # Both are Wald-based, so the comparison is valid: simultaneous uses a
  # larger critical value from the joint MVN than the pointwise t-quantile.
  s_sim  <- summary(mod, simultaneous = TRUE)
  est    <- stats::coef(mod)
  se     <- sqrt(diag(stats::vcov(mod)))
  t_crit <- stats::qt(0.975, df = stats::df.residual(mod))
  pt_lower <- est - t_crit * se
  pt_upper <- est + t_crit * se
  expect_true(all(s_sim$ci_lower <= pt_lower))
  expect_true(all(s_sim$ci_upper >= pt_upper))
})


# sigmoidal model ---------------------------------------------------------

test_that("sigmoidal: logHill_Intercept is tested and logEC50_Intercept is suppressed", {
  if (!.is_converged(mod_sig)) skip()
  s <- suppressWarnings(summary(mod_sig))

  hill_row <- s[s$label == "logHill_Intercept", ]
  ec50_row <- s[s$label == "logEC50_Intercept", ]

  # logHill_Intercept: H0: logHill = 0 <=> Hill = 1 (hyperbolic model) is meaningful
  expect_false(is.na(hill_row$t_statistic))
  expect_false(is.na(hill_row$p_value))

  # logEC50_Intercept: H0: logEC50 = 0 <=> EC50 = 1 on the concentration scale
  # has no pharmacometric meaning; test suppressed, CI retained
  expect_true(is.na(ec50_row$t_statistic))
  expect_true(is.na(ec50_row$p_value))
  expect_false(is.na(ec50_row$ci_lower))
  expect_false(is.na(ec50_row$ci_upper))
})


# .show_p() ---------------------------------------------------------------

test_that(".show_p() works", {
  expect_error(.show_p("not_a_number"))
  expect_error(.show_p(NULL))
  expect_no_error(.show_p(.234))
  expect_no_error(.show_p(c(1002, .234)))
  expect_equal(.show_p(.123423), "0.123")
  expect_equal(.show_p(.000001), "<0.001")
  expect_equal(.show_p(.123423, accuracy = .01), "0.12")
})
