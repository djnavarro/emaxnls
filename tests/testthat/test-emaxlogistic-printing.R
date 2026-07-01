
# Tests for print and summary methods for emaxlogistic objects

mod <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model  = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data             = emax_df
)


# print() -----------------------------------------------------------------

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
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("E0.*cnt_a", output)))
})

test_that("print() output includes Deviance and AIC", {
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("Deviance", output)))
  expect_true(any(grepl("AIC", output)))
})

test_that("print() uses z_statistic column not t_statistic", {
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("z_statistic", output)))
  expect_false(any(grepl("t_statistic", output)))
})

test_that("print() returns object invisibly", {
  capture.output(expect_invisible(print(mod)))
})

mod_bad <- suppressWarnings(emax_logistic(
  rsp_2 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df,
  opts = emax_logistic_options(optim_control = stats::nls.control(maxiter = 1), quiet = TRUE)
))

test_that("print() handles non-converged models", {
  output <- utils::capture.output(print(mod_bad))
  expect_true(any(grepl("does not converge", output)))
})


# summary() ---------------------------------------------------------------

test_that("summary() returns a data frame", {
  s <- summary(mod)
  expect_s3_class(s, "data.frame")
})

test_that("summary() has the expected columns", {
  s <- summary(mod)
  expect_named(s, c("label", "estimate", "std_error", "z_statistic",
                    "p_value", "ci_lower", "ci_upper"))
})

test_that("summary() has one row per parameter", {
  s <- summary(mod)
  expect_equal(nrow(s), length(coef(mod)))
})

test_that("summary(back_transform = TRUE) transforms log-scaled labels", {
  s <- summary(mod, back_transform = TRUE)
  expect_true("EC50_Intercept" %in% s$label)
  expect_false("logEC50_Intercept" %in% s$label)
})

test_that("summary() returns nls_null for non-converged models", {
  expect_s3_class(summary(mod_bad), "emaxnls_null")
})


# .coef_table_logistic() --------------------------------------------------

test_that(".coef_table_logistic() returns a data frame with expected structure", {
  cc <- .coef_table_logistic(mod)
  expect_s3_class(cc, "data.frame")
  expect_named(cc, c("label", "estimate", "std_error", "z_statistic",
                     "p_value", "ci_lower", "ci_upper"))
  expect_equal(cc$label, .get_coefficient_names(mod))
})

test_that("print() output includes expected section headers", {
  output <- utils::capture.output(print(mod))
  expect_true(any(grepl("^Structural model:$", output)))
  expect_true(any(grepl("^Covariate model:$", output)))
  expect_true(any(grepl("^Coefficient table:$", output)))
})
