
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
lbl <- .get_coefficient_names(mod)

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
})

test_that(".coef_table returns data frame with the expected structure", {
  cc <- .coef_table(mod)
  expect_s3_class(cc, class = "data.frame")
  expect_named(cc, c("label", "estimate", "std_error", "t_statistic", "p_value", "ci_lower", "ci_upper"))
  expect_equal(cc$label, lbl)
})

test_that(".show_p() works", {
  expect_error(.show_p("not_a_number"))
  expect_error(.show_p(NULL))
  expect_no_error(.show_p(.234))
  expect_no_error(.show_p(c(1002, .234)))
  expect_equal(.show_p(.123423), "0.123")
  expect_equal(.show_p(.000001), "<0.001")
  expect_equal(.show_p(.123423, accuracy = .01), "0.12")
})
