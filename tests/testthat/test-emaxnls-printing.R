
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

