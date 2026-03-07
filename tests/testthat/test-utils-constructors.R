str_model <- response_1 ~ exposure_1 
cov_model <- list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1)

test_that(".store() works for basic model", {
  expect_no_error(.store(cov_model, str_model, emax_df))
})
