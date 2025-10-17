test_that("emax_nls works withtest data", {
  expect_no_error(emax_nls(
    structural_model = response_1 ~ exposure_1, 
    covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
    data = emax_df
  ))
})
