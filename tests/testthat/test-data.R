test_that("emax_df matches internal function", {
  skip_on_ci()
  expect_equal(emax_df, .simulate_emax_data(seed = 123L))
})
