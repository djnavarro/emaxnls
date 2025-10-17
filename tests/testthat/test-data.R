test_that("emax_df matches internal function", {
  expect_equal(emax_df, .simulate_emax_data(seed = 123))
})
