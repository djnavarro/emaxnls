test_that("emax_df matches internal function", {
  # some rhub architectures fail the simulation step,
  # do not test in that case
  sim_df <- tryCatch(
    expr = .simulate_emax_data(seed = 123L), 
    error = function(e) NULL
  )
  skip_if(is.null(sim_df))
  expect_equal(emax_df, sim_df)
})
