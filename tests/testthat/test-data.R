test_that("emax_df matches internal function", {
  # some rhub architectures fail the simulation step,
  # do not test in that case
  sim_df <- tryCatch(
    expr = .simulate_emax_data(seed = 123L), 
    error = function(e) NULL
  )
  skip_if(is.null(sim_df), "Skip when .simulate_emax_data fails")
  expect_true(inherits(sim_df, "data.frame"))
  expect_equal(names(emax_df), names(sim_df))
  for (nm in names(emax_df)) {
    expect_equal(emax_df[[nm]], sim_df[[nm]])
  }
})
