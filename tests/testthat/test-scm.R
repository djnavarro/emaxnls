mod_0 <- emax_nls(
  structural_model = response_1 ~ exposure_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
) 
mod_1 <- emax_nls(
  structural_model = response_1 ~ exposure_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
cov_list <- list(
  E0 = c("cnt_a", "bin_d"),
  Emax = c("bin_d")
)

test_that("basic use of .emax_once_forward and .emax_once_backward does not error", {
  expect_no_error(.emax_once_forward(mod_0, cov_list, quiet = TRUE))
  expect_no_error(.emax_once_backward(mod_1, cov_list, quiet = TRUE))
})

test_that(".emax_once_forward and .emax_once_backward select the expected terms", {
  fwd_mod_0a <- .emax_once_forward(mod_0, cov_list, threshold = .05, quiet = TRUE)  # should add E0 ~ cnt_a
  bck_mod_1a <- .emax_once_backward(mod_1, cov_list, threshold = .05, quiet = TRUE) # should not remove 

  expect_equal(sort(fwd_mod_0a$coefficients), sort(mod_1$coefficients))
  expect_equal(sort(bck_mod_1a$coefficients), sort(mod_1$coefficients))

  fwd_mod_0b <- .emax_once_forward(mod_0, cov_list, threshold = 0, quiet = TRUE)  # should not add_
  bck_mod_1b <- .emax_once_backward(mod_1, cov_list, threshold = 0, quiet = TRUE) # should remove E0 ~ cnt_a 

  expect_equal(sort(fwd_mod_0b$coefficients), sort(mod_0$coefficients))
  expect_equal(sort(bck_mod_1b$coefficients), sort(mod_0$coefficients))

})
