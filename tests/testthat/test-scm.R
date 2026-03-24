mod_0 <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
) 
mod_1 <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
cov_list <- list(
  E0 = c("cnt_a", "bin_d"),
  Emax = c("bin_d")
)
cov_list_big <- list(
  E0 = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e"),
  Emax = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e")
)


test_that("basic use of .emax_once_forward and .emax_once_backward does not error", {
  expect_no_error(.emax_once_forward(mod_0, cov_list, threshold = .01))
  expect_no_error(.emax_once_backward(mod_1, cov_list, threshold = .001))
})

test_that(".emax_once_forward and .emax_once_backward select the expected terms", {
  fwd_mod_0a <- .emax_once_forward(mod_0, cov_list, threshold = .05)  # should add E0 ~ cnt_a
  bck_mod_1a <- .emax_once_backward(mod_1, cov_list, threshold = .05) # should not remove 

  expect_equal(sort(.get_coefficient_names(fwd_mod_0a)), sort(.get_coefficient_names(mod_1)))
  expect_equal(sort(.get_coefficient_names(bck_mod_1a)), sort(.get_coefficient_names(mod_1)))

  fwd_mod_0b <- .emax_once_forward(mod_0, cov_list, threshold = 0)  # should not add_
  bck_mod_1b <- .emax_once_backward(mod_1, cov_list, threshold = 0) # should remove E0 ~ cnt_a 

  expect_equal(sort(fwd_mod_0b$coefficients), sort(mod_0$coefficients))
  expect_equal(sort(bck_mod_1b$coefficients), sort(mod_0$coefficients))

})

test_that("basic use of forward/backward scm works", {
  fwd <- .emax_scm_forward(mod = mod_0, candidates = cov_list_big, threshold = .01)
  bck <- .emax_scm_backward(mod = fwd, candidates = cov_list_big, threshold = .001)
  expect_equal(sort(.get_coefficient_names(bck)), sort(.get_coefficient_names(mod_1))) # should find the E0 ~ cnt_a term only
})

test_that("scm stores history in mod$info", {
  expect_true(is.null(mod_0$info$history))

  fwd <- .emax_scm_forward(mod = mod_0, candidates = cov_list_big, threshold = .01)
  expect_true(!is.null(fwd$info$history))
  h_fwd <- fwd$info$history

  bck <- .emax_scm_backward(mod = fwd, candidates = cov_list_big, threshold = .001)
  expect_true(!is.null(bck$info$history))
  h_bck <- bck$info$history

  expect_true(inherits(h_fwd, "data.frame"))
  expect_true(inherits(h_bck, "data.frame"))

  expect_equal(.filter(h_bck, step != "backward"), h_fwd)
})

