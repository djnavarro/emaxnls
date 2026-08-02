mod_0 <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df,
  opts = test_nls_opts()
) 
mod_1 <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df,
  opts = test_nls_opts()
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
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")
  skip_if(!.is_converged(mod_1), "Skip if convergence fails on this architecture")

    expect_no_error(.emax_once_forward(mod_0, cov_list, threshold = .01))
    expect_no_error(.emax_once_backward(mod_1, cov_list, threshold = .001))
})

test_that(".emax_once_forward and .emax_once_backward select the expected terms", {
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")
  skip_if(!.is_converged(mod_1), "Skip if convergence fails on this architecture")

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
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")

  fwd <- .emax_scm_forward(mod = mod_0, candidates = cov_list_big, threshold = .01)
  skip_if_not_converged(fwd)
  bck <- .emax_scm_backward(mod = fwd, candidates = cov_list_big, threshold = .001)
  skip_if_not_converged(bck)
  expect_equal(sort(.get_coefficient_names(bck)), sort(.get_coefficient_names(mod_1))) # should find the E0 ~ cnt_a term only
})

test_that("scm stores history in mod$info", {
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")
  
  expect_true(is.null(mod_0$info$history))

  fwd <- .emax_scm_forward(mod = mod_0, candidates = cov_list_big, threshold = .01)
  skip_if_not_converged(fwd)
  expect_true(!is.null(fwd$info$history))
  h_fwd <- fwd$info$history
  expect_true(inherits(h_fwd, "data.frame"))

  bck <- .emax_scm_backward(mod = fwd, candidates = cov_list_big, threshold = .001)
  skip_if_not_converged(bck)
  expect_true(!is.null(bck$info$history))
  h_bck <- bck$info$history
  expect_true(inherits(h_bck, "data.frame"))
  expect_equal(.filter(h_bck, step != "backward"), h_fwd)
})

test_that("scm history has a criterion column with correct values", {
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")

  fwd <- .emax_scm_forward(mod = mod_0, candidates = cov_list, threshold = .05)
  h <- fwd$info$history
  expect_true("criterion" %in% names(h))

  # base model row has NA criterion
  expect_true(is.na(h$criterion[h$step == "base model"]))

  # forward rows have the correct criterion label
  expect_true(all(h$criterion[h$step == "forward"] == "p-value"))
})

test_that("invalid criterion is rejected", {
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")

  expect_error(
    .emax_scm_forward(mod_0, cov_list, threshold = .01, criterion = "likelihood"),
    regexp = "criterion"
  )
  expect_error(
    .emax_scm_backward(mod_1, cov_list, threshold = .001, criterion = "bic2"),
    regexp = "criterion"
  )
})

test_that("aic criterion adds expected term in .emax_once_forward", {
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")
  skip_if(!.is_converged(mod_1), "Skip if convergence fails on this architecture")

  # cnt_a is a real predictor so adding E0 ~ cnt_a should reduce AIC
  fwd_aic <- .emax_once_forward(mod_0, cov_list, threshold = .01, criterion = "aic")
  expect_equal(sort(.get_coefficient_names(fwd_aic)), sort(.get_coefficient_names(mod_1)))
})

test_that("bic criterion adds expected term in .emax_once_forward", {
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")
  skip_if(!.is_converged(mod_1), "Skip if convergence fails on this architecture")

  fwd_bic <- .emax_once_forward(mod_0, cov_list, threshold = .01, criterion = "bic")
  expect_equal(sort(.get_coefficient_names(fwd_bic)), sort(.get_coefficient_names(mod_1)))
})

test_that("aic and bic criteria record correct criterion label in history", {
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")
  skip_if(!.is_converged(mod_1), "Skip if convergence fails on this architecture")

  fwd_aic <- .emax_once_forward(mod_0, cov_list, threshold = .01, criterion = "aic")
  h_aic <- fwd_aic$info$history
  expect_true(all(h_aic$criterion[h_aic$step == "forward"] == "aic"))

  bck_bic <- .emax_once_backward(mod_1, cov_list, threshold = .001, criterion = "bic")
  h_bic <- bck_bic$info$history
  expect_true(all(h_bic$criterion[h_bic$step == "backward"] == "bic"))
})

test_that("aic criterion full forward/backward scm does not error and returns a model", {
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")

  fwd <- .emax_scm_forward(mod = mod_0, candidates = cov_list_big, criterion = "aic", threshold = .01)
  skip_if_not_converged(fwd)
  bck <- .emax_scm_backward(mod = fwd, candidates = cov_list_big, criterion = "aic", threshold = .001)
  skip_if_not_converged(bck)
  expect_true(.is_emaxnls(bck))
})

test_that("emax_scm_history criterion column is NA for base and final model rows", {
  skip_if(!.is_converged(mod_0), "Skip if convergence fails on this architecture")

  fwd <- emax_scm_forward(mod = mod_0, candidates = cov_list, threshold = .05)
  skip_if_not_converged(fwd)
  h <- emax_scm_history(fwd)

  expect_true(is.na(h$criterion[h$step == "base model"]))
  expect_true(is.na(h$criterion[h$step == "final model"]))
})
