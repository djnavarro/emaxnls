# simple tests of the validators

test_that(".assert behaves as expected", {
  expect_error(.assert(FALSE), class = "emax_nls_error")
  expect_no_error(.assert(TRUE))
})

test_that(".validate_structural_formula behaves as expected", {
  # bad
  expect_error(.validate_structural_formula("not a formula"), class = "emax_nls_error")
  expect_error(.validate_structural_formula(~one_sided), class = "emax_nls_error")
  expect_error(.validate_structural_formula(two_sided ~ but_has + two_vars), class = "emax_nls_error")
  expect_error(.validate_structural_formula(two_sided ~ one_var, names = c("but_names", "are_missing")), class = "emax_nls_error")
  # good
  expect_no_error(.validate_structural_formula(response ~ exposure))
  expect_no_error(.validate_structural_formula(response ~ exposure, names = c("response", "exposure")))
})

test_that(".validate_covariate_formula behaves as expected", {
  # bad
  expect_error(.validate_covariate_formula(this_is_not ~ a_list, class = "emax_nls_error"))
  expect_error(.validate_covariate_formula(list(this = 1, list = 2, is = 3, too = 4, long = 5), class = "emax_nls_error"))
  expect_error(.validate_covariate_formula(list(bad = 1, LHS = 2, names = 3)), class = "emax_nls_error")
  expect_error(.validate_covariate_formula(list(E0 = "not", Emax = "formula", logEC50 = "elements")), class = "emax_nls_error")
  expect_error(.validate_covariate_formula(list(E0 ~ names, Emax ~ are + not, logEC50 ~ known), names = c("cov", "names")), class = "emax_nls_error")
  # good
  expect_no_error(.validate_covariate_formula(list(E0 ~ cov, Emax ~ 1, logEC50 ~ 1)))  
  expect_no_error(.validate_covariate_formula(list(E0 ~ cov, Emax ~ 1, logEC50 ~ 1, logHill ~ 1)))  
  expect_no_error(.validate_covariate_formula(list(E0 ~ cov, Emax ~ 1, logEC50 ~ 1, logHill ~ 1), names = c("cov", "names")))  
})

test_that(".validate_candidate_list behaves as expected", {
  # bad
  expect_error(.validate_candidate_list(this_is_not ~ a_list, names = c("cov1", "cov2")), class = "emax_nls_error")
  expect_error(.validate_candidate_list(list(this = 1, list = 2, is = 3, too = 4, long = 5), names = c("cov1", "cov2")), class = "emax_nls_error")
  expect_error(.validate_candidate_list(list(bad = 1, LHS = 2, names = 3), names = c("cov1", "cov2")), class = "emax_nls_error")
  expect_error(.validate_candidate_list(list(E0 = "not", Emax = "known", logEC50 = "names"), names = c("cov1", "cov2")), class = "emax_nls_error")
  # good
  expect_no_error(.validate_candidate_list(
    candidates = list(E0 = c("cov1", "cov2"), logEC50 = "cov1"), 
    names = c("cov1", "cov2")
  ))
  expect_no_error(.validate_candidate_list(
    candidates = list(E0 = c("cov1", "cov2"), Emax = "cov1", logEC50 = "cov2", logHill = "cov1"), 
    names = c("cov1", "cov2")
  ))
})
