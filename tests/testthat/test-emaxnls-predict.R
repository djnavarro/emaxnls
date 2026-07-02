mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

test_that("predict without se.fit, interval, or newdata returns vector", {
  if (!.is_converged(mod)) skip_on_ci()
  pr_vec <- predict(mod)
  expect_equal(length(pr_vec), nrow(emax_df))
  expect_type(pr_vec, "double")
  fitted <- c(mod$env$model$m$fitted())
  expect_equal(pr_vec, fitted)
})

test_that("predict with se.fit returns list", {
  if (!.is_converged(mod)) skip_on_ci()
  pr_vec <- predict(mod)
  pr_lst <- predict(mod, se.fit = TRUE)
  expect_type(pr_lst, "list")
  expect_named(pr_lst, c("fit", "se.fit", "residual.scale", "df"))
  expect_equal(pr_lst$fit, pr_vec)
  expect_true(is.numeric(pr_lst$se))
  expect_true(is.numeric(pr_lst$df))
  expect_length(pr_lst$df, 1L)
  expect_equal(length(pr_lst$se), nrow(emax_df))
})

test_that("predict with interval but not se.fit returns data.frame", {
  if (!.is_converged(mod)) skip_on_ci()
  pr_vec <- predict(mod)
  pr_int <- predict(mod, interval = "confidence")
  expect_s3_class(pr_int, "data.frame")
  expect_named(pr_int, c("fit", "lwr", "upr"))
  expect_true(is.numeric(pr_int$fit))
  expect_true(is.numeric(pr_int$lwr))
  expect_true(is.numeric(pr_int$upr))
  expect_equal(nrow(pr_int), nrow(emax_df))
  expect_equal(pr_int$fit, pr_vec)
})

test_that("predict with newdata produces expected values", {
  if (!.is_converged(mod)) skip_on_ci()
  ii <- 120:125
  nd <- data.frame(
    exp_1 = emax_df$exp_1[ii], 
    cnt_a = emax_df$cnt_a[ii]
  )

  pr_vec    <- predict(mod)
  pr_vec_nd <- predict(mod, newdata = nd)
  expect_equal(pr_vec_nd, pr_vec[ii])
  expect_equal(length(pr_vec_nd), length(ii))

  pr_lst    <- predict(mod, se.fit = TRUE)
  pr_lst_nd <- predict(mod, newdata = nd, se.fit = TRUE)
  expect_equal(pr_lst_nd$fit, pr_lst$fit[ii])
  expect_equal(pr_lst_nd$se, pr_lst$se[ii], tolerance = .00001)
  expect_equal(pr_lst_nd$df, pr_lst$df)

  pr_int    <- predict(mod, interval = "confidence")
  pr_int_nd <- predict(mod, newdata = nd, interval = "confidence")
  expect_equal(pr_int_nd$fit, pr_int$fit[ii], tolerance = .00001)
  expect_equal(pr_int_nd$lwr, pr_int$lwr[ii], tolerance = .00001)
  expect_equal(pr_int_nd$upr, pr_int$upr[ii], tolerance = .00001)
})

test_that("predict with tibble newdata gives identical results to data.frame", {
  if (!.is_converged(mod)) skip_on_ci()
  nd_df  <- data.frame(exp_1 = emax_df$exp_1[120:125], cnt_a = emax_df$cnt_a[120:125])
  nd_tbl <- tibble::as_tibble(nd_df)

  # plain vector: tibble must not silently return a list
  p_df  <- predict(mod, newdata = nd_df)
  p_tbl <- predict(mod, newdata = nd_tbl)
  expect_type(p_tbl, "double")
  expect_equal(p_tbl, p_df)

  # se.fit: $fit must be a numeric vector, not a list
  ps_df  <- predict(mod, newdata = nd_df, se.fit = TRUE)
  ps_tbl <- predict(mod, newdata = nd_tbl, se.fit = TRUE)
  expect_type(ps_tbl$fit, "double")
  expect_equal(ps_tbl, ps_df)

  # interval: must not error with "non-numeric argument to binary operator"
  pi_df  <- predict(mod, newdata = nd_df, interval = "confidence")
  pi_tbl <- predict(mod, newdata = nd_tbl, interval = "confidence")
  expect_s3_class(pi_tbl, "data.frame")
  expect_equal(pi_tbl, pi_df)
})

# set up for a simpler nls model based loosely on SSlogis
old_dat <- data.frame(
  conc = c(0.04, 0.04, 0.19, 0.19, 0.39, 0.39, 0.78, 0.78, 1.56, 1.56),
  dens = c(0.02, 0.02, 0.12, 0.13, 0.21, 0.22, 0.38, 0.37, 0.61, 0.61)
)
new_dat <- data.frame(conc = c(0.1, 0.2, 0.3))
mod <- stats::nls(
  formula = dens ~ asym / (1.0 + exp((xmid - conc))),
  start = c(asym = 100.0, xmid = 6.0),
  data = old_dat
)

test_that(".predict_nls matches default predict method for nls objects", {
  if (!.is_converged(mod)) skip_on_ci()
  
  pred_nls <- c(predict(mod, newdata = new_dat))
  pred_new <- .predict_nls(mod, newdata = new_dat)
  expect_equal(pred_new, pred_nls)

})

# results for xgxr::predict.nls(mod, new_dat, se.fit = TRUE)
xgx_1 <- list(
  fit = c(0.1452964, 0.1605549, 0.1774132),
  se.fit = c(0.02331904, 0.02490512, 0.02647756),
  residual.scale = 0.07637093,
  df = 8L
)

# results for xgxr::predict.nls(mod, new_dat, se.fit = TRUE, interval = "confidence")
xgx_2 <- list(
  fit = .tibble(
    fit = c(0.1452964, 0.1605549, 0.1774132),
    lwr = c(0.09152257, 0.10312361, 0.11635589),
    upr = c(0.1990702, 0.2179862, 0.2384706)
  ),
  se.fit = c(0.02331904, 0.02490512, 0.02647756),
  residual.scale = 0.07637093,
  df = 8L
)

test_that(".predict_nls matches xgxr::predict.nls", {
  if (!.is_converged(mod)) skip_on_ci()
  
  prd_1 <- .predict_nls(mod, newdata = new_dat, se.fit = TRUE)
  prd_2 <- .predict_nls(mod, newdata = new_dat, se.fit = TRUE, interval = "confidence")
  expect_equal(prd_1, xgx_1, tolerance = .00001)
  expect_equal(prd_2, xgx_2, tolerance = .00001)

})
