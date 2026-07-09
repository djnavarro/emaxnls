
xx <- data.frame(a = 1)
yy <- xx
rownames(yy) <- "hi"
yy_named <- data.frame(id = "hi", a = 1)

test_that("tibble used when available", {
  skip_if_not_installed("tibble")
  expect_equal(.tibble(a = 1), tibble::tibble(a = 1))
  expect_equal(.as_tibble(xx), tibble::as_tibble(xx))
  expect_equal(.rownames_to_column(yy), tibble::rownames_to_column(yy))
  expect_equal(.add_row(xx, a = 2), tibble::add_row(xx, a = 2))
})

test_that(".tibble functions are tibble-like when tibble missing", {
  expect_equal(.tibble(a = 1, .no_tibble = TRUE), data.frame(a = 1))
  expect_equal(.as_tibble(xx, .no_tibble = TRUE), xx)
  expect_equal(.rownames_to_column(yy, var = "id", .no_tibble = TRUE), yy_named)
  expect_equal(.add_row(xx, a = 2, .no_tibble = TRUE), data.frame(a = 1:2))
})

test_that(".tibble() errors on cross-column references", {
  # direct call
  expect_error(
    .tibble(a = 1:3, b = a * 2),
    "cross-column references"
  )
  # through ...: must still be caught (regression guard)
  wrapper <- function(...) .tibble(...)
  expect_error(
    wrapper(a = 1:3, b = a * 2),
    "cross-column references"
  )
  # references to outer-scope variables that don't share a column name are fine
  val <- 1:3
  expect_no_error(.tibble(a = val, b = val * 2, .no_tibble = TRUE))
})

