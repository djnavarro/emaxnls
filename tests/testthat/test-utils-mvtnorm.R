# Tests for the internal .rmvnorm() and .qmvnorm() wrappers (utils-mvtnorm.R).
#
# Two layers of behaviour are tested:
#   1. Normal path: when mvtnorm is available the wrappers delegate to it and
#      their output is consistent with direct mvtnorm calls.
#   2. Fallback path: when mvtnorm errors (simulating a .so linking failure)
#      the wrappers issue a warning and return a structurally valid result via
#      the base-R fallback implementations.
#
# All tests in this file require a functional mvtnorm installation (one where
# the shared object actually links successfully).  On platforms where mvtnorm
# is installed but its .so cannot be linked at runtime, every test is skipped
# via mvtnorm_usable() (defined in helper-platform.R).
#
# Fallback tests in particular must skip on those broken platforms because
# local_mocked_bindings(.package = "mvtnorm") itself needs to load the mvtnorm
# namespace — the same operation that fails.  The fallback behaviour is still
# exercised on those platforms by the broader simulate() and confint() tests
# which run without any mvtnorm guards.

# shared fixtures -----------------------------------------------------------

mean_vec  <- c(1, 2, 3)
sigma_mat <- matrix(
  c(1.0, 0.4, 0.2,
    0.4, 1.0, 0.5,
    0.2, 0.5, 1.0),
  nrow = 3, ncol = 3
)


# .rmvnorm() – normal path --------------------------------------------------

test_that(".rmvnorm() returns a matrix with the expected dimensions", {
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  result <- .rmvnorm(20L, mean = mean_vec, sigma = sigma_mat)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), 20L)
  expect_equal(ncol(result), length(mean_vec))
})

test_that(".rmvnorm() results are reproducible with the same seed", {
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  set.seed(4219); r1 <- .rmvnorm(5L, mean = mean_vec, sigma = sigma_mat)
  set.seed(4219); r2 <- .rmvnorm(5L, mean = mean_vec, sigma = sigma_mat)

  expect_equal(r1, r2)
})


# .rmvnorm() – fallback path ------------------------------------------------

test_that(".rmvnorm() warns and falls back when mvtnorm errors", {
  # local_mocked_bindings() needs to load the mvtnorm namespace to install the
  # mock binding, so this test must be skipped when the .so cannot be linked.
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  local_mocked_bindings(
    rmvnorm = function(...) stop("simulated .so linking failure"),
    .package = "mvtnorm"
  )

  expect_warning(
    result <- .rmvnorm(10L, mean = mean_vec, sigma = sigma_mat),
    regexp = "mvtnorm cannot be called"
  )
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 10L)
  expect_equal(ncol(result), length(mean_vec))
})

test_that(".rmvnorm() fallback is reproducible with the same seed", {
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  local_mocked_bindings(
    rmvnorm = function(...) stop("simulated .so linking failure"),
    .package = "mvtnorm"
  )

  set.seed(7341)
  suppressWarnings(r1 <- .rmvnorm(5L, mean = mean_vec, sigma = sigma_mat))
  set.seed(7341)
  suppressWarnings(r2 <- .rmvnorm(5L, mean = mean_vec, sigma = sigma_mat))

  expect_equal(r1, r2)
})

test_that(".rmvnorm() fallback is stable when n increases (row-fill property)", {
  # Filling the z matrix column-by-column (the MASS bug) causes existing rows
  # to change when n grows.  byrow = TRUE means the first n_old rows are
  # stable, which is the behaviour mvtnorm::rmvnorm() provides.
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  local_mocked_bindings(
    rmvnorm = function(...) stop("simulated .so linking failure"),
    .package = "mvtnorm"
  )

  set.seed(2288)
  suppressWarnings(r2 <- .rmvnorm(2L, mean = mean_vec, sigma = sigma_mat))
  set.seed(2288)
  suppressWarnings(r3 <- .rmvnorm(3L, mean = mean_vec, sigma = sigma_mat))

  # First two rows must be identical regardless of whether we asked for 2 or 3
  expect_equal(r2[1L, ], r3[1L, ])
  expect_equal(r2[2L, ], r3[2L, ])
})

test_that(".rmvnorm() fallback column means are close to the requested mean", {
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  local_mocked_bindings(
    rmvnorm = function(...) stop("simulated .so linking failure"),
    .package = "mvtnorm"
  )

  set.seed(8802)
  suppressWarnings(
    result <- .rmvnorm(5000L, mean = mean_vec, sigma = sigma_mat)
  )
  col_means <- colMeans(result)

  expect_equal(col_means, mean_vec, tolerance = 0.1)
})


# .qmvnorm() – normal path --------------------------------------------------

test_that(".qmvnorm() returns a list with a numeric $quantile element", {
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  result <- .qmvnorm(0.95, sigma = sigma_mat, tail = "both.tails")

  expect_true(is.list(result))
  expect_true(is.numeric(result$quantile))
  expect_length(result$quantile, 1L)
})

test_that(".qmvnorm() critical value is >= the marginal 97.5th percentile", {
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  result <- .qmvnorm(0.95, sigma = sigma_mat, tail = "both.tails")

  # Simultaneous critical value must be at least as large as the pointwise one
  expect_true(result$quantile >= stats::qnorm(0.975))
})


# .qmvnorm() – fallback path ------------------------------------------------

test_that(".qmvnorm() warns and falls back when mvtnorm errors", {
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  local_mocked_bindings(
    qmvnorm = function(...) stop("simulated .so linking failure"),
    .package = "mvtnorm"
  )

  expect_warning(
    result <- .qmvnorm(0.95, sigma = sigma_mat, tail = "both.tails"),
    regexp = "mvtnorm cannot be called"
  )
  expect_true(is.list(result))
  expect_true(is.numeric(result$quantile))
  expect_length(result$quantile, 1L)
})

test_that(".qmvnorm() fallback is conservative (>= marginal normal quantile)", {
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  local_mocked_bindings(
    qmvnorm = function(...) stop("simulated .so linking failure"),
    .package = "mvtnorm"
  )

  suppressWarnings(
    result <- .qmvnorm(0.95, sigma = sigma_mat, tail = "both.tails")
  )

  # Bonferroni correction is wider than the pointwise 97.5th percentile
  expect_true(result$quantile >= stats::qnorm(0.975))
})

test_that(".qmvnorm() fallback critical value increases with more parameters", {
  skip_if_not(mvtnorm_usable(), "mvtnorm shared object not usable on this platform")

  local_mocked_bindings(
    qmvnorm = function(...) stop("simulated .so linking failure"),
    .package = "mvtnorm"
  )

  sigma_2d <- sigma_mat[1:2, 1:2]

  suppressWarnings({
    crit_2 <- .qmvnorm(0.95, sigma = sigma_2d,  tail = "both.tails")$quantile
    crit_3 <- .qmvnorm(0.95, sigma = sigma_mat, tail = "both.tails")$quantile
  })

  # More parameters => wider Bonferroni correction
  expect_true(crit_3 >= crit_2)
})
