# Internal wrappers around mvtnorm::rmvnorm() and mvtnorm::qmvnorm().
#
# These wrappers exist so that all code in the package calls a single internal
# entry point rather than the mvtnorm namespace directly.  On some build
# platforms (notably clang-based Rhub builders) mvtnorm can be registered as a
# namespace but its shared object fails to link at runtime.  Each function
# therefore wraps the real call in tryCatch and falls back to a base-R
# implementation when the call errors for any reason.  A warning is always
# issued when the fallback is activated, because the two code-paths are not
# numerically identical and callers should not silently depend on fallback
# behaviour.

# .rmvnorm() ---------------------------------------------------------------
# Fallback: draw n samples from N(mean, sigma) via Cholesky decomposition.
# This is equivalent to mvtnorm::rmvnorm(..., method = "chol") and requires
# only base-R (stats::rnorm + chol + matrix arithmetic).

.rmvnorm <- function(n, mean, sigma, ...) {
  tryCatch(
    mvtnorm::rmvnorm(n, mean = mean, sigma = sigma, ...),
    error = function(e) {
      warning(
        "mvtnorm cannot be called (the shared object may have failed to link). ",
        "Falling back to base R Cholesky sampling. ",
        "Simulation results may differ slightly from the mvtnorm implementation.",
        call. = FALSE
      )
      p <- length(mean)
      L <- chol(sigma)  # upper-triangular: L'L == sigma
      z <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
      sweep(z %*% L, 2L, mean, "+")
    }
  )
}


# .qmvnorm() ---------------------------------------------------------------
# Fallback: compute a Bonferroni-corrected normal quantile.
# This is conservative (wider than the true multivariate quantile) but valid.
# k = nrow(sigma) is the number of simultaneously tested parameters.

.qmvnorm <- function(p, sigma, tail = "both.tails", ...) {
  tryCatch(
    mvtnorm::qmvnorm(p, sigma = sigma, tail = tail, ...),
    error = function(e) {
      warning(
        "mvtnorm cannot be called (the shared object may have failed to link). ",
        "Falling back to a Bonferroni-corrected normal quantile for simultaneous ",
        "confidence intervals. Intervals will be conservative.",
        call. = FALSE
      )
      k     <- nrow(sigma)
      alpha <- 1 - p
      crit  <- switch(
        tail,
        "both.tails"  = stats::qnorm(1 - alpha / (2 * k)),
        "lower.tail"  = stats::qnorm(1 - alpha / k),
        "upper.tail"  = stats::qnorm(1 - alpha / k),
        stats::qnorm(p)   # safe default for unrecognised tail argument
      )
      list(quantile = crit)
    }
  )
}
