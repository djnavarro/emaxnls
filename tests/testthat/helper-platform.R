
# skip a test that assumes model convergence if the model did not converge;
# used throughout the test suite to guard tests that use a fitted model but
# are not themselves testing convergence
skip_if_not_converged <- function(mod) {
  skip_if(!.is_converged(mod), "Skip: model did not converge on this architecture")
}


# default options for use in tests: adds a max_time limit to prevent model fits
# from stalling the test suite. all other arguments are forwarded to the
# underlying options function, so tests that need specific settings
# (e.g., optim_method, optim_control) can still pass them and they will
# override the defaults.
test_nls_opts <- function(...) emax_nls_options(..., max_time = 10)
test_logistic_opts <- function(...) emax_logistic_options(..., max_time = 10)


# check whether mvtnorm is actually callable, not just installed -------------
#
# On some Rhub builders, mvtnorm is installed and its namespace can be
# registered, but the shared object fails to link at runtime.  In that case
# requireNamespace("mvtnorm") returns TRUE, yet any call to a mvtnorm function
# errors.  This helper detects that case by actually invoking rmvnorm() with a
# trivial argument and catching any error.
#
# The result is memoised so the probe is only run once per test session.
mvtnorm_usable <- local({
  result <- NULL
  function() {
    if (is.null(result)) {
      result <<- tryCatch({
        mvtnorm::rmvnorm(1L, mean = c(0, 0), sigma = diag(2))
        TRUE
      }, error = function(e) FALSE)
    }
    result
  }
})
