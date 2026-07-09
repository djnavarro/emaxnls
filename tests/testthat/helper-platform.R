# helpers to check what compiler is used, to allow test skip on particular
# platforms. since emaxnls does not contain compiled code, the issues in
# regard to model convergence are not related to emaxnls itself, so skipping
# certain tests on particular platforms makes sense

# check for clang
is_clang <- local({
  result <- NULL
  function() {
    if (is.null(result)) {
      cc <- tryCatch(
        system2(file.path(R.home("bin"), "R"), c("CMD", "config", "CC"),
                stdout = TRUE, stderr = TRUE),
        error = function(e) ""
      )
      result <<- any(grepl("clang", cc, ignore.case = TRUE))
    }
    result
  }
})

# look for gcc version
gcc_version <- function() {
  cc <- tryCatch(
    system2(file.path(R.home("bin"), "R"), c("CMD", "config", "CC"),
            stdout = TRUE, stderr = TRUE),
    error = function(e) ""
  )
  if (!any(grepl("gcc", cc, ignore.case = TRUE))) return(NA_character_)

  # ask the compiler directly for its version
  cc_bin <- trimws(cc[1])
  ver_out <- tryCatch(
    system2(cc_bin, "-dumpversion", stdout = TRUE, stderr = TRUE),
    error = function(e) ""
  )
  if (length(ver_out) == 0 || !nzchar(ver_out[1])) return(NA_character_)
  ver_out[1]
}

# general purpose gcc detector
is_gcc_version <- function(major) {
  v <- gcc_version()
  if (is.na(v)) return(FALSE)
  identical(as.integer(strsplit(v, "\\.")[[1]][1]), as.integer(major))
}

# the one I care about
is_gcc15 <- function() is_gcc_version(15)


# skip a test that assumes model convergence if the model did not converge;
# used throughout the test suite to guard tests that use a fitted model but
# are not themselves testing convergence
skip_if_not_converged <- function(mod) {
  skip_if(!.is_converged(mod), "Skip: model did not converge on this architecture")
}


# check whether mvtnorm is actually callable, not just installed -------------
#
# On some clang-based Rhub builders, mvtnorm is installed and its namespace can
# be registered, but the shared object fails to link at runtime.  In that case
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