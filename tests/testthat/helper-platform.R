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