# check clang for rhub tests
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