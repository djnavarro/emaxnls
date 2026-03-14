

.safe_fn <- function(.f) {
  function(...) {
    tryCatch(
      list(result = .f(...), error = NULL), 
      error = function(e) list(result = NULL, error = e)
    )
  }
}

.quiet_fn <- function(.f) {
  function(...) {
    warnings <- character()
    wHandler <- function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
    messages <- character()
    mHandler <- function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
    temp <- file()
    sink(temp)
    on.exit({
      sink()
      close(temp)
    })
    result <- withCallingHandlers(.f(...), warning = wHandler, message = mHandler)
    output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")
    list(
      result = result, 
      output = output, 
      warnings = warnings, 
      messages = messages
    )
  }
}

