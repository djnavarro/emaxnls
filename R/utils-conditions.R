
.assert <- function(expr, message = "emax_nls error", class = "emaxnls_error") {
  if (any(expr == FALSE)) rlang::abort(message = message, class = class)
}

.warn <- function(message = "emax_nls warning", class = "emaxnls_warning") {
  rlang::warn(message = message, class = class)
}

.abort <- function(message = "emax_nls error", class = "emaxnls_error") {
  rlang::abort(message = message, class = class)
}

.inform <- function(message = "emax_nls_message", class = "emaxnls_message") {
  rlang::inform(message = message, class = class)
}
