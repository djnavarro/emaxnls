
# massively simplified approach to filtering, loosely inspired by:
# https://github.com/nathaneastwood/poorman/blob/master/R/filter.R

.filter <- function(.data, ...) {
  conditions <- .dotdotdot(...)
  rows <- lapply(
    conditions,
    function(cond, frame) eval(cond, .data, frame),
    frame = parent.frame()
  )
  rows <- Reduce("&", rows)
  .data[rows & !is.na(rows), ]
}

# adapted from: https://github.com/nathaneastwood/poorman/blob/master/R/dots.R

.dotdotdot <- function(..., .impute_names = FALSE) {
  dots <- eval(substitute(alist(...)))
  if (isTRUE(.impute_names)) {
    deparse_dots <- lapply(dots, deparse)
    names_dots <- names(dots)
    unnamed <- if (is.null(names_dots)) rep(TRUE, length(dots)) else nchar(names_dots) == 0L
    names(dots)[unnamed] <- deparse_dots[unnamed]
  }
  dots
}
