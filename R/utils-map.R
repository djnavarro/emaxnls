
.map <- function(.x, .f) {
  out <- lapply(X = .x, FUN = .f)
  names(out) <- names(.x)
  out
}

.walk <- function(.x, .f) {
  .map(.x, .f)
  invisible(.x)  
}

.map2 <- function(.x, .y, .f) {
  .assert(length(.x) == length(.y))  
  .mapper <- function(ind) .f(.x[[ind]], .y[[ind]])
  lapply(X = seq_along(.x), FUN = .mapper)
}

.imap <- function(.x, .f) {
  out <- .map2(.x, names(.x), .f)
  names(out) <- names(.x)
  out
}

.iwalk <- function(.x, .f) {
  .map2(.x, names(.x), .f)
  invisible(.x)
}

.map_dbl <- function(.x, .f) {
  vapply(X = .x, FUN = .f, FUN.VALUE = numeric(1L))
}

.map_lgl <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = logical(1L))
}

.map_chr <- function(.x, .f) {
  vapply(X = .x, FUN = .f, FUN.VALUE = character(1L))
}
