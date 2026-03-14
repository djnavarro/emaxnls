
# adapted from: https://github.com/nathaneastwood/poorman/blob/master/R/case_when.R

.case_when <- function(...) {
  fs <- list(...)
  lapply(fs, function(x) if (!inherits(x, "formula")) stop("`case_when()` requires formula inputs."))
  n <- length(fs)
  if (n == 0L) stop("No cases provided.")
  query <- vector("list", n)
  value <- vector("list", n)
  default_env <- parent.frame()
  for (i in seq_len(n)) {
    query[[i]] <- eval(fs[[i]][[2]], envir = default_env)
    value[[i]] <- eval(fs[[i]][[3]], envir = default_env)
    if (!is.logical(query[[i]])) stop(fs[[i]][[2]], " does not return a `logical` vector.")
  }
  m <- .validate_case_when_length(query, value, fs)
  out <- value[[1]][rep(NA_integer_, m)]
  replaced <- rep(FALSE, m)
  for (i in seq_len(n)) {
    out <- .replace_with(out, query[[i]] & !replaced, value[[i]], NULL)
    replaced <- replaced | (query[[i]] & !is.na(query[[i]]))
  }
  out
}

.validate_case_when_length <- function(query, value, fs) {
  lhs_lengths <- lengths(query)
  rhs_lengths <- lengths(value)
  all_lengths <- unique(c(lhs_lengths, rhs_lengths))
  if (length(all_lengths) <= 1L) return(all_lengths[[1L]])
  non_atomic_lengths <- all_lengths[all_lengths != 1L]
  len <- non_atomic_lengths[[1L]]
  if (length(non_atomic_lengths) == 1L) return(len)
  inconsistent_lengths <- non_atomic_lengths[-1L]
  lhs_problems <- lhs_lengths %in% inconsistent_lengths
  rhs_problems <- rhs_lengths %in% inconsistent_lengths
  problems <- lhs_problems | rhs_problems
  if (any(problems)) {
    stop(
      "The following formulas must be length ", len, " or 1, not ",
      paste(inconsistent_lengths, collapse = ", "), ".\n    ",
      paste(fs[problems], collapse = "\n    ")
    )
  }
}

# adapted from: https://github.com/nathaneastwood/poorman/blob/master/R/replace_with.R

.replace_with <- function(x, i, val, arg_name) {
  if (is.null(val)) return(x)
  .check_length(val, x, arg_name)
  .check_type(val, x, arg_name)
  .check_class(val, x, arg_name)
  i[is.na(i)] <- FALSE
  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }
  x
}

.check_length <- function(x, y, arg_name) {
  length_x <- length(x)
  length_y <- length(y)
  if (all(length_x %in% c(1L, length_y))) return()
  if (length_y == 1) {
    stop(arg_name, " must be length 1, not ", paste(length_x, sep = ", "))
  } else {
    stop(arg_name, " must be length ", length_y, " or 1, not ", length_x)
  }
}

.check_type <- function(x, y, arg_name) {
  x_type <- typeof(x)
  y_type <- typeof(y)
  if (identical(x_type, y_type)) return()
  stop(arg_name, " must be `", y_type, "`, not `", x_type, "`")
}

.check_class <- function(x, y, arg_name) {
  if (!is.object(x)) return()
  exp_classes <- class(y)
  out_classes <- class(x)
  if (identical(out_classes, exp_classes)) return()
  stop(arg_name, " must have class `", exp_classes, "`, not class `", out_classes, "`")
}
