
.tibble <- function(..., .no_tibble = FALSE) {
  # detect cross-column references: the data.frame fallback does not support
  # them, so we error unconditionally to prevent regression regardless of
  # whether tibble happens to be available in the current session
  exprs    <- rlang::enexprs(...)
  nms      <- names(exprs)
  for (k in seq_along(exprs)) {
    if (k < 2L) next
    prior_nms <- nms[seq_len(k - 1L)]
    prior_nms <- prior_nms[nzchar(prior_nms)]
    if (length(prior_nms) == 0L) next
    refs <- intersect(all.vars(exprs[[k]]), prior_nms)
    if (length(refs) > 0L) {
      stop(
        "`.tibble()` does not support cross-column references: column `",
        nms[[k]], "` references `", refs[[1L]], "`, which is defined ",
        "earlier in the same call. Pre-compute intermediate values before ",
        "calling `.tibble()`.",
        call. = FALSE
      )
    }
  }
  if (rlang::is_installed("tibble") & !.no_tibble) {
    return(tibble::tibble(...))
  }  
  lst <- .lst(...)
  lst <- lapply(lst, function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    x
  })
  as.data.frame(lst)
}

.as_tibble <- function(..., .no_tibble = FALSE) {
  if (rlang::is_installed("tibble") & !.no_tibble) {
    return(tibble::as_tibble(...))
  }
  as.data.frame(...)
}

.rownames_to_column <- function(.data, var = "rowname", .no_tibble = FALSE) {
  if (rlang::is_installed("tibble") & !.no_tibble) {
    return(tibble::rownames_to_column(.data = .data, var = var))
  }
  rn <- rownames(.data)
  if (is.null(rn)) return(.data) 
  rownames(.data) <- NULL
  df_col <- data.frame(rn)
  names(df_col) <- var
  cbind(df_col, .data)
}

.add_row <- function(.data, ..., .no_tibble = FALSE) {
  if (rlang::is_installed("tibble") & !.no_tibble) {
    return(tibble::add_row(.data = .data, ...))
  }
  new_row <- .tibble(...)
  rbind(.data, new_row)
}

