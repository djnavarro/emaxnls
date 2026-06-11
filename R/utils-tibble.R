
.tibble <- function(..., .no_tibble = FALSE) {
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

