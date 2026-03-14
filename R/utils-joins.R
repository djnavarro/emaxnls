
# adapted from: https://github.com/nathaneastwood/poorman/blob/master/R/joins.R
# requires that the user explictly specify "by"

.inner_join <- function(x, 
                        y, 
                        by, 
                        suffix = c(".x", ".y"), 
                        ..., 
                        na_matches = c("na", "never")) {
  .join_worker(
    x = x, 
    y = y, 
    by = by, 
    suffix = suffix, 
    sort = FALSE, 
    ..., 
    keep = FALSE, 
    na_matches = na_matches
  )
}

.left_join <- function(x, 
                       y, 
                       by, 
                       suffix = c(".x", ".y"), 
                       ..., 
                       keep = FALSE, 
                       na_matches = c("na", "never")) {
  .join_worker(
    x = x, 
    y = y,
    by = by, 
    suffix = suffix, 
    all.x = TRUE, 
    ..., 
    keep = keep, 
    na_matches = na_matches
  )
}

.right_join <- function(x, 
                        y, 
                        by, 
                        suffix = c(".x", ".y"), 
                        ..., 
                        keep = FALSE, 
                        na_matches = c("na", "never")) {
  .join_worker(
    x = x, 
    y = y, 
    by = by, 
    suffix = suffix, 
    all.y = TRUE, 
    ..., 
    keep = keep, 
    na_matches = na_matches
  )
}

.full_join <- function(x, 
                       y, 
                       by, 
                       suffix = c(".x", ".y"), 
                       ..., 
                       keep = FALSE, 
                       na_matches = c("na", "never")) {
  .join_worker(
    x = x, 
    y = y, 
    by = by, 
    suffix = suffix, 
    all = TRUE, 
    ...,
    keep = keep, 
    na_matches = na_matches
  )
}

.join_worker <- function(x, y, by, suffix, keep, na_matches, ...) {
  na_matches <- match.arg(arg = na_matches, choices = c("na", "never"), several.ok = FALSE)
  incomparables <- if (na_matches == "never") NA else NULL
  x[, ".join_id"] <- seq_len(nrow(x))
  merged <- if (is.null(names(by))) {
    merge(x = x, y = y, by = by, suffixes = suffix, incomparables = incomparables, ...)
  } else {
    merge(x = x, y = y, by.x = names(by), by.y = by, suffixes = suffix, incomparables = incomparables, ...)
  }
  merged <- merged[order(merged[, ".join_id"]), colnames(merged) != ".join_id", drop = FALSE]
  if (isTRUE(keep)) {
    keep_pos <- match(by, names(merged))
    x_by <- paste0(by, suffix[1L])
    colnames(merged)[keep_pos] <- x_by
    merged[, paste0(by, suffix[2L])] <- merged[, x_by]
  }
  rownames(merged) <- NULL
  merged
}
