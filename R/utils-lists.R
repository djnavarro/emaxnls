
# construct a list sequentially, allowing the user to refer to previously
# defined components in the call. adapted from:
# https://github.com/nathaneastwood/poorman/blob/master/R/lst.R

.lst <- function(...) {
  fn_call <- match.call()
  list_to_eval <- as.list(fn_call)[-1]

  out <- vector(mode = "list", length = length(list_to_eval))
  names(out) <- names(list_to_eval)
  exprs <- lapply(substitute(list(...)), deparse)[-1]
  for (element in seq_along(list_to_eval)) {
    value <- list_to_eval[[element]]
    if (is.language(value)) {
      # need to update the environment in which the values are obtained
      # ex: lst(a = 1, a = a + 1, b = a), 'b' needs the updated value of 'a',
      # not its initial value.
      value <- eval(
        value,
        envir = if (length(out) == 0) {
          list_to_eval
        } else {
          # restrict the environment to the previous elements of the list (and
          # to the last value for each name if there are duplicated names)
          .drop_dup_list(out[1:(element - 1)])
        }
      )
    }
    if (is.null(value)) {
      out[element] <- list(NULL)
    } else {
      out[[element]] <- value
    }

    # this naming part needs to happen at the end of the loop to avoid error
    # with lst(NULL)
    invalid_name <- is.null(names(out)[element]) ||
      is.na(names(out)[element]) ||
      names(out)[element] == ""

    if (invalid_name) {
      if (exprs[[element]] != "NULL" || (exprs[[element]] == "NULL" && is.null(out[[element]]))) {
        names(out)[element] <- exprs[[element]]
      }
    }
  }
  out
}

# retain only the last list item with a particular name
.drop_dup_list <- function(x) {

  list_names <- names(x)
  if (identical(list_names, unique(list_names))) return(x)

  count <- table(list_names)
  dupes <- names(count[count > 1])
  uniques <- names(count[count == 1])

  to_drop <- do.call(c, lapply(
    dupes,
    function(x) {
      matches <- which(list_names == x)
      matches[-length(matches)]
    }
  ))
  x[uniques] <- Filter(Negate(is.null), x[uniques])

  return(x[-to_drop])

}
