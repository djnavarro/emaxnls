
add <- function(x, y) x + y

test_that(".safe_fn returns a function", {
  expect_no_error(.safe_fn(add))
  safe_add <- .safe_fn(add)
  expect_type(safe_add, "closure")
})

test_that(".safe_fn captures errors", {
  safe_add <- .safe_fn(add)
  expect_no_error(safe_add("a", 12))
})

test_that(".safe_fn return objects with expected structure", {
  safe_add <- .safe_fn(add)
  out1 <- safe_add("a", 12)
  out2 <- safe_add(111, 12)
  expect_type(out1, "list")
  expect_type(out2, "list")
  expect_named(out1, c("result", "error"))
  expect_named(out2, c("result", "error"))
  expect_null(out1$result)
  expect_null(out2$error)
  expect_equal(out2$result, add(111, 12))
})

chat <- function() {
  rlang::inform("message")
  rlang::warn("warning")
  cat("output")
  return("result")
}

test_that(".quiet_fn returns a function", {
  expect_no_error(.quiet_fn(chat))
  dont_chat <- .quiet_fn(chat)
  expect_type(dont_chat, "closure")
})

test_that(".quiet_fn suppresses output, message, warning", {
  dont_chat <- .quiet_fn(chat)
  expect_no_message(dont_chat())
  expect_no_warning(dont_chat())
  expect_silent(dont_chat())
})

test_that(".quiet_fn captures output, message, warning, result", {
  dont_chat <- .quiet_fn(chat)
  out <- dont_chat()
  expect_type(out, "list")
  expect_named(out, c("result", "output", "warnings", "messages"))
  expect_equal(out$result, "result")
  expect_equal(out$output, "output")
  expect_equal(out$warnings, "warning")
  expect_equal(out$messages, "message")
})


