
test_that(".lst() returns a list", {
  expect_type(.lst(1), "list")
  expect_type(.lst(), "list")
  expect_type(.lst(NULL), "list")
})

test_that(".lst() mirrors list() for named input", {
  expect_equal(.lst(a = 1), list(a = 1))
  expect_equal(.lst(a = 1, b = NULL), list(a = 1, b = NULL))
})

test_that(".lst() evaluates sequentially", {
  expect_no_error(.lst(a = 1, b = a + 1))
  expect_equal(.lst(a = 1, b = a + 1), list(a = 1, b = 2))
})

