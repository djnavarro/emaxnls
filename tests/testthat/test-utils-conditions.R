
test_that(".assert behaves as expected", {
  expect_error(.assert(FALSE), class = "emaxnls_error")
  expect_no_error(.assert(TRUE))
})

test_that(".inform behaves as expected", {
  expect_message(.inform("hello"), class = "emaxnls_message")
})

test_that(".warn behaves as expected", {
  expect_warning(.warn("oh no"), class = "emaxnls_warning")
})

test_that(".abort behaves as expected", {
  expect_error(.abort("this is bad"), class = "emaxnls_error")
})

