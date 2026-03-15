
add_one <- function(x) x + 1

test_that(".map() behaves as simplified purrr::map()", {
  expect_equal(.map(list(1, "a", TRUE), as.character), list("1", "a", "TRUE"))
  expect_equal(.map(1:3, add_one), list(2, 3, 4))
})

test_that(".map_xxx() behaves as simplified purrr::map_xxx()", {
  expect_equal(.map_chr(list(1, "a", TRUE), as.character), c("1", "a", "TRUE"))
  expect_equal(.map_dbl(1:3, add_one), c(2.0, 3.0, 4.0))
  expect_equal(.map_lgl(list(1, "a", TRUE), is.character), c(FALSE, TRUE, FALSE))
})

test_that(".walk() behaves as simplified purrr::walk()", {
  expect_output(.walk(list(1, "a", TRUE), cat), "1aTRUE")  
  capture_output(expect_equal(.walk(list(1, "a", TRUE), cat), list(1, "a", TRUE)))
})

test_that(".map2() behaves as simplified purrr::map2", {
  expect_equal(.map2(1:3, c("a", "b", "c"), paste0), list("1a", "2b", "3c"))
})

test_that("imap() behaves as simplified purrr::imap", {
  expect_equal(.imap(c("a" = 1, "b" = 2, "c" = 3), paste0), list("a" = "1a", "b" = "2b", "c" = "3c"))
})

