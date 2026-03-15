
xx <- data.frame(
  aa = 1:2,
  dd = c("o", "p")
)
yy <- data.frame(
  aa = 1:4,
  bb = c("a", "b", "c", "d"),
  cc = c(TRUE, FALSE, FALSE, FALSE)
)
zz <- data.frame(
  aa = c(1, 5),
  ee = c("r", "s")  
)

test_that("joins work as expected", {
  expect_equal(
    .left_join(xx, yy, by = "aa"),
    data.frame(aa = 1:2, dd = c("o", "p"), bb = c("a", "b"), cc = c(TRUE, FALSE))
  )
  expect_equal(
    .right_join(yy, xx, by = "aa"),
    data.frame(aa = 1:2, bb = c("a", "b"), cc = c(TRUE, FALSE), dd = c("o", "p"))
  )
  expect_equal(
    .full_join(xx, yy, by = "aa"),
    data.frame(
      aa = 1:4,
      dd = c("o", "p", NA, NA),
      bb = c("a", "b", "c", "d"),
      cc = c(TRUE, FALSE, FALSE, FALSE)
    )
  )
  expect_equal(
    .inner_join(xx, zz, by = "aa"),
    data.frame(aa = 1, dd = "o", ee = "r")
  )
})

