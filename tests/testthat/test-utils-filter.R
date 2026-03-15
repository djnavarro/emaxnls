
test_that(".dotdotdot captures dots and can impute names", {
  expect_equal(.dotdotdot(a = 1, b = 2), list(a = 1, b = 2))
  expect_equal(.dotdotdot("a", 1, .impute_names = FALSE), list("a", 1))
  expect_equal(.dotdotdot("a", 1, .impute_names = TRUE), list(`"a"` = "a", `1` = 1))
  expect_equal(.dotdotdot("a", b = 1, .impute_names = TRUE), list(`"a"` = "a", b = 1))
})

df <- data.frame(
  aa = 1:4,
  bb = c("a", "b", "c", "d"),
  cc = c(TRUE, FALSE, FALSE, FALSE)
)
out <- df[1,]

test_that(".filter behaves like a very simple dplyr::filter", {
  expect_equal(.filter(df, cc), out)
  expect_equal(.filter(df, aa == 1), out)  
  expect_equal(.filter(df, aa < 3, bb != "b"), out)  
})

