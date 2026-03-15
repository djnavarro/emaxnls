
letters <- c("a", "b", "c", "d")
numbers <- 1:4

test_that(".case_when behaves like simplified dplyr::case_when", {
  expect_equal(
    .case_when(
      numbers < 3 ~ toupper(letters),
      TRUE ~ letters
    ),
    c("A", "B", "c", "d")
  )
})

