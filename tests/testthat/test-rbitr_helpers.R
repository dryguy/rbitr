list_1 <- list(
  list(list(1, 2, 3), list(4, 5, 6)),
  list(list(7, 8, 9), list(10, 11, 12))
)
list_2 <- list(
  list(c(1, 2, 3), c(4, 5, 6)),
  list(c(7, 8, 9), c(10, 11, 12))
)
list_3 <- list(
  list(list(1, 2, 3), list(4, 5, 6)),
  list(list(7, 8, 9), numeric(0))
)
list_4 <- list(
  list(list(1, 2, 3), list(4, 5, 6)),
  list(list(7, 8, 9), NA)
)
test_that('nested_lapply applies input_function to root levels of x', {
  result <- nested_lapply(list_1, unlist)
  expect_identical(result, list_2)
  replace_empty_vectors_with_NA <- function(y) {
    if (length(y) == 0) {NA} else {y}
  }
  result <- nested_lapply(list_3, replace_empty_vectors_with_NA)
  expect_identical(result, list_4)
})
