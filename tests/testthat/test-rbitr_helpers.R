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


dataframe <- data.frame(
  x = 1:4,
  y = c(1, -1, 1, -1)
)
new_dataframe <- add_x_intercepts(dataframe, 'x', 'y')
test_that('add_x_intercepts adds x-intercepts', {
  expect_equal(new_dataframe$x, c(1, 1.5, 2, 2.5, 3, 3.5, 4))
})


dataframe <- data.frame(
  x = 1:4,
  y = c(1, -1, 1, -1)
)
p1 <- plot_2_color_area(dataframe, 'x', 'y')
test_that("plot_2_color_area plots 2 color area", {
  expect_equal(class(p1), c('gg', 'ggplot'))
})
