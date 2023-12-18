test_that("move_exposes_king works", {
  board <- fen_to_board("8/R7/4Q1pk/7p/5P1P/6P1/7K/8 b - - 0 63")
  coordinates <- c(6, 7)
  new_coordinates <- c(5, 7)
  color <- "black"
  expect_true(move_exposes_king(board, coordinates, new_coordinates, color))
  board <- fen_to_board()
  coordinates <- c(2, 5)
  new_coordinates <- c(4, 5)
  color <- "white"
  expect_false(move_exposes_king(board, coordinates, new_coordinates, color))
})
