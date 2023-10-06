test_that("fen_to_board works", {
  result <- fen_to_board()
  expect_equal(dim(result$board), c(8, 8))
  expect_equal(result$to_move, "w")
  expect_equal(result$castling_rights, "KQkq")
  expect_equal(result$ep_target, "-")
  expect_equal(result$halfmove_clock, "0")
  expect_equal(result$fullmove_number, "1")
  starting_position <- matrix(c("R", "N", "B", "Q", "K", "B", "N", "R",
                                rep('P', 8), rep('', 32), rep('p', 8),
                                "r", "n", "b", "q", "k", "b", "n", "r"),
                              nrow = 8, byrow = TRUE)
  colnames(starting_position) <- letters[1:8]
  rownames(starting_position) <- as.character(1:8)
  expect_equal(result$board, starting_position)
})
