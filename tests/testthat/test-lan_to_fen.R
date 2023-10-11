test_that("lan_to_fen correctly converts a series of moves to a FEN string", {
  lan <- "e2e4 e7e5 g1f3"
  expect_equal(lan_to_fen(lan),
               "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")

  # Castling
  lan <- "e2e4 e7e5 g1f3 b8c6 f1b5 g8f6 e1g1"
  expect_equal(lan_to_fen(lan),
           "r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 5 4")

  # Pawn promotion
  lan <- "h2h4 g7g5 g2g4 g5h4 g4g5 g8f6 g5g6 e7e5 g6g7 d7d5 g7g8Q"
  expect_equal(lan_to_fen(lan),
               "rnbqkbQr/ppp2p1p/5n2/3pp3/7p/8/PPPPPP2/RNBQKBNR b KQkq - 0 6")

  # Start position
  lan <- ""
  expect_equal(lan_to_fen(lan),
               "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

  # En passant
  lan <- "g2g4 d7d5 g4g5 f7f5 g5f6"
  expect_equal(lan_to_fen(lan),
               "rnbqkbnr/ppp1p1pp/5P2/3p4/8/8/PPPPPP1P/RNBQKBNR b KQkq - 0 3")
})
