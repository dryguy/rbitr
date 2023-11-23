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

  # Bug where some pieces have wrong color
  movetext <- "1. Nh3 a5 2. d4 a4 3. Kd2 g5 4. f4 Bh6 5. fxg5 Kf8 6. c4 Nf6 7. e3 c5 8. a3 Kg7 9. Be2 Qc7 10. Qc2 Nd5 11. e4 Nb4 12. g4 Qxh2 13. Nf2 Rd8 14. Bd1 Qf4+ 15. Ke2 e6 16. Nd3 e5 17. Bxf4 f5 18. Kf2 Kh8 19. Rh3 Na2 20. Qc3 Rf8 21. g6 Ra7 22. Rf3 Nc1 23. b4 fxg4 24. Bxc1 Be3+ 25. Kg2 cxd4 26. Nc5 d3 27. Nb3 Bd4 28. Bc2 b5 29. Kg3 Bb7 30. Nc5 Rf6 31. Nxd7 gxf3 32. Kh4 Rfa6 33. Qe1 bxc4 34. Qh1 Rb6 35. Nxb8 Bxa1 36. Bxa4 Ra5 37. Bb5 Kg7 38. Qg2 Bc3 39. a4 hxg6 40. Nxc3 Raxb5 41. Qf1 Rxb4 42. Qg2 Rc6 43. Qd2 Rcb6 44. Qf4 g5+ 45. Kh5 Rh6+ 46. Kxg5 Rh8 47. Qg4 Bc6 48. Bf4 Rh2 49. Qe6 Rb5 50. Nd5 exf4 51. Qh6+ Rxh6 52. a5 Rb4 53. Na6 d2 54. Ndxb4 Rh1 55. Nc7 Rh6 56. Ne6+ Kf7 57. Kf5 Kg8 58. Ng7 Bb5 59. Nd5 Kf7 60. Nc7 Re6 61. Ncxe6 Bd7 62. Ne8 Kxe8 63. e5 Bb5 64. Nxf4 Ke7 65. Ng2 Ba6 66. Ke4 Bb7+ 67. Kf4 Bc6 68. Kg5 d1=N 69. Kh6 Be4 70. Kg7 f2 71. Kh6 f1=Q 72. Nh4 Qh3 73. Kg5 Qg3+ 74. Kh5 Qxe5+ 75. Kh6 Qxa5 76. Ng6+ Bxg6 77. Kxg6 c3 78. Kg7 Qh5 79. Kg8 Kf6 80. Kf8 Qf7+"
  lan <- bigchess::san2lan(movetext)
  fen <- rbitr::lan_to_fen(lan)
  expect_equal(fen, "5K2/5q2/5k2/8/8/2p5/8/3n4 w - - 6 81")
})
