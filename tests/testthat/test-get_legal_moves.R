# Load the locations of the chess engines that will be tested
engines_file_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'engine_paths.R'
)
source(engines_file_path)

# Load game with high number of possible moves in a single position.
# Black has 79 possible moves after whites 23rd move.
# (https://timkr.home.xs4all.nl/records/records.htm)
pgn_path <- pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'test',
  '79_possible_moves_at_whites_23.pgn'
)
pgn <- get_pgn(pgn_path)
movetext <- clean_movetext(pgn$Movetext)
moves <- get_moves(movetext)
position <- moves[[1]][1:45]
position <- paste0(position, collapse = ' ')

for (engine_path in engine_paths) {
  legal_moves <- get_legal_moves('', engine_path)
  legal_moves <- sort(legal_moves)
  test_that("get_legal_moves returns all legal moves", {
    expect_identical(length(legal_moves), 20L)
    expect_identical(class(legal_moves), 'character')
    expect_identical(legal_moves, c("a2a3", "a2a4", "b1a3", "b1c3", "b2b3",
                                    "b2b4", "c2c3", "c2c4", "d2d3", "d2d4",
                                    "e2e3", "e2e4", "f2f3", "f2f4", "g1f3",
                                    "g1h3", "g2g3", "g2g4", "h2h3", "h2h4"))
  })
  legal_moves <- get_legal_moves(position, engine_path)
  test_that("get_legal_moves works when many moves exist", {
    expect_identical(length(legal_moves), 79L)
  })
}
