#' Given a movetext string, generate a vector of chess positions
#'
#' Given a single-element character vector containing a series of chess *moves*
#' (typically from a pgn) in standard algebraic notation (SAN), return a
#' character vector containing the game *positions* in long algebraic notation
#' (LAN), to enable evaluation by a chess engine. The positions are represented
#' as sequences of moves.
#'
#' @param movetext A single element character vector containing a sequence of
#' moves in SAN.
#'
#' @return A character vector where each element represents a successive
#' position in the game as a sequence of moves in LAN.
#' @export
#'
#' @examples
#' movetext <- '1. e4 g5 2. Nc3 f5 3. Qh5#'
#' get_positions(movetext)
get_positions <- function(movetext) {
  # Convert moves to machine readable format
  # SAN = Standard Algebraic Notation (human readable)
  # LAN = Long Algebraic Notation (machine readable)
  get_position <- function(movetext) {
    if (movetext == '' | is.na(movetext)) {
      return('')
    } else {
      lan_moves <- unlist(
        strsplit(bigchess::san2lan(movetext), ' ', fixed = TRUE),
        use.names = FALSE
      )
      lan_moves <- tolower(lan_moves)

      # Convert to a list of positions
      ply <- 1:length(lan_moves)
      paste_moves <- function(ply_depth) {
        return(stringr::str_c(
          lan_moves[1:ply_depth], collapse = ' '
        ))
      }
      position <- unlist(lapply(ply, paste_moves), use.names = FALSE)
      c('', position)
    }
  }
  lapply(movetext, get_position)
}
