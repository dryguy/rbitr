#' Create a data frame containing engine analysis of a single chess position
#'
#' The `tabulate_positionlog()` function takes a character vector of engine
#' analysis for a single chess position and condenses it into a data frame for
#' easier access.
#'
#' @details The `tabulate_positionlog()` function tabulates data from a
#'   [UCI-compatible](https://github.com/fsmosca/UCIChessEngineProtocol) chess
#'   engine and returns it as a data frame. Data sent by the engine may include
#'   the best move, the principal variations, the current depth of search, the
#'   number of nodes searched, and so on. The data are marked in the output by
#'   tags such as `depth`, `pv`, and `score`. The tags will appear in the data
#'   frame as column names.
#'
#'   For a list of tags that can appear, refer to the
#'   [UCI](https://github.com/fsmosca/UCIChessEngineProtocol) protocol. Some
#'   engines may also provide information that is not covered by the UCI
#'   protocol.
#'
#'   By default, the only data extracted are `depth`, `multipv`, `score`, and
#'   `pv`. The `custom_tags` parameter allows the user to choose additional tags
#'   to extract. Setting the `all_tags` parameter to `TRUE` will extract all of
#'   the tags listed in the UCI protocol.
#'
#'   The `custom_tags` parameter may also be used for engines that supply data
#'   not covered by the UCI protocol, but *only* if the engine's custom tags
#'   conform to one of the following formats:
#'
#'   * (tag name) (tag value)
#'   * (tag name) (move 1) (move 2) ... (move i)
#'
#'   where (tag name), (tag value), and (move i) are space delimited, and must
#'   not contain spaces themselves. Additionally, moves must be in the UCI LAN
#'   format. Each (move i) should consist of two chessboard squares followed by
#'   an optional promotion piece, e.g. e2e4 or e7e8q.
#'
#'   When requested data is not found, `NA` will appear to indicate missing
#'   values. If entire rows or columns are missing, they will be deleted. To
#'   leave missing rows/columns in place, set the `delete_blank_lines` parameter
#'   to `FALSE.`
#'
#' @note `tabulate_positionlog()` does not change the class of the data it
#'   returns. Since the `positionlog` parameter is a character vector, the
#'   function will return a data frame with character columns.
#'
#' @param positionlog A character vector that contains engine analysis.
#' @param all_tags (Default = `FALSE`) A Boolean. Setting `all_tags` = TRUE will
#'   search for all of the tags listed in the UCI protocol.
#' @param custom_tags (Default = `NULL`) An optional character vector of custom
#'   tag names.
#' @param delete_blank_lines (Default = `TRUE`) A Boolean. Setting this value to
#'   FALSE will leave blank rows/columns intact.
#'
#' @return A data frame with character columns summarizing the data for the
#'   position.
#'
#' @export
#'
#' @seealso The 'tabulate' functions tabulate analysis logs into data frames.
#'   * [rbitr::tabulate_positionlog()] for condensing analysis of one position.
#'   * [rbitr::tabulate_gamelog()] for condensing analysis of one game.
#'   * [rbitr::tabulate_pgnlog()] for condensing analysis of an entire pgn file.
#'
#'   The 'parse' functions extract specific data from analysis logs.
#'   * [rbitr::parse_gamelog()] for extracting data from one evaluated game.
#'   * [rbitr::parse_pgnlog()] for extracting data from games in a pgn.
#'
#'   The 'evaluate' functions produce analysis logs.
#'   * [rbitr::evaluate_position()] for analyzing chess positions.
#'   * [rbitr::evaluate_game()] for analyzing individual games.
#'   * [rbitr::evaluate_pgn()] for evaluating all the games in a PGN file.
#'
#' @examples
#' positionlog <- c(
#'   "info depth 1 seldepth 1 multipv 1 score cp 13 nodes 20 nps 20000 tbhits 0 time 1 pv e2e4",
#'   "info depth 2 seldepth 2 multipv 1 score cp 14 nodes 40 nps 40000 tbhits 0 time 1 pv e2e4 e7e5",
#'   "bestmove e2e4 ponder e7e5"
#' )
#' tabulate_positionlog(positionlog)
tabulate_positionlog <- function(positionlog, all_tags = FALSE, custom_tags = NULL,
                             delete_blank_lines = TRUE) {

  # According to the UCI protocol, when the engine has stopped searching, it
  # should send the best move, and optionally, which move it will ponder if
  # told. The `bestmove` line takes the format:
  #
  # `bestmove <move1> [ponder <move2>]`, where:
  #  - `<move1>` is the best move found by the engine, in long algebraic notation
  #  - `<move2>` is the move the engine expects the opponent may play in response.
  #
  # At other times, the engine may send data indicating the engine's evaluation
  # of the position (and associated principal variation, or pv for short), the
  # current depth of search, the number of nodes searched, and so on. For
  # example:
  #
  # `info depth [a] seldepth [b] score [cp/mate] [c] pv [d]`, where:
  #   - `[a]` is the depth of the search
  #   - `[b]` is the selective depth of the search
  #   - `[cp/mate]` indicates if the score is in centipawns or moves until mate
  #   - `[c]` is the score of the position
  #   - `[d]` is the principal variation

  # Validate input
  assertthat::assert_that(is.character(positionlog))
  assertthat::assert_that(assertthat::is.flag(all_tags))
  assertthat::assert_that(is.character(custom_tags) | is.null(custom_tags))
  assertthat::assert_that(assertthat::is.flag(delete_blank_lines))

  # If custom tags are provided, add them to the list of tags to search for.
  tag_names <- c(custom_tags, 'depth', 'multipv', 'score', 'pv')
  if (all_tags) {
    tag_names <- c(tag_names, 'seldepth', 'nodes', 'nps', 'tbhits', 'sbhits',
                   'cpuload', 'hashfull', 'time', 'currmove', 'currline',
                   'currmovenumber', 'refutation', 'string', 'ponder',
                   'bestmove')
  }
  tag_names <- unique(tag_names)

  # Parse the position log
  parsed_lines <- lapply(positionlog, parse_engine_line_cpp, tag_names)
  parsed_lines <- lapply(parsed_lines, unlist)
  positionlog_table <- do.call(rbind, parsed_lines)
  colnames(positionlog_table) <- tag_names
  if(delete_blank_lines) {
    positionlog_table <- remove_na_rows_cols(positionlog_table)
  }
  return(as.data.frame(positionlog_table))
}

#' Remove rows and columns that are all NAs from a matrix
#'
#' @param mat A matrix.
#'
#' @return A matrix with rows and columns that are all NAs removed.
remove_na_rows_cols <- function(mat) {
  # Remove rows that are all NAs
  mat <- mat[rowSums(is.na(mat)) != ncol(mat), , drop = FALSE]
  # Remove columns that are all NAs
  mat <- mat[, colSums(is.na(mat)) != nrow(mat), drop = FALSE]
  return(mat)
}

#' Deprecated alias for [rbitr::tabulate_positionlog()]
#' @inherit tabulate_positionlog
#' @export
cram_positionlog <- function(positionlog, all_tags = FALSE, custom_tags = NULL,
                             delete_blank_lines = TRUE) {
  tabulate_positionlog(positionlog, all_tags, custom_tags, delete_blank_lines)
}
