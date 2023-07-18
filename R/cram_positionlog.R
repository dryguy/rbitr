#' Creates a data frame containing engine analysis of a single chess position
#'
#' The `cram_positionlog()` function takes a character vector of engine
#'   analysis for a single chess position and condenses it into a data frame for
#'   easier access.
#'
#' @details The `cram_positionlog()` function parses output from a
#'   [UCI](https://github.com/fsmosca/UCIChessEngineProtocol)-compatible chess
#'   engine.
#'
#' @details According to the UCI protocol, when the engine has stopped
#'   searching, it will send the best move, and optionally, which move it will
#'   ponder if told. The `bestmove` line takes the format:
#'
#'  `bestmove <move1> [ponder <move2>]`, where:
#'  - `<move1>` is the best move found by the engine, in long algebraic notation
#'  - `<move2>` is the move the engine expects the opponent may play in response.
#'
#' @details At other times, the engine may send data indicating the engine's
#'   evaluation of the position (and associated principal variation, or pv for
#'   short), the current depth of search, the number of nodes searched, and so
#'   on. For example:
#'
#'   `info depth [a] seldepth [b] score [cp/mate] [c] pv [d]`, where:
#'   - `[a]` is the depth of the search
#'   - `[b]` is the selective depth of the search
#'   - `[cp/mate]` indicates if the score is in centipawns or moves until mate
#'   - `[c]` is the score of the position
#'   - `[d]` is the principal variation
#'
#'  For a list of data that can appear, refer to the
#'  [UCI](https://github.com/fsmosca/UCIChessEngineProtocol) protocol. Not all
#'  types of data may be present in every info line. Some engines may also
#'  provide information that is not covered by the UCI protocol.
#'
#' @details By default, the only data extracted are `depth`, `multipv`, `score`,
#'   and `pv`. Setting the `all_tags` parameter to `TRUE` will extract all of
#'   the info tags listed in the UCI protocol. In cases where the engine
#'   supplies data not covered by the UCI protocol, the user may add custom tag
#'   names as a character vector via the `custom_tags` parameter, but *only* if
#'   the engine's custom tags conform to one of the following formats:
#'
#'   <tag name> <tag value>
#'   <tag name> <move 1> <move 2> ... <move i>
#'
#'   where <tag name>, <tag value>, and <move i> must not contain spaces. Tag
#'   names, tag values and moves must of course be separated from each other by
#'   spaces. Additionally, moves must be in the UCI format. Each <move i> should
#'   consist of two chessboard squares followed by an optional promotion piece,
#'   e.g. e2e4 or e7e8q.
#'
#' @details If any of the requested data does not appear in the search, NA will
#'   appear in the data frame in the place of missing values. If entire rows or
#'   columns are missing values, they will be deleted by default. To leave
#'   missing rows/columns in place, set the `delete_blank_lines` parameter to
#'   FALSE.
#'
#' @param positionlog A character vector of engine analysis
#' @param all_tags (Default = FALSE) A boolean. Setting `all_tags` = TRUE will
#'   search for all of the info tags listed in the UCI protocol
#' @param custom_tags (Default = NULL) An optional character vector of custom
#'   tag names for tags not in the UCI protocol
#' @param delete_blank_lines (Default = TRUE) A boolean. Setting this value to
#'   FALSE will leave blank rows/columns intact
#' @return A data frame summarizing the data for the position
#' @export
#'
#' @seealso
#'   The 'cram' functions condense analysis logs into data frames.
#'   * [rbitr::cram_gamelog()] for condensing analysis of one game.
#'   * [rbitr::cram_pgnlog()] for condensing analysis of an entire pgn file.
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
#' cram_positionlog(positionlog)
cram_positionlog <- function(positionlog, all_tags = FALSE, custom_tags = NULL,
                             delete_blank_lines = TRUE) {
  # Validate input
  assertthat::assert_that(is.character(positionlog))
  assertthat::assert_that(assertthat::is.flag(all_tags))
  assertthat::assert_that(is.character(custom_tags) | is.null(custom_tags))
  assertthat::assert_that(assertthat::is.flag(delete_blank_lines))

  splitlog <- strsplit(positionlog, split = ' ', fixed = TRUE)
  tag_names <- c(custom_tags, 'depth', 'multipv', 'score', 'pv')
  if (all_tags) {
    tag_names <- c(tag_names, 'seldepth', 'nodes', 'nps', 'tbhits', 'sbhits',
                   'cpuload', 'hashfull', 'time', 'currmove', 'currline',
                   'currmovenumber', 'refutation', 'string', 'ponder',
                   'bestmove')
  }
  parse_line <- function(splitlog_line, tag_names) {
    lapply(tag_names, parse_split_engine_line, splitlog_line)
  }

  parsed_lines <- lapply(splitlog, parse_line, tag_names)
  parsed_lines <- lapply(parsed_lines, unlist)
  crammed_positionlog <- do.call(rbind, parsed_lines)
  colnames(crammed_positionlog) <- tag_names
  if(delete_blank_lines) {
    crammed_positionlog <- remove_na_rows_cols(crammed_positionlog)
  }
  return(as.data.frame(crammed_positionlog))
}

#' Helper function for extracting data from UCI chess engine output
#'
#' @details The `parse_split_engine_line()` function is optimized for speed for
#'   large data sets. Input validation should therefore be handled by a parent
#'   function.
#'
#' @details `parse_split_engine_line()` expects a single line of UCI engine
#'   output that has been split using `strsplit()`, with `split` = ' '. It will
#'   return the value immediately following the specified `tag`. If the tag is
#'   not present, it will return NA.
#'
#' @details The [UCI](https://github.com/fsmosca/UCIChessEngineProtocol)
#'   protocol lists a number of tags that may appear in engine output. Most tags
#'   have values that are either numeric, or that are a series of chess moves in
#'   long algebraic notation (LAN):
#'
#'   Numeric tags: depth, seldepth, multipv, time, nodes, currmovenumber,
#'     hashfull, nps, tbhits, sbhits, cpuload
#'   Move tags: bestmove, ponder, pv, currmove, refutation
#'   Special tags: score, string, currline
#'
#'   In the case of move tags, `parse_split_engine_line()` will check for the
#'   next move until it reaches the end of the series, and return the moves as a
#'   string separated by spaces.
#'
#'   The tags `score`, `string`, and `currline` differ from the rest. The
#'   `score` tag has 4 different types of value (<x> indicates numeric):
#'     cp: A score (centipawns), 'cp <x>'
#'     mate: Moves until mate, 'mate <x>'
#'     lowerbound: A lower bound for the score (centipawns), '<x> lowerbound'
#'     upperbound: An upper bound for the score (centipawns), '<x> upperbound'
#'
#'   The `string` tag can have any value, and it runs to the end of the line.
#'
#'   The `currline` has a slightly different format than the other move tags.
#'   The moves may be preceded by a number indicating which cpu generated the
#'   moves. In the case of only 1 cpu, the number may be omitted.
#'
#'   Some engines may use tags not listed in the UCI protocol. The
#'   `parse_split_engine_line()` function may be able to deal with such tags if
#'   they conform to the general format of other UCI tags.
#'
#' @param tag A single-element character vector of a tag name that appears in
#'   the engine output.
#' @param split_engine_line A character vector generated by using `strsplit()`
#'   on a line of UCI engine output.
#'
#' @return A single-element character vector of the value that follows `tag` in
#'   the output of a UCI chess engine.
parse_split_engine_line <- function(tag, split_engine_line) {
  # Find the position of the tag
  tag_index <- which(split_engine_line == tag)

  # First, deal with special cases score and string
  if (length(tag_index) != 0) {
    # If the tag is 'score', return the value and type of score
    if (tag == "score") {
      return(paste(split_engine_line[tag_index + 1],
                   split_engine_line[tag_index + 2]))
    }
    # If tag is 'string', return the value of the string to the end of line
    if (tag == "string") {
      return(
        paste(
          split_engine_line[(tag_index[1] + 1):length(split_engine_line)],
          collapse = " "
        )
      )
    }

    # Next deal with numeric tags
    # Move to the next index after the tag and get the value at that index
    tag_index <- tag_index + 1
    tag_value <- split_engine_line[tag_index]
    # Be careful: currline can look like a numeric value.
    # If value is not a UCI move and tag is not 'currline', return the value
    if (!is_uci_move(tag_value) & !(tag == "currline")) {
      return(tag_value)
    }

    # Next deal with move tags
    length_split_engine_line <- length(split_engine_line)
    # If there are no more values after the current index, return current value
    if (tag_index + 1 > length_split_engine_line) {
      return(tag_value)
    } else {
      # Otherwise, look for the next move in the series; stop when none are left
      # While there are still values in the split engine line
      while (tag_index < length_split_engine_line) {
        # Move to the next index
        tag_index <- tag_index + 1
        # If the value at that index is not a UCI move, return the current value
        if (!is_uci_move(split_engine_line[tag_index])) {
          return(tag_value)
        }
        # Otherwise, add the value at that index to the current value
        tag_value <- paste(tag_value, split_engine_line[tag_index])
      }
      # Return the series of moves
      return(tag_value)
    }
  } else {
    # If the tag is not present, return NA
    return(NA)
  }
}

#' Check if a move is in UCI format
#'
#' The `is_uci_move()` function checks to see if a given move is in the
#'   [UCI](https://github.com/fsmosca/UCIChessEngineProtocol) format. It is an
#'   internal helper function optimized for speed. Input validation should
#'   therefore be handled by a parent function.
#'
#' @details A valid UCI move is 4 or 5 characters long, with the first and third
#'   characters being letters from 'a' to 'h', the second and fourth characters
#'   being numbers from 1 to 8, and an optional fifth character indicating a
#'   promoted piece ('q', 'r', 'b', or 'n').
#'
#' @note The `is_uci_move()` function does *not* check to see if the move is a
#'   legal chess move. It only checks to see of the move consists of two valid
#'   chessboard squares and an optional valid promotion piece.
#'
#' @param move A single-element character string representing a chess move.
#'
#' @return A logical value indicating if the move is in UCI format.
is_uci_move <- function(move) {
  # Check if move is the correct length
  if (nchar(move) < 4 || nchar(move) > 5) {
    return(FALSE)
  }

  # Split move into individual characters
  move_characters <- unlist(strsplit(move, split = '', fixed = TRUE))

  # Check if first and third characters are valid letters
  if (!all(move_characters[c(1,3)] %in% letters[1:8]) > 0) {
    return(FALSE)
  }

  # Check if second and fourth characters are valid numbers
  if (!all(as.integer(move_characters[c(2,4)]) >= 1 &
           as.integer(move_characters[c(2,4)]) <= 8)) {
    return(FALSE)
  }

  # If move has a fifth character, check if it is a valid promotion piece
  if (length(move_characters) == 5 &&
      !move_characters[5] %in% c("q", "r", "b", "n")) {
    return(FALSE)
  }

  TRUE
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
