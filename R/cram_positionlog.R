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
#' @details `cram_positionlog()` uses regular expressions to extract data from
#'   the engine's logs. By default, the only data extracted are `depth`,
#'   `multipv`, `score`, and `pv`. Setting the `all` parameter to
#'   TRUE will extract all of the info tokens listed in the UCI protocol. In
#'   cases where the engine supplies data not covered by the UCI protocol, the
#'   user may add custom regular expressions as a character vector via the
#'   `patterns` parameter. The names for these additional columns may be
#'   specified with the `column_names` parameter, which should be a character
#'   vector of equal length to `patterns`.
#'
#' @details If any of the requested data does not appear in search, NA will
#'   appear in the data frame in the place of missing values. If entire rows or
#'   columns are missing values, they will be deleted by default. To leave
#'   missing rows/columns in place, set the `delete_blank_lines` parameter to
#'   FALSE.
#'
#' @param positionlog A character vector of engine analysis
#' @param all (Default = FALSE) A boolean. Setting `all` = TRUE will search for
#'   all of the info tokens listed in the UCI protocol
#' @param patterns (Default = NULL) An optional character vector of one or more
#'   user-supplied regular expressions
#' @param column_names (Default = NULL) An optional character vector of column
#'   names for data extracted by the user-supplied regular expressions
#' @param delete_blank_lines (Default = TRUE) A boolean. Setting this value to
#'   FALSE will leave blank rows/columns intact
#' @return A data frame summarizing the data for the position
#' @export
#'
#' @seealso
#'   The 'cram' functions condense analysis logs into data frames.
#'   * [rbitr::cram_gamelog()] for condensing analysis of one game.
#'   * [rbitr::cram_pgnlog()] for condensing analysis of an entire pgn file.
#'   The 'parse' functions extract specific data from analysis logs.
#'   * [rbitr::parse_gamelog()] for extracting data from one evaluated game.
#'   * [rbitr::parse_pgnlog()] for extracting data from games in a pgn.
#'   The 'evaluate' functions produce analysis logs.
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
cram_positionlog <- function(positionlog, all = FALSE, patterns = NULL,
                             column_names = NULL, delete_blank_lines = TRUE) {
  # Validate input
  assertthat::assert_that(is.character(positionlog))
  assertthat::assert_that(assertthat::is.flag(all))
  assertthat::assert_that(is.character(patterns) | is.null(patterns))
  assertthat::assert_that(is.character(column_names) | is.null(column_names))
  assertthat::assert_that(length(column_names) == length(patterns))
  assertthat::assert_that(assertthat::is.flag(delete_blank_lines))

  # Set up regex patterns (defaults are depth, score, pv, and bestmove)
  # UCI info tokens are depth, seldepth, time, nodes, pv, multipv, score,
  #   currmove, currmovenumber, hashfull, nps, tbhits, sbhits, cpuload, string,
  #   refutation, currline, (bestmove, ponder)
  patterns <- c(patterns,
                '(?<=\\bdepth\\s)\\s*(\\d+)',
                '(?<=\\bmultipv\\s)\\s*(\\d+)',
                '(?<=\\bscore\\s)\\s*(\\w+\\s+-?\\d+)',
                '(?<=\\bpv\\s)\\s*([a-h][1-8][a-h][1-8][a-z]?\\s*)*')
  column_names <- c(column_names, 'depth', 'multipv', 'score', 'pv')
  if (all) {
    patterns <- c(patterns,
                  '(?<=\\bseldepth\\s)\\s*(\\d+)',
                  '(?<=\\bnodes\\s)\\s*(\\d+)',
                  '(?<=\\bnps\\s)\\s*(\\d+)',
                  '(?<=\\btbhits\\s)\\s*(\\d+)',
                  '(?<=\\bsbhits\\s)\\s*(\\d+)',
                  '(?<=\\bcpuload\\s)\\s*(\\d+)',
                  '(?<=\\bhashfull\\s)\\s*(\\d+)',
                  '(?<=\\btime\\s)\\s*(\\d+)',
                  '(?<=\\bcurrmove\\s)\\s*([a-h][1-8][a-h][1-8][a-z]?\\s*)*',
                  '(?<=\\bcurrline\\s)\\s*([a-h][1-8][a-h][1-8][a-z]?\\s*)*',
                  '(?<=\\bcurrmovenumber\\s)\\s*(\\d+)',
                  '(?<=\\brefutation\\s)\\s*([a-h][1-8][a-h][1-8][a-z]?\\s*)*',
                  '(?<=\\bstring\\s)\\s*(.*)',
                  '(?<=\\bponder\\s)\\s*([a-h][1-8][a-h][1-8][a-z]?\\s*)*',
                  '(?<=\\bbestmove\\s)\\s*([a-h][1-8][a-h][1-8][a-z]?\\s*)*')
    column_names <- c(column_names, 'seldepth', 'nodes', 'nps', 'tbhits',
                      'sbhits', 'cpuload', 'hashfull', 'time', 'currmove',
                      'currline', 'currmovenumber', 'refutation', 'string',
                      'ponder', 'bestmove')
  }

  # Helper function to parse individual lines
  parse_lines <- function(line_index, positionlog, patterns, column_names,
                          pattern_indices) {

    # Extract tags using regular expressions
    cycle_patterns <- function(pattern_index, positionlog, line_index, pattern,
                               column_names) {
      result <- stringr::str_match(positionlog[[line_index]],
                                   pattern[[pattern_index]])[, 1]
      return(result)
    }
    result <- vapply(pattern_indices, cycle_patterns, character(1), positionlog,
                     line_index, patterns, column_names)

    # Return a data frame with the parsed information
    names(result) <- column_names
    return(result)
  }

  # Apply the helper function to all lines
  line_indices <- 1:length(positionlog)
  pattern_indices <- 1:length(patterns)
  result <- do.call(rbind, lapply(line_indices, parse_lines, positionlog,
                                  patterns, column_names, pattern_indices))

  # Clean up the data frame.
  result <- as.data.frame(apply(result, 2, trimws))
  result <- result[apply(result, MARGIN = 1, function(x) any(!is.na(x))), ]
  result <- result[, colSums(is.na(result)) != nrow(result)]
  result <- as.data.frame(result)

  suppressWarnings(result <- convert_to_numeric(result))
  return(result)
}

#' Convert character columns containing only numeric data to numeric
#'
#' This is a helper function for `cram_positionlog()`. It assumes that the data
#'   frame can only have numeric or character columns (i.e., no factors). It
#'   looks for columns where every string contains numeric data, and coverts the
#'   them to numeric. If the column contains any non-numeric or mixed values, it
#'   will not be converted.
#'
#' @param df A data frame
#' @return A converted data frame
convert_to_numeric <- function(df) {
  for (i in 1:ncol(df)) {
    if (all(is.na(as.numeric(df[, i])))) next
    df[, i] <- as.numeric(df[, i])
  }
  return(df)
}







