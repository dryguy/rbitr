% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_checkmate.R
\name{is_checkmate}
\alias{is_checkmate}
\title{Check if the current board state is a checkmate}
\usage{
is_checkmate(board, color = board$to_move)
}
\arguments{
\item{board}{A list containing the 8x8 matrix \code{board$board} representing the game state and the color \code{board$to_move} of the player to move next.}

\item{color}{The color of the player to check for checkmate. Default is \code{board$to_move}.}
}
\value{
TRUE if the player of the specified color is in checkmate, FALSE otherwise.
}
\description{
Check if the current board state is a checkmate
}
\examples{
# Starting position
board <- fen_to_board()
is_checkmate(board)
# Fool's mate
board <- fen_to_board("rnbqkbnr/ppppp1pp/8/8/4pP1q/8/PPPPP1PP/RNBQKBNR w KQkq - 0 3")
is_checkmate(board)

}
