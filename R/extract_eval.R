#' Extract Evaluations from Logs
#'
#' @param evaluations A recursive list of evaluation data.
#' @param root The type of evaluation to extract.
#'
#' @return The extracted evaluations.
#' @export
#'
#' @examples
#' evaluations <- list(list(list(scores = '146', pvs = 'd2d4')))
#' extract_eval(evaluations, 'scores')
#' extract_eval(evaluations, 'pvs')
extract_eval <- function(evaluations, root) {
  extract_root <- function(x, root) {
    unlist(lapply(x, '[[', root), use.names = FALSE)
  }
  lapply(evaluations, extract_root, root)
}
