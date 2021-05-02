get_cpl <- function(movetext, engine, go_mode, go_value) {
  # Validate inpt
  assertthat::assert_that(assertthat::is.string(movetext))
  assertthat::assert_that(class(engine$pipe)[1] == 'process')
  assertthat::assert_that(go_mode == 'depth' |
                            go_mode == 'nodes' |
                            go_mode == 'movetime')
  assertthat::assert_that(assertthat::is.count(go_value))
}
