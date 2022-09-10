# Load the locations of the chess engines that will be tested
engines_file_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'engine_paths.R'
)
source(engines_file_path)

test_that("evaluate_position evaluates a chess position", {
  for (engine_path in engine_paths) {
    test <- evaluate_position('e2e4', engine_path, n_pv = 1, n_cpus = 1,
                              limiter = 'depth', limit = 1)
    expect_identical(class(test), 'list')
    expect_identical(test[[1]][2], 'readyok')
  }
})
