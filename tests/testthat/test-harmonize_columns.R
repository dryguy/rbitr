test_that("harmonize_columns works as expected", {
  # Create some example data frames
  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(b = letters[4:6], c = 4:6)
  df3 <- data.frame(a = 7:9, c = letters[7:9])

  # harmonize columns
  df_list <- harmonize_columns(list(df1, df2, df3))

  # Check that all data frames have the same columns in the same order
  expect_identical(colnames(df_list[[1]]), colnames(df_list[[2]]))
  expect_identical(colnames(df_list[[1]]), colnames(df_list[[3]]))

  # Check that missing columns were added with NA values
  expect_identical(df_list[[1]]$c, as.double(rep(NA, nrow(df_list[[1]]))))
  expect_identical(df_list[[2]]$a, as.double(rep(NA, nrow(df_list[[2]]))))
})

test_that("harmonize_columns works with empty input", {
  # harmonize columns on an empty list
  df_list <- harmonize_columns(list())

  # Check that the result is an empty list
  expect_identical(df_list, list())
})

test_that("harmonize_columns works with empty data frames", {
  # Create some example data frames
  df1 <- data.frame(a = numeric(0), b = character(0))
  df2 <- data.frame(b = character(0), c = numeric(0))

  # harmonize columns
  df_list <- harmonize_columns(list(df1, df2))

  # Check that all data frames have the same columns in the same order
  expect_identical(colnames(df_list[[1]]), colnames(df_list[[2]]))

  # Check that missing columns were added with the correct type
  expect_identical(class(df_list[[1]]$c), "numeric")
  expect_identical(class(df_list[[2]]$a), "numeric")
})

test_that("harmonize_columns works with data frames with no common columns", {
  # Create some example data frames
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = letters[4:6])

  # harmonize columns
  df_list <- harmonize_columns(list(df1, df2))

  # Check that all data frames have the same columns in the same order
  expect_identical(colnames(df_list[[1]]), colnames(df_list[[2]]))

  # Check that missing columns were added with NA values
  expect_identical(df_list[[1]]$b, as.double(rep(NA, nrow(df_list[[1]]))))
  expect_identical(df_list[[2]]$a, as.double(rep(NA, nrow(df_list[[2]]))))
})

test_that("harmonize_columns works with data frames with different row counts", {
  # Create some example data frames
  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(a = 4:6, b = letters[4:6], c = 7:9)

  # harmonize columns
  df_list <- harmonize_columns(list(df1, df2))

  # Check that all data frames have the same columns in the same order
  expect_identical(colnames(df_list[[1]]), colnames(df_list[[2]]))

  # Check that missing columns were added with NA values
  expect_identical(df_list[[1]]$c, as.double(rep(NA, nrow(df_list[[1]]))))
})

test_that("harmonize_columns works with data frames with different column types", {
  # Create some example data frames
  df1 <- data.frame(a = factor(c("x", "y", "z")), b = c(1,2,3))
  df2 <- data.frame(a = c("a", "b", "c"), b = factor(c("x", "y", "z")))

  # harmonize columns
  df_list <- harmonize_columns(list(df1, df2))

  # Check that all data frames have the same columns in the same order
  expect_identical(colnames(df_list[[1]]), colnames(df_list[[2]]))

  # Check that column types are preserved
  expect_identical(class(df_list[[1]]$a), "factor")
  expect_identical(class(df_list[[2]]$a), "character")
  expect_identical(class(df_list[[1]]$b), "numeric")
  expect_identical(class(df_list[[2]]$b), "factor")
})

