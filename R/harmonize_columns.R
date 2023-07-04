#' Harmonize columns in a list of data frames
#'
#' This function takes a list of data frames and adds missing columns to each
#' data frame so that all data frames have the same columns in the same order.
#' Any added columns will be populated with NAs. The function can be used to
#' prepare data frames for an rbind when the columns are not identical.
#'
#' @param df_list A list of data frames
#' @return A list of data frames with harmonized columns
harmonize_columns <- function(df_list) {
  # Find all unique column names across all data frames
  all_colnames <- unique(unlist(lapply(df_list, colnames)))

  # For each data frame, add missing columns with NA values and reorder columns
  df_list <- lapply(df_list, function(df) {
    missing_cols <- setdiff(all_colnames, colnames(df))
    if (length(missing_cols) > 0) {
      missing_df <- as.data.frame(lapply(missing_cols, function(x) rep(NA_real_, nrow(df))))
      colnames(missing_df) <- missing_cols
      df <- cbind(df, missing_df)
    }
    df[, all_colnames]
  })

  return(df_list)
}
