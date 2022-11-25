add_column_if_missing <- function(df, ...) {
  column_values <- list(...)
  for (column_name in names(column_values)) {
    default_val <- rep(column_values[[column_name]], nrow(df))
    
    if (column_name %in% colnames(df)) {
      df[[column_name]] <- ifelse(
        is.na(df[[column_name]]),
        default_val,
        df[[column_name]]
      )
    } else {
      df[[column_name]] <- default_val
    }
  }
  df
}