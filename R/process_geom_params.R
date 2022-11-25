process_geom_params <- function(column_info) {
  bind_cols(
    column_info %>%
      select(-"options"),
    column_info %>%
      pull(.data$options) %>%
      map_df(function(l) {
        if (length(l) == 0 || (length(l) == 1) && is.na(l)) {
          # return a tibble with one row but no columns
          tibble(a = 1)[,-1]
        } else {
          as_tibble(l)
        }
      })
  )
}