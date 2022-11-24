process_geom_params <- function(column_info) {
  bind_cols(
    column_info %>% select(-options),
    column_info %>% pull(options) %>% map_df(function(l) {
      if (length(l) == 0 || (length(l) == 1) && is.na(l)) {
        tibble(a = 1)[,integer(0)]
      } else {
        as_tibble(l)
      }
    })
  )
}