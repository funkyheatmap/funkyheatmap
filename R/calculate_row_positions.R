calculate_row_positions <- function(row_info, row_height, row_space, row_bigspace) {
  row_pos <-
    row_info %>%
    group_by(group) %>%
    mutate(group_i = row_number()) %>%
    ungroup() %>%
    mutate(
      row_i = row_number(),
      colour_background = group_i %% 2 == 1,
      do_spacing = c(FALSE, diff(as.integer(factor(group))) != 0),
      ysep = ifelse(do_spacing, row_height + 2 * row_space, row_space),
      y = - (row_i * row_height + cumsum(ysep)),
      ymin = y - row_height / 2,
      ymax = y + row_height / 2
    )
  row_pos
}