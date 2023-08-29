calculate_row_positions <- function(row_info, row_height, row_space, row_bigspace) {
  row_pos <-
    row_info %>%
    group_by(.data$group) %>%
    mutate(group_i = row_number()) %>%
    ungroup() %>%
    mutate(
      row_i = row_number(),
      colour_background = .data$group_i %% 2 == 1,
      do_spacing = c(FALSE, diff(as.integer(factor(.data$group))) != 0),
      ysep = ifelse(.data$do_spacing, row_bigspace, row_space),
      y = -(.data$row_i * row_height + cumsum(.data$ysep)),
      ymin = .data$y - row_height / 2,
      ymax = .data$y + row_height / 2
    )
  row_pos
}
