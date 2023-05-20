calculate_column_positions <- function(
    column_info,
    col_width,
    col_space,
    col_bigspace) {
  column_info %>%
    mutate(
      do_spacing = c(FALSE, diff(as.integer(factor(.data$group))) != 0),
      xsep = case_when(
        .data$overlay ~ c(0, -head(.data$width, -1)),
        .data$do_spacing ~ col_bigspace,
        TRUE ~ col_space
      ),
      xwidth = case_when(
        .data$overlay & .data$width < 0 ~ .data$width - .data$xsep,
        .data$overlay ~ -.data$xsep,
        TRUE ~ .data$width
      ),
      xmax = cumsum(.data$xwidth + .data$xsep),
      xmin = .data$xmax - .data$xwidth,
      x = .data$xmin + .data$xwidth / 2
    )
}
