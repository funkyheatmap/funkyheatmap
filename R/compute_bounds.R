compute_bounds <- function(row_pos, column_pos, segment_data, rect_data, circle_data, funkyrect_data, pie_data, text_data) {
  # determine size of current geoms
  suppressWarnings({
    minimum_x <- min(
      column_pos$xmin,
      segment_data$x,
      segment_data$xend,
      rect_data$xmin,
      circle_data$x - circle_data$r,
      funkyrect_data$x - funkyrect_data$r,
      pie_data$xmin,
      text_data$xmin,
      na.rm = TRUE
    )
    maximum_x <- max(
      column_pos$xmax,
      segment_data$x,
      segment_data$xend,
      rect_data$xmax,
      circle_data$x + circle_data$r,
      funkyrect_data$x + funkyrect_data$r,
      pie_data$xmax,
      text_data$xmax,
      na.rm = TRUE
    )
    minimum_y <- min(
      row_pos$ymin,
      segment_data$y,
      segment_data$yend,
      rect_data$ymin,
      circle_data$y - circle_data$r,
      funkyrect_data$y - funkyrect_data$r,
      pie_data$ymin,
      text_data$ymin,
      na.rm = TRUE
    )
    maximum_y <- max(
      row_pos$ymax,
      segment_data$y,
      segment_data$yend,
      rect_data$ymax,
      circle_data$y + circle_data$r,
      funkyrect_data$y + funkyrect_data$r,
      pie_data$ymax,
      text_data$ymax,
      na.rm = TRUE
    )
  })

  list(
    minimum_x = minimum_x,
    maximum_x = maximum_x,
    minimum_y = minimum_y,
    maximum_y = maximum_y
  )
}
