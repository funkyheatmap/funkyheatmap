#' Defines parameters for positioning in a plot.
#'
#' This function sets parameters that affect positioning within a
#' plot, such as row and column dimensions, annotation details, and the
#' expansion directions of the plot.
#'
#' @param row_height The height of the rows.
#' @param row_space The space between rows.
#' @param row_bigspace The large space between row groups.
#' @param col_width The width of the columns.
#' @param col_space The space between columns.
#' @param col_bigspace The large space between column groups.
#' @param col_annot_offset How much the column annotation will be offset by.
#' @param col_annot_angle The angle of the column annotation labels.
#' @param expand_xmin The minimum expansion of the plot in the x direction.
#' @param expand_xmax The maximum expansion of the plot in the x direction.
#' @param expand_ymin The minimum expansion of the plot in the y direction.
#' @param expand_ymax The maximum expansion of the plot in the y direction.
#'
#' @return A list of plot positioning parameters.
#' @export
#' @examples
#' position_arguments(row_height = 1.2, col_width = 1.5, expand_xmax = 3)
position_arguments <- function(
  row_height = 1,
  row_space = .1,
  row_bigspace = .5,
  col_width = 1,
  col_space = .1,
  col_bigspace = .5,
  col_annot_offset = 3,
  col_annot_angle = 30,
  expand_xmin = 0,
  expand_xmax = 2,
  expand_ymin = 0,
  expand_ymax = 0
) {
  list(
    row_height = row_height,
    row_space = row_space,
    row_bigspace = row_bigspace,
    col_width = col_width,
    col_space = col_space,
    col_bigspace = col_bigspace,
    col_annot_offset = col_annot_offset,
    col_annot_angle = col_annot_angle,
    expand_xmin = expand_xmin,
    expand_xmax = expand_xmax,
    expand_ymin = expand_ymin,
    expand_ymax = expand_ymax
  )
}