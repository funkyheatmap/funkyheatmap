#' Generate a funky heatmaps for benchmarks
#'
#' Allows generating heatmap-like visualisations for benchmark data
#' frames. Funky heatmaps can be fine-tuned by providing annotations of the
#' columns and rows, which allows assigning multiple palettes or geometries
#' or grouping rows and columns together in categories.
#'
#' @param data A data frame with items by row and features in the columns.
#' Must contain one column named `"id"`.
#'
#' @param column_info A data frame describing which columns in `data` to
#' plot. This data frame should contain the following columns:
#'
#' * `id` (`character`): The corresponding column name in `data`.
#'
#' * `name` (`character`): A label for the column. If `NA` or `""`,
#'   no label will be plotted. If this column is missing, `id` will
#'   be used to generate the `name` column.
#'
#' * `geom` (`character`): The geom of the column. Must be one of:
#'   `"funkyrect"`, `"circle"`, `"rect"`, `"bar"`, `"pie"`, `"text"` or `"image"`.
#'   For `"text"`, the corresponding column in `data` must be a `character`.
#'   For `"pie"`, the column must be a list of named numeric vectors.
#'   For all other geoms, the column must be a `numeric`.
#'
#' * `group` (`character`): The grouping id of each column, must match with
#'   `column_groups$group`. If this column is missing or all values are `NA`,
#'   columns are assumed not to be grouped.
#'
#' * `palette` (`character`): Which palette to colour the geom by.
#'   Each value should have a matching value in `palettes$palette`.
#'
#' * `width`: Custom width for this column (default: 1).
#'
#' * `overlay`: Whether to overlay this column over the previous column.
#'     If so, the width of that column will be inherited.
#'
#' * `legend`: Whether or not to add a legend for this column.
#'
#' * `hjust`: Horizontal alignment of the bar, must be between \[0,1\]
#'     (only for `geom = "bar"`).
#'
#' * `hjust`: Horizontal alignment of the label, must be between \[0,1\]
#'     (only for `geom = "text"`).
#'
#' * `vjust`: Vertical alignment of the label, must be between \[0,1\]
#'     (only for `geom = "text"`).
#'
#' * `size`: Size of the label, must be between \[0,1\]
#'     (only for `geom = "text"`).
#'
#' * `label`: Which column to use as a label (only for `geom = "text"`).
#'
#' * `directory`: Which directory to use to find the images (only for `geom = "image"`).
#'
#' * `extension`: The extension of the images (only for `geom = "image"`).
#'
#' * `options` (`list` or `json`): Any of the options above. Any values in this
#'   column will be spread across the other columns. This is useful for
#'   not having to provide a data frame with 1000s of columns.
#'   This column can be a json string.
#'
#' @param row_info A data frame describing the rows of `data`.
#' This data should contain two columns:
#'
#' * `id` (`character`): Corresponds to the column `data$id`.
#'
#' * `group` (`character`): The group of the row.
#'   If all are `NA`, the rows will not be split up into groups.
#'
#' @param column_groups A data frame describing of how to group the columns
#' in `column_info`. Can consist of the following columns:
#'
#' * `group` (`character`): The corresponding group in `column_info$group`.
#' * `palette` (`character`, optional): The palette used to colour the
#'   column group backgrounds.
#' * `level1` (`character`): The label at the highest level.
#' * `level2` (`character`, optional): The label at the middle level.
#' * `level3` (`character`, optional): The label at the lowest level
#'   (not recommended).
#'
#' @param row_groups A data frame describing of how to group the rows
#' in `row_info`. Can consist of the following columns:
#'
#' * `group` (`character`): The corresponding group in `row_info$group`.
#' * `level1` (`character`): The label at the highest level.
#' * `level2` (`character`, optional): The label at the middle level.
#' * `level3` (`character`, optional): The label at the lowest level
#'   (not recommended).
#'
#' @param palettes A named list of palettes. Each entry in `column_info$palette`
#' should have an entry in this object. If an entry is missing, the type
#' of the column will be inferred (categorical or numerical) and one of the
#' default palettes will be applied. Alternatively, the name of one of the
#' standard palette names can be used:
#'
#' * `numerical`: `"Greys"`, `"Blues"`, `"Reds"`, `"YlOrBr"`, `"Greens"`
#' * `categorical`: `"Set3"`, `"Set1"`, `"Set2"`, `"Dark2"`
#' 
#' @param legends A list of legends to add to the plot. Each entry in
#' `column_info$legend` should have a corresponding entry in this object.
#' Each entry should be a list with the following names:
#'  * `palette` (`character`): The palette to use for the legend. Must be
#'   a value in `palettes`.
#'  * `geom` (`character`): The geom of the legend. Must be one of:
#'    `"funkyrect"`, `"circle"`, `"rect"`, `"bar"`, `"pie"`, `"text"`.
#'  * `title` (`character`, optional): The title of the legend. Defaults
#'    to the palette name.
#'  * `enabled` (`logical`, optional): Whether or not to add the legend.
#'    Defaults to `TRUE`.
#'  * `labels` (`character`, optional): The labels to use for the legend.
#'    The defaults depend on the selected geom.
#'  * `size` (`numeric`, optional): The size of the listed geoms.
#'    The defaults depend on the selected geom.
#'  * `color` (`character`, optional): The color of the listed geoms.
#'    The defaults depend on the selected geom.
#'
#' @param position_args Sets parameters that affect positioning within a
#' plot, such as row and column dimensions, annotation details, and the
#' expansion directions of the plot. See `position_arguments()` for more information.
#'
#' @param scale_column Whether or not to apply min-max scaling to each
#' numerical column.
#'
#' @param add_abc Whether or not to add subfigure labels to the different
#' columns groups.
#'
#' @param col_annot_offset DEPRECATED: use `position_args = position_arguments(col_annot_offset = ...)` instead.
#' @param col_annot_angle DEPRECATED: use `position_args = position_arguments(col_annot_angle = ...)` instead.
#' @param expand DEPRECATED: use `position_args = position_arguments(expand_* = ...)` instead.
#'
#' @importFrom ggforce geom_arc_bar geom_circle geom_arc
#' @importFrom cowplot theme_nothing
#' @importFrom patchwork wrap_plots plot_spacer
#'
#' @returns A ggplot. `.$width` and `.$height` are suggested dimensions for
#' storing the plot with [ggsave()].
#'
#' @export
#'
#' @examples
#' library(tibble, warn.conflicts = FALSE)
#'
#' data("mtcars")
#'
#' data <- rownames_to_column(mtcars, "id")
#'
#' funky_heatmap(data)
funky_heatmap <- function(
  data,
  column_info = NULL,
  row_info = NULL,
  column_groups = NULL,
  row_groups = NULL,
  palettes = NULL,
  legends = NULL,
  position_args = position_arguments(),
  scale_column = TRUE,
  add_abc = TRUE,
  col_annot_offset,
  col_annot_angle,
  expand
) {
  # validate input objects
  data <- verify_data(data)
  column_info <- verify_column_info(column_info, data)
  row_info <- verify_row_info(row_info, data)
  column_groups <- verify_column_groups(column_groups, column_info)
  row_groups <- verify_row_groups(row_groups, row_info)
  palettes <- verify_palettes(palettes, column_info, data)
  legends <- verify_legends(legends, palettes, column_info, data)

  # check deprecated arguments
  if (!missing(col_annot_offset)) {
    warning("Argument `col_annot_offset` is deprecated. Use `position_arguments(col_annot_offset = ...)` instead.")
    position_args$col_annot_offset <- col_annot_offset
  }
  if (!missing(col_annot_angle)) {
    warning("Argument `col_annot_angle` is deprecated. Use `position_arguments(col_annot_angle = ...)` instead.")
    position_args$col_annot_angle <- col_annot_angle
  }
  if (!missing(expand)) {
    warning("Argument `expand` is deprecated. Use `position_arguments(expand_* = ...)` instead.")
    for (name in names(expand)) {
      position_args[[paste0("expand_", name)]] <- expand[[name]]
    }
  }
  # todo: add column groups to verify_palettes

  geom_positions <- calculate_geom_positions(
    data,
    column_info,
    row_info,
    column_groups,
    row_groups,
    palettes,
    position_args,
    scale_column,
    add_abc
  )

  main_plot <- compose_ggplot(
    geom_positions,
    position_args
  )

  # start plotting legends
  geom_legend_funs <- list(
    funkyrect = create_funkyrect_legend,
    circle = create_circle_legend,
    rect = create_rect_legend,
    pie = create_pie_legend
    # todo: add text legend
    # todo: add bar legend
  )
  legend_plots <- list()
  for (legend in legends) {
    if (legend$enabled) {
      legend_plot <- geom_legend_funs[[legend$geom]](
        title = legend$title,
        palette = palettes[[legend$palette]],
        labels = legend$labels,
        size = legend$size,
        color = legend$color,
        position_args = position_args
      )
      legend_plots <- c(legend_plots, list(legend_plot))
    }
  }

  legend_widths <- map_dbl(legend_plots, ~ .x$width)
  legend_heights <- map_dbl(legend_plots, ~ .x$height)
  
  heights <- main_plot$height
  width <- main_plot$width
  if (length(legend_plots) > 0) {
    heights <- c(heights, .1, max(legend_heights))
    width <- max(width, sum(legend_widths))
  }
  
  out <- patchwork::wrap_plots(
    main_plot,
    patchwork::plot_spacer(),
    patchwork::wrap_plots(
      legend_plots,
      nrow = 1,
      widths = legend_widths
    ),
    ncol = 1,
    heights = heights
  )

  # TODO: fix this heuristic
  out$width <- width
  out$height <- sum(heights)

  out
}
