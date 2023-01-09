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
#'   `"funkyrect"`, `"circle"`, `"rect"`, `"bar"`, `"pie"`, or `"text"`.
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
#'
#' @param scale_column Whether or not to apply min-max scaling to each
#' numerical column.
#' 
#' @param add_abc Whether or not to add subfigure labels to the different
#' columns groups.
#' 
#' @param col_annot_offset How much the column annotation will be offset by.
#' @param row_annot_offset How much the row annotation will be offset by.
#' @param removed_entries Which methods to not show in the rows. Missing methods
#' are replaced by a "Not shown" label.
#' 
#' @param expand A list of directions to expand the plot in.
#'
#' @importFrom ggforce geom_arc_bar geom_circle geom_arc
#' @importFrom cowplot theme_nothing
#'
#' @returns A ggplot. `.$width` and `.$height` are suggested dimensions for
#' storing the plot with [ggsave()].
#' 
#' @export
#' 
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' library(tibble, warn.conflicts = FALSE)
#' 
#' data("mtcars")
#' 
#' data <- mtcars %>%
#'   rownames_to_column("id")
funky_heatmap <- function(
  data,
  column_info = NULL,
  row_info = NULL,
  column_groups = NULL,
  row_groups = NULL,
  palettes = NULL,
  scale_column = TRUE,
  add_abc = TRUE,
  col_annot_offset = 3,
  row_annot_offset = .5,
  removed_entries = NULL,
  expand = c(xmin = 0, xmax = 2, ymin = 0, ymax = 0)
) {
  # no point in making these into parameters
  row_height <- 1
  row_space <- .1
  row_bigspace <- .5
  col_width <- 1
  col_space <- .1
  col_bigspace <- .5
  
  # validate input objects
  data <- verify_data(data)
  column_info <- verify_column_info(column_info, data)
  row_info <- verify_row_info(row_info, data)
  column_groups <- verify_column_groups(column_groups, column_info)
  row_groups <- verify_row_groups(row_groups, row_info)
  palettes <- verify_palettes(palettes, column_info, data)
  # todo: add column groups to verify_palettes

  # DETERMINE ROW POSITIONS
  if (!"group" %in% colnames(row_info) || all(is.na(row_info$group))) {
    row_info$group <- ""
    row_groups <- tibble(group = "")
    plot_row_annotation <- FALSE
  } else {
    plot_row_annotation <- TRUE
  }

  row_pos <- calculate_row_positions(
    row_info,
    row_height = row_height,
    row_space = row_space,
    row_bigspace = row_bigspace
  )

  # DETERMINE COLUMN POSITIONS
  if (!"group" %in% colnames(column_info) || all(is.na(column_info$group))) {
    column_info$group <- ""
    plot_column_annotation <- FALSE
  } else {
    plot_column_annotation <- TRUE
  }

  column_pos <- calculate_column_positions(
    column_info,
    col_width = col_width,
    col_space = col_space,
    col_bigspace = col_bigspace
  )

  ####################################
  ###         PROCESS DATA         ###
  ####################################
  geom_data_processor <- make_geom_data_processor(
    data,
    column_pos,
    row_pos,
    scale_column,
    palettes
  )

  # gather segment data
  segment_data <- NULL # placeholder for if this ever gets used

  # gather circle data
  circle_data <- geom_data_processor("circle", function(dat) {
    dat %>% mutate(
      x0 = .data$x,
      y0 = .data$y,
      r = row_height / 2 * .data$value
    )
  })

  # gather rect data
  rect_data <- geom_data_processor("rect", identity)

  # gather funkyrect data
  funkyrect_data <- geom_data_processor("funkyrect", function(dat) {
    dat %>%
      select("xmin", "xmax", "ymin", "ymax", "value") %>%
      pmap_df(score_to_funky_rectangle, midpoint = .8)
  })

  # gather bar data
  bar_data <- geom_data_processor("bar", function(dat) {
    dat %>%
      add_column_if_missing(hjust = 0) %>%
      mutate(
        xmin = .data$xmin + (1 - .data$value) * .data$xwidth * .data$hjust,
        xmax = .data$xmax - (1 - .data$value) * .data$xwidth * (1 - .data$hjust)
      )
  })

  # gather bar guides data
  barguides_data <- geom_data_processor("bar", function(dat) {
    crossing(
      dat %>%
        group_by(.data$column_id) %>%
        slice(1) %>%
        ungroup() %>%
        select(.data$xmin, .data$xmax) %>%
        gather("col", "x") %>%
        transmute(.data$x, xend = .data$x),
      row_pos %>%
        select(y = .data$ymin, yend = .data$ymax)
    ) %>%
      mutate(palette = NA, value = NA)
  })
  segment_data <-
    bind_rows(
      segment_data,
      barguides_data %>% mutate(
        colour = "black",
        size = .5,
        linetype = "dashed"
      )
    )

  # gather text data
  text_data <- geom_data_processor("text", function(dat) {
    dat %>% mutate(colour = "black")
    # colour "black" is overridden if !is.na(palette)
  })

  # gather pie data
  pie_data <- geom_data_processor("pie", function(dat) {
    dat <-
      inner_join(
        dat %>% select(-"value") %>% mutate(iii = row_number()),
        dat %>% select("value") %>% mutate(iii = row_number()) %>%
          dynutils::mapdf_dfr(function(l) {
            enframe(l$value) %>% mutate(iii = l$iii)
          }),
        by = "iii"
      ) %>%
      select(-"iii")

    dat %>%
      group_by(.data$row_id, .data$column_id) %>%
      mutate(
        y0 = .data$y,
        x0 = .data$x,
        pct = ifelse(is.finite(.data$value), .data$value, 0),
        pct = .data$pct / sum(.data$pct),
        rad = .data$pct * 2 * pi,
        rad_end = cumsum(.data$rad),
        rad_start = .data$rad_end - .data$rad,
        r0 = 0,
        r = row_height / 2,
        value = .data$name
      ) %>%
      filter(.data$rad_end != .data$rad_start, 1e-10 <= .data$pct) %>%
      ungroup()
  })

  # would be better to have a generic solution for this
  # # hidden feature trajectory plots
  # trajd <- geom_data_processor("traj", function(dat) {
  #   dat %>% mutate(
  #     topinf = gsub("^gray_", "", value),
  #     colour = ifelse(grepl("^gray_", value), "#AAAAAA", NA)
  #   )
  # })

  ####################################
  ###  ADD ANNOTATION TO GEOM DATA ###
  ####################################

  # GENERATE ROW ANNOTATION
  if (plot_row_annotation) {
    row_annotation <-
      row_groups %>%
      gather("level", "name", -"group") %>%
      left_join(row_pos %>% select("group", "ymin", "ymax"), by = "group") %>%
      group_by(.data$name) %>%
      summarise(
        ymin = min(.data$ymin),
        ymax = max(.data$ymax),
        y = (.data$ymin + .data$ymax) / 2
      ) %>%
      ungroup() %>%
      mutate(xmin = -.5, xmax = 5) %>%
      filter(!is.na(.data$name), .data$name != "")

    text_data <- text_data %>% bind_rows(
      row_annotation %>%
        transmute(
          .data$xmin,
          .data$xmax,
          ymin = .data$ymax + row_space,
          label_value = gsub("\n", " ", .data$name),
          hjust = 0,
          vjust = .5,
          fontface = "bold"
        ) %>%
        mutate(ymax = .data$ymin + row_height)
    )
  }

  # GENERATE COLUMN ANNOTATION
  if (plot_column_annotation) {
    col_join <- column_groups %>%
      gather("level", "name", -"group", -"palette") %>%
      left_join(column_pos %>% select("group", "xmin", "xmax"), by = "group")

    text_pct <- .9
    level_heights <-
      col_join %>%
      group_by(.data$level) %>%
      summarise(max_newlines = max(str_count(.data$name, "\n"))) %>%
      mutate(
        height = (.data$max_newlines + 1) * text_pct + (1 - text_pct),
        levelmatch = match(.data$level, colnames(column_groups))
      ) %>%
      arrange(desc(.data$levelmatch)) %>%
      mutate(
        ysep = row_space,
        ymax = col_annot_offset + cumsum(.data$height + .data$ysep) - .data$ysep,
        ymin = .data$ymax - .data$height,
        y = (.data$ymin + .data$ymax) / 2
      )
    
    palette_mids <- map_chr(palettes, function(x) x[round(length(x)/2)])

    column_annotation <-
      col_join %>%
      group_by(.data$level) %>%
      mutate(max_newlines = max(str_count(.data$name, "\n"))) %>%
      group_by(.data$level, .data$name, .data$palette) %>%
      summarise(
        xmin = min(.data$xmin),
        xmax = max(.data$xmax),
        x = (.data$xmin + .data$xmax) / 2
      ) %>%
      ungroup() %>%
      left_join(level_heights, by = "level") %>%
      filter(!is.na(.data$name), grepl("[A-Za-z]", .data$name)) %>%
      # mutate(colour = palettes$column_annotation[palette])
      mutate(colour = palette_mids[.data$palette])
      # todo: change colour depending on level height?

    rect_data <- rect_data %>% bind_rows(
      column_annotation %>%
        transmute(
          .data$xmin,
          .data$xmax,
          .data$ymin,
          .data$ymax,
          .data$colour,
          alpha = ifelse(.data$levelmatch == 1, 1, .25),
          border = FALSE
        )
    )

    text_data <- text_data %>% bind_rows(
      column_annotation %>%
        transmute(
          .data$xmin,
          .data$xmax,
          .data$ymin,
          .data$ymax,
          hjust = 0.5,
          vjust = 0.5,
          fontface = ifelse(.data$levelmatch == 1, "bold", NA),
          colour = ifelse(.data$levelmatch == 1, "white", "black"),
          label_value = .data$name
        )
    )

    if (add_abc) {
      text_data <- text_data %>% bind_rows(
        column_annotation %>%
          filter(.data$levelmatch == 1) %>%
          arrange(.data$x) %>%
          transmute(
            xmin = .data$xmin + col_space,
            xmax = .data$xmax - col_space,
            .data$ymin,
            .data$ymax,
            hjust = 0,
            vjust = 0.5,
            fontface = "bold",
            colour = "white",
            label_value = paste0(letters[row_number()], ")")
          )
      )
    }
  }

  # ADD COLUMN NAMES
  df <- column_pos %>% filter(.data$name != "")
  if (nrow(df) > 0) {
    segment_data <- segment_data %>% bind_rows(
      df %>% transmute(
        x = .data$x,
        xend = .data$x,
        y = -.3,
        yend = -.1,
        size = .5
      )
    )
    text_data <-
      bind_rows(
        text_data,
        df %>% transmute(
          xmin = .data$xmin,
          xmax = .data$xmax,
          ymin = 0,
          ymax = col_annot_offset,
          angle = 30,
          vjust = 0,
          hjust = 0,
          label_value = .data$name
        )
      )
  }

  # determine dsize of current geoms
  suppressWarnings({
    minimum_x <- min(column_pos$xmin, segment_data$x, segment_data$xend, rect_data$xmin, circle_data$x - circle_data$r, funkyrect_data$x - funkyrect_data$r, pie_data$xmin, text_data$xmin, na.rm = TRUE)
    maximum_x <- max(column_pos$xmax, segment_data$x, segment_data$xend, rect_data$xmax, circle_data$x + circle_data$r, funkyrect_data$x + funkyrect_data$r, pie_data$xmax, text_data$xmax, na.rm = TRUE)
    minimum_y <- min(row_pos$ymin, segment_data$y, segment_data$yend, rect_data$ymin, circle_data$y - circle_data$r, funkyrect_data$y - funkyrect_data$r, pie_data$ymin, text_data$ymin, na.rm = TRUE)
    maximum_y <- max(row_pos$ymax, segment_data$y, segment_data$yend, rect_data$ymax, circle_data$y + circle_data$r, funkyrect_data$y + funkyrect_data$r, pie_data$ymax, text_data$ymax, na.rm = TRUE)
  })

  ####################################
  ###   CREATE HARDCODED LEGENDS   ###
  ####################################

  legend_pos <- minimum_y

  # CREATE LEGENDS
  if (any(column_pos$geom == "pie")) {
    rel_cols <- column_pos %>%
      filter(.data$geom == "pie") %>%
      arrange(.data$x) %>%
      group_by(.data$palette) %>%
      slice(1) %>%
      ungroup()

    for (i in seq_len(nrow(rel_cols))) {
      palette <- palettes[[rel_cols$palette[[i]]]]

      pie_minimum_x <- rel_cols$xmin[[i]]

      pie_legend_df <-
        palette %>%
        enframe(value = "fill") %>%
        mutate(
          rad_start = seq(0, pi, length.out = n() + 1) %>% head(-1),
          rad_end = seq(0, pi, length.out = n() + 1) %>% tail(-1),
          rad = (.data$rad_start + .data$rad_end) / 2,
          colour = rep("black", length(.data$rad)),
          lab_x = row_height * sin(.data$rad),
          lab_y = seq(
            row_height * (cos(first(.data$rad)) + .2),
            row_height * (cos(last(.data$rad)) - .2),
            length.out = n()
          ),
          hjust = rep(0, length(.data$rad)),
          xpt = row_height * sin(.data$rad),
          ypt = row_height * cos(.data$rad),
          vjust = .5
        )

      pie_title_data <-
        tibble(
          xmin = pie_minimum_x,
          xmax = pie_minimum_x,
          ymin = legend_pos - 1.5,
          ymax = legend_pos - .5,
          label_value = "Error reason",
          hjust = 0,
          vjust = 1,
          fontface = "bold"
        )

      pie_pie_data <-
        pie_legend_df %>%
        transmute(
          x0 = pie_minimum_x,
          y0 = legend_pos - 2.75,
          r0 = 0,
          r = row_height * .75,
          .data$rad_start,
          .data$rad_end,
          colour = .data$fill
        )

      pie_text_data <-
        pie_legend_df %>%
        transmute(
          x = pie_minimum_x + .5 + .data$lab_x,
          y = legend_pos - 2.75 + .data$lab_y,
          label_value = as.character(.data$name),
          .data$vjust,
          .data$hjust,
          .data$colour
        ) %>%
        mutate(
          xmin = .data$x,
          xmax = .data$x,
          ymin = .data$y - .4,
          ymax = .data$y + .4
        )

      pie_seg_data <-
        pie_legend_df %>%
        transmute(
          x = pie_minimum_x + .data$xpt * .85,
          xend = pie_minimum_x + .data$xpt * 1.1,
          y = legend_pos - 2.75 + .data$ypt * .85,
          yend = legend_pos - 2.75 + .data$ypt * 1.1
        )

      text_data <- text_data %>% bind_rows(
        pie_title_data,
        pie_text_data
      )

      pie_data <- pie_data %>% bind_rows(
        pie_pie_data
      )

      segment_data <- segment_data %>% bind_rows(
        pie_seg_data
      )
    }
  }

  # funkyrect legend
  if (any(column_pos$geom == "funkyrect")) {
    fr_minimum_x <- column_pos %>% filter(.data$geom == "funkyrect") %>% pull(.data$xmin) %>% min

    fr_legend_size <- 1
    fr_legend_space <- .2

    fr_legend_dat1 <-
      tibble(
        value = seq(0, 1, by = .1),
        xmin = 0,
        xmax = col_width * fr_legend_size,
        ymin = 0,
        ymax = col_width * fr_legend_size
      )

    fr_poly_data1 <-
      fr_legend_dat1 %>%
      select("xmin", "xmax", "ymin", "ymax", "value") %>%
      pmap_df(score_to_funky_rectangle, midpoint = .8)

    fr_legend_dat2 <-
      fr_poly_data1 %>%
      filter(!is.na(.data$r)) %>%
      group_by(.data$value) %>%
      summarise(
        minx = min(.data$x - .data$r),
        maxx = max(.data$x + .data$r),
        miny = min(.data$y - .data$r),
        maxy = max(.data$y + .data$r)
      ) %>%
      mutate(
        width = .data$maxx - .data$minx,
        height = .data$maxy - .data$miny,
        xmin = cumsum(.data$width + fr_legend_space) - .data$width - fr_legend_space,
        xmin = fr_minimum_x + .data$xmin - min(.data$xmin),
        xmax = .data$xmin + .data$width,
        ymin = legend_pos - 2.5,
        ymax = .data$ymin + .data$height
      ) %>%
      transmute(
        .data$width,
        .data$height,
        .data$value,
        .data$xmin,
        .data$xmax,
        .data$ymin,
        .data$ymax,
        x = (.data$xmin + .data$xmax) / 2,
        y = (.data$ymin + .data$ymax) / 2
      )

    fr_maximum_x <- max(fr_legend_dat2$xmax)

    # use grey palette for generic funkyrect legend
    grey_palette <- default_palettes$numerical$Greys
    fr_poly_data2 <-
      transmute(
        fr_legend_dat2,
        xmin = .data$x - fr_legend_size / 2,
        xmax = .data$x + fr_legend_size / 2,
        ymin = .data$y - fr_legend_size / 2,
        ymax = .data$y + fr_legend_size / 2,
        .data$value
      ) %>%
      pmap_df(score_to_funky_rectangle, midpoint = .8) %>%
      mutate(
        col_value = round(.data$value * (length(grey_palette) - 1)) + 1,
        colour = ifelse(
          is.na(.data$col_value),
          "#444444FF",
          grey_palette[.data$col_value]
        )
      )

    fr_title_data <-
      tibble(
        xmin = fr_minimum_x,
        xmax = fr_maximum_x,
        ymin = legend_pos - 1.5,
        ymax = legend_pos - .5,
        label_value = "Score",
        hjust = 0,
        vjust = 1,
        fontface = "bold"
      )

    fr_value_data <-
      fr_legend_dat2 %>% 
        filter(.data$value %% .2 == 0) %>%
        transmute(
          ymin = .data$ymin - 1,
          ymax = .data$ymin,
          .data$xmin,
          .data$xmax,
          hjust = .5,
          vjust = 0,
          label_value = ifelse(
            .data$value %in% c(0, 1),
            sprintf("%.0f", .data$value),
            sprintf("%.1f", .data$value)
          )
        )

    text_data <- bind_rows(
      text_data,
      fr_title_data,
      fr_value_data
    )
    funkyrect_data <- bind_rows(funkyrect_data, fr_poly_data2)
  }

  # TODO: remove hard coded column id
  if (any(column_pos$id == "method_priors_required_str")) {
    pr_minimum_x <- column_pos %>%
      filter(.data$id == "method_priors_required_str") %>%
      pull(.data$xmin) %>%
      min

    legend_vals <- tribble(
      ~symbol, ~value,
      "", "None",
      "\u2715", "Weak: Start or end cells",
      "\u2716", "Strong: Cell grouping or time course"
    )

    pr_labels_df <-
      legend_vals %>%
      mutate(
        lab_x1 = pr_minimum_x,
        lab_x2 = pr_minimum_x + 1,
        lab_y = legend_pos - 1 - row_number() * row_height * .9
      )

    pr_text_data <-
      bind_rows(
        tibble(
          xmin = pr_minimum_x,
          xmax = pr_minimum_x,
          ymin = legend_pos - 1.5,
          max = legend_pos - .5,
          label_value = "Prior information required",
          hjust = 0,
          vjust = 1,
          fontface = "bold"
        ),
        pr_labels_df %>% transmute(
          xmin = .data$lab_x1,
          xmax = .data$lab_x1,
          ymin = .data$lab_y,
          ymax = .data$lab_y,
          label_value = .data$symbol,
          hjust = 0,
          vjust = 0
        ),
        pr_labels_df %>% transmute(
          xmin = .data$lab_x2,
          xmax = .data$lab_x2,
          ymin = .data$lab_y,
          ymax = .data$lab_y,
          label_value = .data$value,
          hjust = 0,
          vjust = 0
        )
      )

    text_data <- text_data %>% bind_rows(
      pr_text_data
    )
  }

  if (!is.null(removed_entries)) {
    # rm_min_x <- column_pos %>% filter(!group %in% c("method_characteristic", "inferrable_trajtype", "benchmark_metric", "benchmark_source")) %>% slice(1) %>% pull(xmin)
    rm_min_x <- 20

    num_cols <- 2
    num_rows <- ceiling(length(removed_entries) / num_cols)

    rm_lab_df <-
      tibble(label_value = removed_entries) %>%
      mutate(
        row = (row_number() - 1) %% num_rows,
        col = ceiling(row_number()  / num_rows) - 1,
        x = rm_min_x + col * 5,
        y = legend_pos - (row + 2) * row_height * .9
      )
    rm_text_data <-
      bind_rows(
        tibble(
          xmin = rm_min_x,
          xmax = rm_min_x,
          ymin = legend_pos - 1.5,
          ymax = legend_pos - .5,
          label_value = "Not shown, insufficient data points",
          hjust = 0,
          vjust = 1,
          fontface = "bold"
        ),
        rm_lab_df %>% mutate(
          xmin = .data$x,
          xmax = .data$x,
          ymin = .data$y,
          ymax = .data$y,
          hjust = 0,
          vjust = 0
        )
      )

    text_data <- text_data %>% bind_rows(
      rm_text_data
    )
  }

  minimum_y <- min(minimum_y, min(text_data$ymin, na.rm = TRUE))

  ####################################
  ###    SIMPLIFY CERTAIN GEOMS    ###
  ####################################

  # small funkyrects are circles
  if (nrow(funkyrect_data) > 0) {
    funkyrect_data <- funkyrect_data %>% mutate(
      is_circle = !is.na(.data$start) & 
        .data$start < 1e-10 & 2 * pi - 1e-10 < .data$end
    )
    circle_data <- circle_data %>% bind_rows(
      funkyrect_data %>%
        filter(.data$is_circle) %>%
        select(
          x0 = "x",
          y0 = "y",
          "r",
          "colour"
        )
    )
    funkyrect_data <- funkyrect_data %>% filter(!.data$is_circle)
  }

  # bars are rects
  rect_data <-
    bind_rows(
      rect_data,
      bar_data
    )
  bar_data <- NULL

  # 100% pies are circles
  if (nrow(pie_data) > 0) {
    pie_data <- pie_data %>% mutate(
      pct = (.data$rad_end - .data$rad_start) / 2 / pi
    )
    # plot 100% pies as circles
    circle_data <- bind_rows(
      circle_data,
      pie_data %>% filter(.data$pct >= (1 - 1e-10))
    )
    pie_data <- pie_data %>% filter(.data$pct < (1 - 1e-10))
  }

  ####################################
  ###         COMPOSE PLOT         ###
  ####################################

  g <-
    ggplot() +
    coord_equal(expand = FALSE) +
    scale_alpha_identity() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_size_identity() +
    scale_linetype_identity() +
    cowplot::theme_nothing()

  # PLOT ROW BACKGROUNDS
  df <- row_pos %>% filter(.data$colour_background)
  if (nrow(df) > 0) {
    g <- g + geom_rect(
      aes(
        xmin = min(column_pos$xmin) - .25,
        xmax = max(column_pos$xmax) + .25,
        ymin = .data$ymin - (row_space / 2),
        ymax = .data$ymax + (row_space / 2)
      ),
      df, 
      fill = "#DDDDDD"
    )
  }

  # PLOT SEGMENTS
  if (nrow(segment_data) > 0) {
    # add defaults for optional values
    segment_data <- segment_data %>% add_column_if_missing(
      size = .5,
      colour = "black",
      linetype = "solid"
    )

    g <- g + geom_segment(
      aes(
        x = .data$x,
        xend = .data$xend,
        y = .data$y,
        yend = .data$yend,
        size = .data$size,
        colour = .data$colour,
        linetype = .data$linetype
      ),
      segment_data
    )
  }

  # PLOT RECTANGLES
  if (nrow(rect_data) > 0) {
    # add defaults for optional values
    rect_data <- rect_data %>%
      add_column_if_missing(
        alpha = 1,
        border = TRUE,
        border_colour = "black"
      ) %>%
      mutate(
        border_colour = ifelse(.data$border, .data$border_colour, NA_character_)
      )

    g <- g + geom_rect(
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$colour,
        colour = .data$border_colour,
        alpha = .data$alpha
      ),
      rect_data,
      size = .25
    )
  }

  # PLOT CIRCLES
  if (nrow(circle_data) > 0) {
    g <- g + ggforce::geom_circle(
      aes(
        x0 = .data$x0,
        y0 = .data$y0,
        fill = .data$colour,
        r = .data$r
      ),
      circle_data,
      size = .25
    )
  }

  # PLOT FUNKY RECTANGLES
  if (nrow(funkyrect_data) > 0) {
    # there are polygons and there are quarter-circles to be plotted
    # it's possible to distinguish one from another by looking at the 'r' column
    funky_poly_data <- funkyrect_data %>% filter(is.na(.data$r))
    funky_arc_data <- funkyrect_data %>% filter(!is.na(.data$r))

    g <- g +
      # plot polygon fill
      geom_polygon(
        aes(
          .data$x,
          .data$y,
          group = .data$name,
          fill = .data$colour
        ),
        funky_poly_data
      ) +
      # plot quarter circle fill
      ggforce::geom_arc_bar(
        aes(
          x0 = .data$x,
          y0 = .data$y,
          r0 = 0,
          r = .data$r,
          start = .data$start,
          end = .data$end,
          fill = .data$colour
        ),
        funky_arc_data,
        colour = NA
      ) +
      # plot polygon border
      geom_path(
        aes(
          x = .data$x,
          y = .data$y,
          group = paste0(.data$name, "_", .data$subgroup)
        ),
        funky_poly_data,
        colour = "black",
        size = .25
      ) +
      # plot quarter circle border
      ggforce::geom_arc(
        aes(
          x0 = .data$x,
          y0 = .data$y,
          r = .data$r,
          start = .data$start,
          end = .data$end
        ),
        funky_arc_data,
        colour = "black",
        size = .25
      )
  }

  # PLOT PIES
  if (nrow(pie_data) > 0) {
    g <- g + ggforce::geom_arc_bar(
      aes(
        x0 = .data$x0,
        y0 = .data$y0,
        r0 = .data$r0,
        r = .data$r,
        start = .data$rad_start,
        end = .data$rad_end,
        fill = .data$colour
      ),
      data = pie_data,
      size = .25
    )
  }

  # PLOT TEXT
  if (nrow(text_data) > 0) {
    # add defaults for optional values
    text_data <- text_data %>%
      add_column_if_missing(
        hjust = .5,
        vjust = .5,
        size = 4,
        fontface = "plain",
        colour = "black",
        lineheight = 1,
        angle = 0
      ) %>%
      mutate(
        angle2 = .data$angle / 360 * 2 * pi,
        cosa = cos(.data$angle2) %>% round(2),
        sina = sin(.data$angle2) %>% round(2),
        alphax =
          ifelse(.data$cosa < 0, 1 - .data$hjust, .data$hjust) * abs(.data$cosa) +
          ifelse(.data$sina > 0, 1 - .data$vjust, .data$vjust) * abs(.data$sina),
        alphay =
          ifelse(.data$sina < 0, 1 - .data$hjust, .data$hjust) * abs(.data$sina) +
          ifelse(.data$cosa < 0, 1 - .data$vjust, .data$vjust) * abs(.data$cosa),
        x = (1 - .data$alphax) * .data$xmin + .data$alphax * .data$xmax,
        y = (1 - .data$alphay) * .data$ymin + .data$alphay * .data$ymax
      ) %>%
      filter(.data$label_value != "")

    g <- g + geom_text(
      aes(
        x = .data$x,
        y = .data$y,
        label = .data$label_value,
        colour = .data$colour,
        hjust = .data$hjust,
        vjust = .data$vjust,
        size = .data$size,
        fontface = .data$fontface,
        angle = .data$angle
      ),
      data = text_data
    )
  }

  # todo: need a generic solution
  # # PLOT TRAJ TYPES
  # if (nrow(trajd) > 0) {
  #   g <-
  #     plot_trajectory_types(
  #       plot = g,
  #       trajectory_types = trajd$topinf,
  #       xmins = trajd$xmin,
  #       xmaxs = trajd$xmax,
  #       ymins = trajd$ymin,
  #       ymaxs = trajd$ymax,
  #       node_colours = trajd$colour,
  #       edge_colours = trajd$colour,
  #       size = 1,
  #       geom = "circle",
  #       circ_size = .1
  #     )
  # }


  # ADD SIZE
  # reserve a bit more room for text that wants to go outside the frame
  expand_li <- as.list(expand)
  minimum_x <- minimum_x - (expand_li$xmin %||% 0)
  maximum_x <- maximum_x + (expand_li$xmax %||% 0)
  minimum_y <- minimum_y - (expand_li$ymin %||% 0)
  maximum_y <- maximum_y + (expand_li$ymax %||% 0)
  g <- g + expand_limits(
    x = c(minimum_x, maximum_x),
    y = c(minimum_y, maximum_y)
  )
  
  # store dimensions
  g$minimum_x <- minimum_x
  g$maximum_x <- maximum_x
  g$minimum_y <- minimum_y
  g$maximum_y <- maximum_y
  g$width <- (maximum_x - minimum_x) / 4
  g$height <- (maximum_y - minimum_y) / 4

  # return plot
  g
}

