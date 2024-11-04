compose_ggplot <- function(
    geom_positions,
    position_args) {
  # start ggplot
  g <-
    ggplot() +
    coord_equal(expand = FALSE) +
    scale_alpha_identity() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_size_identity() +
    scale_linewidth_identity() +
    scale_linetype_identity() +
    cowplot::theme_nothing()

  # PLOT ROW BACKGROUNDS
  row_pos <- (geom_positions$row_pos %||% tibble(colour_background = logical(0))) %>%
    filter(.data$colour_background)
  if (nrow(row_pos) > 0) {
    g <- g + geom_rect(
      aes(
        xmin = min(geom_positions$column_pos$xmin) - .25,
        xmax = max(geom_positions$column_pos$xmax) + .25,
        ymin = .data$ymin - (geom_positions$viz_params$row_space / 2),
        ymax = .data$ymax + (geom_positions$viz_params$row_space / 2)
      ),
      row_pos,
      fill = "#DDDDDD"
    )
  }

  # PLOT SEGMENTS
  if (nrow(geom_positions$segment_data %||% tibble()) > 0) {
    # add defaults for optional values
    geom_positions$segment_data <- geom_positions$segment_data %>% add_column_if_missing(
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
        linewidth = .data$size,
        colour = .data$colour,
        linetype = .data$linetype
      ),
      geom_positions$segment_data
    )
  }

  # PLOT RECTANGLES
  if (nrow(geom_positions$rect_data %||% tibble()) > 0) {
    # add defaults for optional values
    geom_positions$rect_data <- geom_positions$rect_data %>%
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
      geom_positions$rect_data,
      linewidth = .25
    )
  }

  # PLOT BARS
  if (nrow(geom_positions$bar_data %||% tibble()) > 0) {
    # add defaults for optional values
    geom_positions$bar_data <- geom_positions$bar_data %>%
      add_column_if_missing(
        alpha = 1,
        border = TRUE,
        border_colour = "black"
      )

    bar_data <- tibble(
      xmin = geom_positions$bar_data$xmin,
      xmax = geom_positions$bar_data$xmax,
      ymin = geom_positions$bar_data$ymin,
      ymax = geom_positions$bar_data$ymax,
      alpha = geom_positions$bar_data$alpha,
      border_colour = geom_positions$bar_data$border_colour,
    )

    fake_data <- data.frame(x=seq(bar_data$xmin,bar_data$xmax, length.out = 500), val = seq(0, 1, length.out = 500))
    colours <- geom_positions$bar_data$colour[[1]]
    # g <- g + geom_ribbon(
    #   aes(
    #     x = x,
    #     y = val,
    #     ymin = bar_data$ymin,
    #     ymax = bar_data$ymax,
    #     colour = bar_data$border_colour,
    #     alpha = bar_data$alpha
    #   ),
    #   data = fake_data,
    #   linewidth = .25
    #   ) + geom_col(aes(x = x, y = bar_data$ymax, fill = val), data = fake_data) +
    #   scale_fill_gradient(low = colours[1], high = colours[length(colours)])

    g <- g + geom_col(aes(x = x, y = bar_data$ymax, fill = val), data = fake_data) +
        scale_fill_gradient2(low = colours[1], mid = colours[6], high = colours[length(colours)]) +
        geom_rect(aes(xmin = bar_data$xmin, xmax = bar_data$xmax, ymin = bar_data$ymin, ymax = bar_data$ymax, alpha = 0, colour = bar_data$border_colour))


  }

  # PLOT CIRCLES
  if (nrow(geom_positions$circle_data %||% tibble()) > 0) {
    g <- g + ggforce::geom_circle(
      aes(
        x0 = .data$x0,
        y0 = .data$y0,
        fill = .data$colour,
        r = .data$r
      ),
      geom_positions$circle_data,
      linewidth = .25
    )
  }

  # PLOT FUNKY RECTANGLES
  if (nrow(geom_positions$funkyrect_data %||% tibble()) > 0) {
    g <- g + geom_rounded_rect(
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        radius = .data$corner_size,
        fill = .data$colour
      ),
      geom_positions$funkyrect_data,
      linewidth = .25,
      colour = "black"
    )
  }

  # PLOT PIES
  if (nrow(geom_positions$pie_data %||% tibble()) > 0) {
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
      data = geom_positions$pie_data,
      linewidth = .25
    )
  }
  # PLOT IMAGES
  if (nrow(geom_positions$img_data %||% tibble()) > 0) {
    if (!requireNamespace("magick", quietly = TRUE)) {
      cli_alert_warning("Package `magick` is required to draw images. Skipping columns with geom == \"image\".")
    } else {
      for (r in seq_len(nrow(geom_positions$img_data))) {
        image <- geom_positions$img_data[[r, "path"]]
        if (!inherits(image, "magick-image")) {
          if (is.character(image)) {
            assert_that(file.exists(image), msg = paste0("Image '", image, "' does not exist."))
          }
          image <- magick::image_read(image)
        }
        g <- g + cowplot::draw_image(
          image = image,
          x = geom_positions$img_data[[r, "xmin"]],
          y = geom_positions$img_data[[r, "ymin"]],
          width = geom_positions$img_data[[r, "width"]],
          height = geom_positions$img_data[[r, "height"]]
        )
      }
    }
  }

  # PLOT TEXT
  if (nrow(geom_positions$text_data %||% tibble()) > 0) {
    # add defaults for optional values
    geom_positions$text_data <- geom_positions$text_data %>%
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
      data = geom_positions$text_data
    )
  }

  # Compute bounds
  if (is.null(geom_positions$bounds)) {
    # determine size of current geoms
    geom_positions$bounds <- compute_bounds(
      row_pos = geom_positions$row_pos,
      column_pos = geom_positions$column_pos,
      segment_data = geom_positions$segment_data,
      rect_data = geom_positions$rect_data,
      circle_data = geom_positions$circle_data,
      funkyrect_data = geom_positions$funkyrect_data,
      pie_data = geom_positions$pie_data,
      text_data = geom_positions$text_data
    )
  }

  # ADD SIZE
  # reserve a bit more room for text that wants to go outside the frame
  minimum_x <- geom_positions$bounds$minimum_x - (position_args$expand_xmin %||% 0)
  maximum_x <- geom_positions$bounds$maximum_x + (position_args$expand_xmax %||% 0)
  minimum_y <- geom_positions$bounds$minimum_y - (position_args$expand_ymin %||% 0)
  maximum_y <- geom_positions$bounds$maximum_y + (position_args$expand_ymax %||% 0)
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
