#' Create a funkyrect legend
#' 
#' @param palette_name The name of the palette
#' @param palette The palette
#' @param position_args Sets parameters that affect positioning within a
#' plot, such as row and column dimensions, annotation details, and the
#' expansion directions of the plot. See `position_arguments()` for more information.
#' 
#' @examples
#' palette_name <- "Greys"
#' palette <- c("#FF0000", "#00FF00", "#0000FF")
#' create_funkyrect_legend(palette_name, palette)
create_funkyrect_legend <- function(palette_name, palette, position_args = position_arguments()) {
  start_x <- 0
  start_y <- 0
  
  col_width <- position_args$col_width
  row_height <- position_args$row_height

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
    pmap_df(score_to_funky_rectangle)

  fr_legend_dat2 <-
    fr_poly_data1 %>%
    # filter(!is.na(.data$r)) %>%
    group_by(.data$value) %>%
    summarise(
      minx = min(.data$xmin),
      maxx = max(.data$xmax),
      miny = min(.data$ymin),
      maxy = max(.data$ymax)
    ) %>%
    mutate(
      width = .data$maxx - .data$minx,
      height = .data$maxy - .data$miny,
      xmin = cumsum(.data$width + fr_legend_space) - .data$width - fr_legend_space,
      xmin = start_x + .data$xmin - min(.data$xmin),
      xmax = .data$xmin + .data$width,
      ymin = start_y - 2.5,
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
  funkyrect_data <-
    transmute(
      fr_legend_dat2,
      xmin = .data$x - fr_legend_size / 2,
      xmax = .data$x + fr_legend_size / 2,
      ymin = .data$y - fr_legend_size / 2,
      ymax = .data$y + fr_legend_size / 2,
      .data$value
    ) %>%
    pmap_df(score_to_funky_rectangle) %>%
    mutate(
      col_value = round(.data$value * (length(grey_palette) - 1)) + 1,
      colour = ifelse(
        is.na(.data$col_value),
        "#444444FF",
        grey_palette[.data$col_value]
      )
    )

  text_data <- bind_rows(
    tibble(
      xmin = start_x,
      xmax = fr_maximum_x,
      ymin = start_y - 1.5,
      ymax = start_y - .5,
      label_value = palette_name,
      hjust = 0,
      vjust = 1,
      fontface = "bold"
    ),
    fr_legend_dat2 %>%
      filter(abs((.data$value * 10) %% 2) < 1e-10) %>%
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
  ) %>%
    mutate(
      x = (1 - .data$hjust) * .data$xmin + .data$hjust * .data$xmax,
      y = (1 - .data$vjust) * .data$ymin + .data$vjust * .data$ymax
    )

  geom_positions <- lst(
    text_data,
    funkyrect_data
  )

  compose_ggplot(geom_positions, list())
}


#' Create a pie legend
#' @param palette_name The name of the palette
#' @param palette The palette
#' @param position_args Sets parameters that affect positioning within a
#' plot, such as row and column dimensions, annotation details, and the
#' expansion directions of the plot. See `position_arguments()` for more information.
#' @examples
#' palette_name <- "Greys"
#' palette <- c(One = "#FF0000", Two = "#00FF00", Three = "#0000FF")
#' create_pie_legend(palette_name, palette)
create_pie_legend <- function(palette_name, palette, position_args = position_arguments()) {
  start_x <- 0
  start_y <- 0
  row_height <- position_args$row_height

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

  text_data <- bind_rows(
    tibble(
      x = start_x,
      y = start_y - 1,
      label_value = palette_name,
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      colour = "black"
    ),
    pie_legend_df %>%
      transmute(
        x = start_x + .5 + .data$lab_x,
        y = start_y - 2.75 + .data$lab_y,
        label_value = as.character(.data$name),
        .data$vjust,
        .data$hjust,
        .data$colour
      )
  ) %>%
    mutate(
      # todo: need to find a better width
      xwidth = nchar(.data$label_value),
      yheight = row_height,
      xmin = .data$x - .data$xwidth * .data$hjust,
      xmax = .data$x + .data$xwidth * (1 - .data$hjust),
      ymin = .data$y - .data$yheight * .data$vjust,
      ymax = .data$y + .data$yheight * (1 - .data$vjust)
    )

  pie_data <-
    pie_legend_df %>%
      transmute(
        x0 = start_x,
        y0 = start_y - 2.75,
        r0 = 0,
        r = row_height * .75,
        .data$rad_start,
        .data$rad_end,
        colour = .data$fill
      )

  segment_data <-
    pie_legend_df %>%
    transmute(
      x = start_x + .data$xpt * .85,
      xend = start_x + .data$xpt * 1.1,
      y = start_y - 2.75 + .data$ypt * .85,
      yend = start_y - 2.75 + .data$ypt * 1.1
    )

  geom_positions <- lst(
    segment_data,
    pie_data,
    # rect_data = text_data %>% mutate(colour = NA),
    text_data
  )
  
  compose_ggplot(geom_positions, list())
}
