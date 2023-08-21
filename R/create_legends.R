#' Create a generic geom legend (for circles, rects, and funkyrects)
#'
#' @param title The name of the palette
#' @param palette The palette
#' @param geom Which geom to draw.
#' @param labels The labels to use for the legend.
#' @param size The sizes to use for the legend.
#' @param color The colors to use for the legend.
#' @param position_args Sets parameters that affect positioning within a
#' plot, such as row and column dimensions, annotation details, and the
#' expansion directions of the plot. See `position_arguments()` for more information.
#' @param values Used as value for the 'image' and 'text' geom.
#' 
#' @noRd
#'
#' @examples
#' title <- "Greys"
#' palette <- funkyheatmap:::default_palettes$numerical$Greys
#' geom <- "circle"
#' create_generic_geom_legend(title, palette, geom)
create_generic_geom_legend <- function(
  title,
  palette,
  geom = c("circle", "rect", "funkyrect"),
  labels,
  size,
  color,
  position_args = position_arguments()
) {
  geom <- match.arg(geom)

  start_x <- 0
  start_y <- 0

  legend_size <- 1
  legend_space <- .2

  # compute sizes of geoms
  geom_size_data <-
    tibble(
      value = size,
      xmin = - size * legend_size / 2,
      xmax = size * legend_size / 2,
      ymin = - size * legend_size / 2,
      ymax = size * legend_size / 2
    )

  if (geom == "funkyrect") {
    geom_size_data <- geom_size_data %>%
      pmap_df(score_to_funky_rectangle)
  } else if (geom == "circle") {
    geom_size_data <- geom_size_data %>%
      mutate(r = size / 2)
  }

  # add metadata
  geom_size_data <- geom_size_data %>%
    mutate(
      label = labels,
      colour = color,
      size = size
    )

  # compute positions of geoms
  geom_data <- geom_size_data %>%
    mutate(
      width = .data$xmax - .data$xmin,
      height = .data$ymax - .data$ymin,
      xmin = cumsum(.data$width + legend_space) - .data$width - legend_space,
      xmin = start_x + .data$xmin - min(.data$xmin),
      xmax = .data$xmin + .data$width,
      ymin = start_y - 2.5,
      ymax = .data$ymin + .data$height,
      x = (.data$xmin + .data$xmax) / 2,
      y = (.data$ymin + .data$ymax) / 2,
      x0 = .data$x,
      y0 = .data$y
    )

  maximum_x <- max(geom_data$xmax)

  text_data <- bind_rows(
    tibble(
      xmin = start_x,
      xmax = maximum_x,
      ymin = start_y - 1.5,
      ymax = start_y - .5,
      label_value = title,
      hjust = 0,
      vjust = 1,
      fontface = "bold"
    ),
    geom_data %>%
      filter(abs((.data$value * 10) %% 2) < 1e-10) %>%
      transmute(
        ymin = .data$ymin - 1,
        ymax = .data$ymin,
        x = (.data$xmin + .data$xmax) / 2,
        xwidth = pmax(.data$xmax - .data$xmin, .5),
        xmin = .data$x - .data$xwidth / 2,
        xmax = .data$x + .data$xwidth / 2,
        hjust = .5,
        vjust = 0,
        label_value = as.character(.data$label)
      )
  ) %>%
    mutate(
      x = (1 - .data$hjust) * .data$xmin + .data$hjust * .data$xmax,
      y = (1 - .data$vjust) * .data$ymin + .data$vjust * .data$ymax
    )

  geom_positions <- list(text_data = text_data)
  geom_positions[[paste0(geom, "_data")]] <- geom_data

  compose_ggplot(geom_positions, list())
}

#' Create a funkyrect legend
#' 
#' @inheritParams create_generic_geom_legend
#' 
#' @noRd
#' 
#' @examples
#' title <- "Greys"
#' palette <- funkyheatmap:::default_palettes$numerical$Greys
#' create_funkyrect_legend(title, palette)
create_funkyrect_legend <- function(
  title,
  palette,
  labels,
  size,
  color,
  position_args = position_arguments()
) {
  create_generic_geom_legend(title, palette, "funkyrect", labels, size, color, position_args)
}

#' Create a rect legend
#' 
#' @inheritParams create_generic_geom_legend
#' 
#' @noRd
#' 
#' @examples
#' title <- "Greys"
#' palette <- funkyheatmap:::default_palettes$numerical$Greys
#' create_rect_legend(title, palette)
create_rect_legend <- function(
  title,
  palette,
  labels,
  size,
  color,
  position_args = position_arguments()
) {
  create_generic_geom_legend(title, palette, "rect", labels, size, color, position_args)
}

#' Create a circle legend
#' 
#' @inheritParams create_generic_geom_legend
#' 
#' @noRd
#' 
#' @examples
#' title <- "Greys"
#' palette <- funkyheatmap:::default_palettes$numerical$Greys
#' create_circle_legend(title, palette)
create_circle_legend <- function(
  title,
  palette,
  labels,
  size,
  color,
  position_args = position_arguments()
) {
  create_generic_geom_legend(title, palette, "circle", labels, size, color, position_args)
}

#' Create a pie legend
#' @inheritParams create_generic_geom_legend
#' 
#' @noRd
#' 
#' @examples
#' title <- "Greys"
#' palette <- c(One = "#FF0000", Two = "#00FF00", Three = "#0000FF")
#' create_pie_legend(title, palette)
create_pie_legend <- function(
  title,
  palette, # not used
  labels,
  size, # not used
  color,
  position_args = position_arguments(),
  # TODO: if we could determine the width of the labels, this would not be needed
  label_width = 2
) {
  start_x <- 0
  start_y <- 0
  row_height <- position_args$row_height

  pie_legend_df <-
    tibble(
      name = labels,
      fill = color
    ) %>%
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
      label_value = title,
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
      xwidth = label_width,
      yheight = row_height,
      xmin = .data$x - .data$xwidth * .data$hjust,
      xmax = .data$x + .data$xwidth * (1 - .data$hjust),
      ymin = .data$y - .data$yheight * .data$vjust,
      ymax = .data$y + .data$yheight * (1 - .data$vjust)
    )

  pie_data <- pie_legend_df %>%
    transmute(
      x0 = start_x,
      y0 = start_y - 2.75,
      r0 = 0,
      r = row_height * .75,
      .data$rad_start,
      .data$rad_end,
      colour = .data$fill
    )

  segment_data <- pie_legend_df %>%
    transmute(
      x = start_x + .data$xpt * .85,
      xend = start_x + .data$xpt * 1.1,
      y = start_y - 2.75 + .data$ypt * .85,
      yend = start_y - 2.75 + .data$ypt * 1.1
    )

  geom_positions <- lst(
    segment_data,
    pie_data,
    text_data
  )

  compose_ggplot(geom_positions, list())
}






#' Create a text legend
#' @inheritParams create_generic_geom_legend
#' 
#' @noRd
#' 
#' @examples
#' title <- "Greys"
#' labels <- c("A", "B", "C")
#' values <- c("One", "Two", "Three")
#' create_text_legend(title, values = values, labels = labels)
create_text_legend <- function(
  title,
  palette,
  labels,
  size,
  color,
  values,
  position_args = position_arguments(),
  # TODO: if we could determine the width of the labels, this would not be needed
  label_width = 1,
  value_width = 2
) {
  start_x <- 0
  start_y <- 0
  row_height <- position_args$row_height

  data_df <-
    tibble(
      name = labels,
      value = values,
      colour = color,
      size = size,
      vjust = .5,
      hjust = 0,
      lab_y = - row_height * (seq_along(labels) - 1)
    )

  text_data <- bind_rows(
    tibble(
      x = start_x,
      y = start_y - 1,
      label_value = title,
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      colour = "black"
    ),
    data_df %>%
      transmute(
        x = start_x + .5,
        y = start_y - 2 + .data$lab_y,
        label_value = as.character(.data$name),
        .data$vjust,
        .data$hjust,
        .data$colour
      ),
    data_df %>%
      transmute(
        x = start_x + 2 * .5 + label_width,
        y = start_y - 2 + .data$lab_y,
        label_value = as.character(.data$value),
        .data$vjust,
        .data$hjust,
        .data$colour
      )
  ) %>%
    mutate(
      # todo: need to find a better width
      xwidth = 2 * .5 + label_width + value_width,
      yheight = row_height,
      xmin = .data$x - .data$xwidth * .data$hjust,
      xmax = .data$x + .data$xwidth * (1 - .data$hjust),
      ymin = .data$y - .data$yheight * .data$vjust,
      ymax = .data$y + .data$yheight * (1 - .data$vjust)
    )


  geom_positions <- lst(
    text_data
  )

  compose_ggplot(geom_positions, list())
}



# #' Create an image legend
# #' @inheritParams create_generic_geom_legend
# #' 
# #' @noRd
# create_image_legend <- function(
#   title,
#   palette,
#   labels,
#   size,
#   color,
#   values,
#   position_args = position_arguments()
# ) {
#   warning("Image legend not yet implemented.")
#   NULL
# }