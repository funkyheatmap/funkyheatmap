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
create_generic_geom_legend <- function(
    title,
    geom = c("circle", "rect", "funkyrect"),
    labels,
    size,
    color,
    position_args = position_arguments(),
    label_hjust = .5) {
  geom <- match.arg(geom)

  start_x <- 0
  start_y <- 0

  legend_size <- 1
  legend_space <- .2

  # compute sizes of geoms
  legend_data <-
    tibble(
      size_value = size,
      color_value = color,
      xmin = -size * legend_size / 2,
      xmax = size * legend_size / 2,
      ymin = -size * legend_size / 2,
      ymax = size * legend_size / 2,
      label = labels,
      colour = color,
      size = size,
      label_hjust = label_hjust
    )

  if (geom == "funkyrect") {
    legend_data <- legend_data %>%
      pmap_df(score_to_funky_rectangle)
  } else if (geom == "circle") {
    legend_data <- legend_data %>%
      mutate(r = size / 2)
  }

  # compute positions of geoms
  geom_data <- legend_data %>%
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
      transmute(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin - 1,
        ymax = .data$ymin,
        hjust = .data$label_hjust,
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
create_funkyrect_legend <- function(
    title,
    labels,
    size,
    color,
    position_args = position_arguments(),
    label_hjust = .5) {
  create_generic_geom_legend(
    title = title,
    geom = "funkyrect",
    labels = labels,
    size = size,
    color = color,
    position_args = position_args,
    label_hjust = label_hjust
  )
}

#' Create a rect legend
#'
#' @inheritParams create_generic_geom_legend
#'
#' @noRd
create_rect_legend <- function(
    title,
    labels,
    size,
    color,
    position_args = position_arguments(),
    label_hjust = .5) {
  create_generic_geom_legend(
    title = title,
    geom = "rect",
    labels = labels,
    size = size,
    color = color,
    position_args = position_args,
    label_hjust = label_hjust
  )
}

#' Create a circle legend
#'
#' @inheritParams create_generic_geom_legend
#'
#' @noRd
create_circle_legend <- function(
    title,
    labels,
    size,
    color,
    position_args = position_arguments(),
    label_hjust = .5) {
  create_generic_geom_legend(
    title = title,
    geom = "circle",
    labels = labels,
    size = size,
    color = color,
    position_args = position_args,
    label_hjust = label_hjust
  )
}

#' Create a pie legend
#' @inheritParams create_generic_geom_legend
#'
#' @noRd
create_pie_legend <- function(
    title,
    labels,
    size, # not used
    color,
    position_args = position_arguments(),
    # TODO: if we could determine the width of the labels, this would not be needed
    label_width = 2) {
  start_x <- 0
  start_y <- 0
  row_height <- position_args$row_height

  legend_data <-
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
    legend_data %>%
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

  pie_data <- legend_data %>%
    transmute(
      x0 = start_x,
      y0 = start_y - 2.75,
      r0 = 0,
      r = row_height * .75,
      .data$rad_start,
      .data$rad_end,
      colour = .data$fill
    )

  segment_data <- legend_data %>%
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


create_bar_legend <- function(
  title,
  labels,
  size,
  color,
  position_args = position_arguments(),
  label_hjust = .5) {

  legend_width <- 5
  legend_height <- 1

  # title data
  start_x <- 0
  start_y <- 0
  title_df <-
    tibble(
      xmin = start_x,
      xmax = start_x + legend_width,
      ymin = start_y - 1.5,
      ymax = start_y - .5,
      label_value = title,
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      colour = "black"
    )

  # label data

  # bar data
  bar_data <-
    tibble(
      colour = list(color),
      xmin = start_x,
      xmax = start_x + legend_width,
      ymin = start_y - 2,
      ymax = start_y - 2 - legend_height,
      i = 0
    )

  # should generate a bunch of small rectangles with different colors
  n_col <- 500
  rect_data <- 
    tibble(
      xmin = start_x + seq(0, legend_width, length.out = n_col),
      xmax = start_x + seq(0, legend_width, length.out = n_col) + legend_width / n_col,
      ymin = start_y - 2,
      ymax = start_y - 2 - legend_height,
      i = seq_len(n_col),
      colour = list(color)
    )

  geom_positions <- lst(
    "text_data" = title_df,
    "bar_data" = rbind(bar_data, rect_data)
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
    labels,
    size,
    color,
    values,
    position_args = position_arguments(),
    # TODO: if we could determine the width of the labels, this would not be needed
    label_width = 1,
    value_width = 2) {
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
      lab_y = -row_height * (seq_along(labels) - 1)
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



#' Create an image legend
#' @inheritParams create_generic_geom_legend
create_image_legend <- function(
  title,
  labels,
  size,
  color,
  values,
  position_args = position_arguments(),
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
      lab_y = -row_height * (seq_along(labels) - 1)
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
      xwidth = 2 * .5 + value_width + label_width,
      yheight = row_height,
      xmin = .data$x - .data$xwidth * .data$hjust,
      xmax = .data$x + .data$xwidth * (1 - .data$hjust),
      ymin = .data$y - .data$yheight * .data$vjust,
      ymax = .data$y + .data$yheight * (1 - .data$vjust)
    )

  size <- min(2 * .5 + label_width, row_height)
  image_data <- data_df %>%
    transmute(
      path = .data$name,
      vjust = 0.5,
      hjust = 1,
      lab_y = -row_height * (seq_along(labels) - 1),
      x = start_x + 2 * .5 + label_width,
      y = start_y - 2 + .data$lab_y,
      width = 2 * .5 + label_width,
      height = row_height,
      xmin = x - width * hjust,
      ymin = y - height * vjust,

    )

  geom_positions <- lst(
    "img_data" = image_data,
    "text_data" = text_data
  )

  compose_ggplot(geom_positions, list())

}
