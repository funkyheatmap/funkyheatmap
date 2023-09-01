#' Verify the integrity of the column info object
#'
#' @inheritParams funky_heatmap
#'
#' @returns The column info object with all expected columns.
#'
#' @export
#'
#' @examples
#' library(tibble)
#' data <- tribble(
#'   ~id, ~name, ~x, ~y,
#'   "foo", "Foo", 0.5, 0.7,
#'   "bar", "Bar", 1.0, 0.1
#' )
#' column_info <- tribble(
#'   ~id, ~geom,
#'   "name", "text",
#'   "x", "funkyrect",
#'   "y", "funkyrect"
#' )
#' verify_column_info(column_info, data)
verify_column_info <- function(column_info, data) {
  if (is.null(column_info)) {
    cli_alert_info("No column info was provided, assuming all columns in `data` are to be plotted.")
    column_info <- tibble(id = colnames(data))
  }
  assert_that(
    is.data.frame(column_info),
    column_info %has_name% "id",
    is.character(column_info$id) | is.factor(column_info$id),
    all(column_info$id %in% colnames(data))
  )

  # checking options
  if (column_info %has_name% "options") {
    if (is.character(column_info$options)) {
      cli_alert_info("Detected json in the options column, parsing.")
      column_info$options <- map(column_info$options, jsonlite::fromJSON)
    }
    column_info <- column_info %>%
      mutate(options = map(options, function(x) {
        optdf <- if (is.null(x) || length(x) == 0) tibble(a = 1)[, -1] else as_tibble(x)
        assert_that(nrow(optdf) == 1, msg = paste0("Trying to convert: ", as.character(x)))
        optdf
      })) %>%
      unnest(cols = "options")
  }

  # checking name
  if (!column_info %has_name% "name") {
    cli_alert_info("Column info did not contain column `name`, using `id` to generate it.")
    column_info$name <- stringr::str_to_title(gsub("_", " ", column_info$id))
  }
  assert_that(
    is.character(column_info$name) | is.factor(column_info$name)
  )

  # checking geom
  if (!column_info %has_name% "geom") {
    cli_alert_info("Column info did not contain information on which columns to plot, inferring from `data` types.")
    column_info$geom <- map_chr(data, function(x) {
      case_when(
        is.numeric(x) ~ "funkyrect",
        is.character(x) | is.factor(x) | is.logical(x) ~ "text",
        is.list(x) && map_lgl(x, function(z) is.numeric(z) && !is.null(names(z))) ~ "pie",
        TRUE ~ NA_character_
      )
    })
  }
  check_geom <-
    (is.character(column_info$geom) | is.factor(column_info$geom)) &
    column_info$geom %in% c("funkyrect", "circle", "rect", "bar", "pie", "text", "image")
  assert_that(
    all(check_geom),
    msg = paste0("Invalid geom types for columns: '", paste0(column_info$id[!check_geom], collapse = "', '"), "'")
  )

  # checking id_color
  if (column_info %has_name% "id_colour") {
    # rename id_colour to id_color
    column_info$id_color <- column_info$id_colour
    column_info$id_colour <- NULL
  }
  if (!column_info %has_name% "id_color") {
    column_info$id_color <- NA_character_
  }
  assert_that(
    is.character(column_info$id_color),
    all(is.na(column_info$id_color) | is_color(column_info$id_color) | column_info$id_color %in% colnames(data))
  )
  column_info$id_color <- case_when(
    !is.na(column_info$id_color) ~ column_info$id_color,
    column_info$geom == "text" ~ NA_character_,
    column_info$geom == "image" ~ NA_character_,
    TRUE ~ column_info$id
  )

  # checking id_size
  if (!column_info %has_name% "id_size") {
    column_info$id_size <- NA_character_
  }
  assert_that(
    is.character(column_info$id_size),
    all(is.na(column_info$id_size) | column_info$id_size %in% colnames(data))
  )
  column_info$id_size <- case_when(
    !is.na(column_info$id_size) ~ column_info$id_size,
    column_info$geom == "text" ~ NA_character_,
    column_info$geom == "image" ~ NA_character_,
    column_info$geom == "rect" ~ NA_character_, # replicate legacy behaviour?
    TRUE ~ column_info$id
  )

  # checking group
  if (!column_info %has_name% "group" || all(is.na(column_info$group))) {
    cli_alert_info("Column info did not contain group information, assuming columns are ungrouped.")
    column_info$group <- NA_character_
  }
  assert_that(
    is.character(column_info$group) | is.factor(column_info$group),
    msg = "Column info 'group' must be character or factor."
  )
  column_info$group[column_info$group == ""] <- NA_character_

  # checking palette
  if (!column_info %has_name% "palette" || all(is.na(column_info$palette))) {
    cli_alert_info("Column info did not contain a column called 'palette', generating palettes based on the 'geom' column.")
    column_info$palette <- case_when(
      column_info$geom == "text" ~ NA_character_,
      column_info$geom == "image" ~ NA_character_,
      column_info$geom == "pie" ~ "categorical_palette",
      TRUE ~ "numerical_palette"
    )
  }
  assert_that(
    is.character(column_info$palette) | is.factor(column_info$palette),
    msg = "Column info 'palette' must be character or factor."
  )

  # checking width
  if (!column_info %has_name% "width") {
    cli_alert_info("Column info did not contain a column called 'width', generating options based on the 'geom' column.")
    column_info <- column_info %>% mutate(
      width = case_when(
        .data$geom == "text" ~ 6,
        .data$geom == "bar" ~ 4,
        TRUE ~ 1
      )
    )
  }
  column_info$width[is.na(column_info$width)] <- 1
  assert_that(
    is.numeric(column_info$width),
    msg = "Column info 'width' must be numeric."
  )

  # checking overlay
  if (!column_info %has_name% "overlay") {
    column_info$overlay <- FALSE
  }
  column_info$overlay[is.na(column_info$overlay)] <- FALSE
  assert_that(
    is.logical(column_info$overlay),
    all(!is.na(column_info$overlay)),
    msg = "Column info 'overlay' must be logical."
  )

  # checking legend
  if (!column_info %has_name% "legend") {
    cli_alert_info("Column info did not contain a column called 'legend', generating options based on the 'geom' column.")
    columns_info$legend <- NA
  }
  column_info <- column_info %>% mutate(
    legend = ifelse(is.na(.data$legend), .data$geom != "text", .data$legend)
  )
  assert_that(
    is.logical(column_info$legend),
    all(!is.na(column_info$legend)),
    msg = "Column info 'legend' must be logical."
  )

  # checking draw_outline
  if (!column_info %has_name% "draw_outline") {
    column_info$draw_outline <- TRUE
  }
  column_info$draw_outline[is.na(column_info$draw_outline)] <- TRUE
  assert_that(
    is.logical(column_info$draw_outline),
    all(!is.na(column_info$draw_outline)),
    msg = "Column info 'draw_outline' must be logical."
  )

  column_info
}