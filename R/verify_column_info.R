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
        is.character(x) | is.factor(x) ~ "text",
        is.list(x) && map_lgl(x, function(z) is.numeric(z) && !is.null(names(z))) ~ "pie",
        TRUE ~ NA_character_
      )
    })
  }
  assert_that(
    is.character(column_info$geom) | is.factor(column_info$geom),
    all(column_info$geom %in% c("funkyrect", "circle", "rect", "bar", "pie", "text", "image"))
  )

  # checking color
  if (!column_info %has_name% "color") {
    cli_alert_info("Column info did not contain column `color`, assuming color is defined by the 'id' column.")
    column_info$color <- case_when(
      column_info$geom == "text" ~ NA_character_,
      column_info$geom == "image" ~ NA_character_,
      TRUE ~ column_info$id
    )
  }
  assert_that(
    is.character(column_info$color),
    all(is.na(column_info$color) | is_color(column_info$color) | column_info$color %in% colnames(data))
  )

  # checking group
  if (!column_info %has_name% "group" || all(is.na(column_info$group))) {
    cli_alert_info("Column info did not contain group information, assuming columns are ungrouped.")
    column_info$group <- NA_character_
  }
  assert_that(
    is.character(column_info$group) | is.factor(column_info$group)
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
    is.character(column_info$palette) | is.factor(column_info$palette)
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
  assert_that(
    is.numeric(column_info$width)
  )
  column_info$width[is.na(column_info$width)] <- 1

  # checking overlay
  if (!column_info %has_name% "overlay") {
    column_info$overlay <- FALSE
  }
  assert_that(
    is.logical(column_info$overlay)
  )
  column_info$overlay[is.na(column_info$overlay)] <- FALSE

  # checking legend
  if (!column_info %has_name% "legend") {
    cli_alert_info("Column info did not contain a column called 'legend', generating options based on the 'geom' column.")
    column_info <- column_info %>% mutate(legend = .data$geom != "text")
  }
  assert_that(
    is.logical(column_info$legend)
  )

  column_info
}

# Function to check if a string represents a color
is_color <- function(x) {
  x %in% colors() | grepl("^#[0-9A-Fa-f]{6}$", x)
}