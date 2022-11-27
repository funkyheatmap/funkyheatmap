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

  # checking name
  if (!column_info %has_name% "name") {
    cli_alert_info("Column info did not contain column `name`, using `id` to generate it.")
    column_info$name <- stringr::str_to_title(column_info$id)
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
    all(column_info$geom %in% c("funkyrect", "circle", "rect", "bar", "pie", "text"))
  )

  # checking group
  if (!column_info %has_name% "group") {
    cli_alert_info("Column info did not contain group information, assuming columns are ungrouped.")
    column_info$group <- NA_character_
  }
  assert_that(
    is.character(column_info$group) | is.factor(column_info$group)
  )
  column_info$group[column_info$group == ""] <- NA_character_

  # checking palette
  if (!column_info %has_name% "palette") {
    cli_alert_info("Column info did not contain a column called 'palette', generating palettes based on the 'geom' column.")
    column_info$palette <- case_when(
      column_info$geom == "text" ~ NA_character_,
      column_info$geom == "pie" ~ "categorical_palette",
      TRUE ~ "numerical_palette"
    )
  }
  assert_that(
    is.character(column_info$palette) | is.factor(column_info$palette)
  )

  # checking options
  if (!column_info %has_name% "options") {
    cli_alert_info("Column info did not contain a column called 'options', generating options based on the 'geom' column.")
    # column_info$options <- map(seq_len(nrow(column_info)), function(x) list())
    column_info$options <- pmap(column_info, function(geom, ...) {
      if (geom == "text") {
        list(width = 6)
      } else {
        list()
      }
    })
  }
  assert_that(
    is.list(column_info$options),
    all(map_lgl(column_info$options, is.list))
  )

  column_info
}
