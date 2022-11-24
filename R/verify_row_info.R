#' Verify the integrity of the row info object
#'
#' @inheritParams funky_heatmap
#'
#' @returns The row info object with all expected columns.
#' 
#' @export
#' 
#' @examples
#' library(tibble)
#' data <- tribble(
#'   ~id, ~name, ~x, ~y,
#'   "foo1", "Foo1", 0.5, 0.7,
#'   "foo2", "Foo2", 0.5, 0.8,
#'   "bar1", "Bar1", 1.0, 0.2,
#'   "bar2", "Bar2", 1.0, 0.1
#' )
#' row_info <- tribble(
#'   ~id, ~group,
#'   "foo1", "foo",
#'   "foo2", "foo",
#'   "bar1", "bar",
#'   "bar2", "bar"
#' )
#' verify_row_info(row_info, data)
verify_row_info <- function(row_info, data) {
  # assume data and palettes are already verified

  if (is.null(row_info)) {
    cli_alert_info("No row info was provided, assuming all rows in `data` are to be plotted.")
    row_info <- tibble(id = data$id)
  }
  assert_that(
    is.data.frame(row_info),
    row_info %has_name% "id",
    is.character(row_info$id) | is.factor(row_info$id),
    all(row_info$id %in% data$id)
  )

  # checking group
  if (!row_info %has_name% "group") {
    cli_alert_info("Row info did not contain group information, assuming rows are ungrouped.")
    row_info$group <- NA_character_
  }
  assert_that(
    is.character(row_info$group) | is.factor(row_info$group)
  )

  row_info
}