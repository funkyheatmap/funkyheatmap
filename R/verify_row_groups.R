#' Verify the integrity of the row groups object
#'
#' @inheritParams funky_heatmap
#'
#' @returns The row groups object with all expected rows.
#'
#' @export
#'
#' @examples
#' library(tibble)
#' row_groups <- tribble(
#'   ~group, ~level1,
#'   "foo", "Foo",
#'   "bar", "Bar"
#' )
#' row_info <- tribble(
#'   ~id, ~group,
#'   "name", NA_character_,
#'   "foo1", "foo",
#'   "foo2", "foo",
#'   "bar1", "bar",
#'   "bar2", "bar"
#' )
#' verify_row_groups(row_groups, row_info)
verify_row_groups <- function(row_groups, row_info) {
  if (is.null(row_groups) && !all(is.na(row_info$group))) {
    cli_alert_info("No row groups was provided, deriving from row info.")
    row_groups <- row_info %>%
      select("group") %>%
      filter(!is.na(.data$group)) %>%
      distinct()
  }

  # if row_groups is still NULL, simply return
  if (is.null(row_groups)) {
    return(NULL)
  }

  row_groups <- if_list_to_tibble(row_groups)

  assert_that(
    is.data.frame(row_groups),
    row_groups %has_name% "group",
    is.character(row_groups$group) | is.factor(row_groups$group),
    all(row_groups$group %in% row_info$group),
    all(is.na(row_info$group) | row_info$group %in% row_groups$group)
  )

  # checking other rows
  row_groups_colnames <- setdiff(colnames(row_groups), "group")

  if (length(row_groups_colnames) == 0) {
    cli_alert_info("Row groups did not contain a row called 'level1'. Using `row_info$group` as a makeshift row group name.")
    row_groups$level1 <- stringr::str_to_title(row_groups$group)
  }
  for (colname in row_groups_colnames) {
    assert_that(is.character(row_groups[[colname]]) || is.character(row_groups[[colname]]))
  }

  row_groups
}
