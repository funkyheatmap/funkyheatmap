#' Verify the integrity of the column groups object
#'
#' @inheritParams funky_heatmap
#'
#' @returns The column groups object with all expected columns.
#' 
#' @export
#'
#' @examples
#' library(tibble)
#' column_groups <- tribble(
#'   ~group, ~level1,
#'   "foo", "Foo",
#'   "bar", "Bar"
#' )
#' column_info <- tribble(
#'   ~id, ~geom, ~group,
#'   "name", "text", NA_character_,
#'   "foo1", "funkyrect", "foo",
#'   "foo2", "funkyrect", "foo",
#'   "bar1", "funkyrect", "bar",
#'   "bar2", "funkyrect", "bar"
#' )
#' verify_column_groups(column_groups, column_info)
verify_column_groups <- function(column_groups, column_info) {
  if (is.null(column_groups) && !all(is.na(column_info$group))) {
    cli_alert_info("No column groups was provided, deriving from column info.")
    column_groups <- column_info %>%
      select("group") %>%
      filter(!is.na(.data$group)) %>%
      distinct()
  }

  # if column_groups is still NULL, simply return
  if (is.null(column_groups)) {
    return(NULL)
  }

  assert_that(
    is.data.frame(column_groups),
    column_groups %has_name% "group",
    is.character(column_groups$group) | is.factor(column_groups$group),
    all(is.na(column_info$group) | column_info$group %in% column_groups$group)
  )
  if (!all(column_groups$group %in% column_info$group)) {
    unused <- unique(column_groups$group[!column_groups$group %in% column_info$group])
    cli_alert_warning("Detected unused column groups: {paste(unused, collapse = ', ')}.")
  }

  # checking palette
  if (!column_groups %has_name% "palette") {
    cli_alert_info("Column groups did not contain a column called 'palette'. Assuming no colour scales need to be used.")
    column_groups$palette <- NA_character_
  }
  assert_that(
    is.character(column_groups$palette) | is.factor(column_groups$palette)
  )

  # checking other columns
  column_groups_colnames <- setdiff(colnames(column_groups), c("group", "palette"))
  
  if (length(column_groups_colnames) == 0) {
    cli_alert_info("Column groups did not contain a column called 'level1'. Using `column_info$group` as a makeshift column group name.")
    column_groups$level1 <- stringr::str_to_title(column_groups$group)
  }
  for (colname in column_groups_colnames) {
    assert_that(is.character(column_groups[[colname]]) || is.character(column_groups[[colname]]))
  }

  column_groups
}