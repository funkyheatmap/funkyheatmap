#' @importFrom grDevices colorRampPalette
smear <- function(cols) {
  grDevices::colorRampPalette(cols)(101)
}

#' @importFrom RColorBrewer brewer.pal
default_palettes <- list(
  numerical = list(
    "Blues" = RColorBrewer::brewer.pal(9, "Blues") %>% c("#011636") %>% rev %>% smear,
    "Reds" = RColorBrewer::brewer.pal(9, "Reds")[-8:-9] %>% rev %>% smear,
    "YlOrBr" = RColorBrewer::brewer.pal(9, "YlOrBr")[-7:-9] %>% rev %>% smear,
    "Greens" = RColorBrewer::brewer.pal(9, "Greens")[-1] %>% c("#00250f") %>% rev %>% smear,
    "Greys" = RColorBrewer::brewer.pal(9, "Greys")[-1] %>% rev %>% smear
  ),
  categorical = list(
    "Set3" = RColorBrewer::brewer.pal(12, "Set3"),
    "Set1" = RColorBrewer::brewer.pal(9, "Set1"),
    "Set2" = RColorBrewer::brewer.pal(8, "Set2"),
    "Dark2" = RColorBrewer::brewer.pal(8, "Dark2")
  )
)

#' Verify the integrity of the palettes object
#'
#' @inheritParams funky_heatmap
#'
#' @returns The palettes object with all expected columns.
#' 
#' @export
#'
#' @examples
#' library(tibble)
#' library(grDevices)
#' library(RColorBrewer)
#'
#' # explicit form
#' data <- tribble(
#'   ~id, ~name, ~x, ~y,
#'   "foo", "Foo", 0.5, 0.7,
#'   "bar", "Bar", 1.0, 0.1
#' )
#' column_info <- tribble(
#'   ~id, ~geom, ~palette,
#'   "name", "text", NA,
#'   "foo", "funkyrect", "pal1",
#'   "bar", "funkyrect", "pal2"
#' )
#' palettes <- list(
#'   pal1 = rev(brewer.pal(9, "Greys")[-1]),
#'   pal2 = rev(brewer.pal(9, "Reds")[-8:-9])
#' )
#' verify_palettes(palettes, column_info, data)
#'
#' # implicit palettes
#' palettes <- list(
#'   pal1 = "Greys",
#'   pal2 = "Reds"
#' )
#' verify_palettes(palettes, column_info, data)
#'
#' # passing a tibble should also work (for backwards compatibility)
#' palettes <- tribble(
#'   ~palette, ~colours,
#'   "pal1", rev(brewer.pal(9, "Greys")[-1]),
#'   "pal2", rev(brewer.pal(9, "Reds")[-8:-9])
#' )
#' verify_palettes(palettes, column_info, data)
verify_palettes <- function(palettes, column_info, data) {
  # todo: should also check for palettes in column_groups
  if (is.null(palettes)) {
    cli_alert_info("No palettes were provided, trying to automatically assign palettes.")
    palettes <- list()
  }

  # deframe palettes if it is a data frame
  if (is.data.frame(palettes)) {
    palettes <- deframe(palettes)
  }

  # check palettes
  assert_that(
    is.list(palettes)
  )

  # check missing palettes
  col_info_palettes <- na.omit(unique(column_info$palette))
  rotation_counter <- list(numerical = 1L, categorical = 1L)
  for (palette_id in col_info_palettes) {
    if (!palettes %has_name% palette_id) {
      # take one of the columns with this palette
      columns <- column_info %>% filter(.data$palette == !!palette_id) %>% slice_head()
      
      # try to determine palette type (numerical or categorical)
      palette_type <- 
        if (columns$geom == "pie") {
          "categorical"
        } else if (is.numeric(data[[columns$id]])) {
          "numerical"
        } else {
          "categorical"
        }

      # fetch palette
      counter <- rotation_counter[[palette_type]]
      palette_name <- names(default_palettes[[palette_type]])[[counter]]

      # increment counter
      counter <- counter + 1
      if (counter > length(default_palettes[[palette_type]])) {
        counter <- 1
      }
      rotation_counter[[palette_type]] <- counter
      
      cli_alert_info("Palette named '{palette_id}' was not defined. Assuming palette is {palette_type}. Automatically selected palette '{palette_name}'.")

      # assigning palette
      palettes[[palette_id]] <- palette_name
    }

    assert_that(is.character(palettes[[palette_id]]) | is.factor(palettes[[palette_id]]))

    pal_value <- palettes[[palette_id]]
    if (length(pal_value) == 1) {
      if (default_palettes$numerical %has_name% pal_value) {
        pal_value <- default_palettes$numerical[[pal_value]]
      } else if (default_palettes$categorical %has_name% pal_value) {
        pal_value <- default_palettes$categorical[[pal_value]]
      }
    }
    palettes[[palette_id]] <- pal_value
  }

  # todo: add check whether all values in the palettes are colors?

  palettes
}