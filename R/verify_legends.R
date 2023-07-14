#' Verify the integrity of the legends object
#'
#' @inheritParams funky_heatmap
#'
#' @returns The legends object in the expected format.
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
#' legends <- list()
#' verify_legends(legends, palettes, column_info, data)
verify_legends <- function(legends, palettes, column_info, data) {
  # todo: should also check for palettes in column_groups
  if (is.null(legends)) {
    cli_alert_info("No legends were provided, trying to automatically infer legends.")
    legends <- list()
  }

  # deframe legends if it is a data frame
  if (is.data.frame(legends)) {
    legends <- deframe(legends)
  }

  # check legends
  assert_that(
    is.list(legends)
  )


  # check for missing legends, add them if necessary
  palettes_in_col_info <- na.omit(unique(column_info$palette))
  palettes_in_palette_names <- names(palettes)
  used_palettes <- intersect(palettes_in_col_info, palettes_in_palette_names)
  
  palettes_in_legends <- sapply(legends, function(legend) {
    if (is.list(legend)) {
      legend$palette
    } else {
      NULL
    }
  }) %>% unlist %>% unique
  
  missing_palettes <- setdiff(used_palettes, palettes_in_legends)

  if (length(missing_palettes) > 0) {
    cli_alert_info("Some palettes were not used in the column info, adding legends for them.")
    for (i in seq_along(missing_palettes)) {
      palette <- missing_palettes[[i]]
      legends[[length(legends) + 1]] <- list(
        title = palette,
        palette = palette,
        enabled = TRUE
      )
    }
  }

  lapply(seq_along(legends), function(i) {
    legend <- legends[[i]]

    # check data structure
    assert_that(
      is.list(legend),
      msg = paste0("Legend '", i, "' is not a list.")
    )

    # check palette
    if (legend %has_name% "palette") {
      assert_that(
        is.character(legend$palette) || is.factor(legend$palette),
        legend$palette %in% names(palettes),
        msg = paste0("Legend '", i, "' has invalid palette '", legend$palette, "'.")
      )
    }

    if (legend %has_name% "enabled") {
      assert_that(
        is.logical(legend$enabled),
        msg = paste0("Legend ", i, " has invalid enabled value '", legend$enabled, "'.")
      )
    }
    if (!legend$enabled) {
      return(legend)
    }

    # check title
    if (!legend %has_name% "title" && legend %has_name% "palette") {
      cli_alert_info(paste0("Legend ", i, " did not contain a title, using the name of the palette as title."))
      legend$title <- legend$palette
    }
    assert_that(
      legend %has_name% "title",
      is.character(legend$title) || is.factor(legend$title),
      msg = paste0("Legend ", i, " has invalid title.")
    )

    # check geom
    if (!legend %has_name% "geom" && legend %has_name% "palette") {
      cli_alert_info(paste0("Legend ", i, " did not contain a geom, inferring from the column info."))
      legend$geom <- column_info$geom[!is.na(column_info$palette) & column_info$palette == legend$palette][[1]]
    }
    assert_that(
      legend %has_name% "geom",
      legend$geom %in% c("circle", "rect", "funkyrect", "text", "pie", "continuous", "discrete", "bar"),
      msg = paste0("Legend '", i, "' has invalid geom '", legend$geom, "'.")
    )

    if (legend$geom == "bar") {
      cli_alert_warning(paste0("Legend ", i, " has geom 'bar', which is not yet implemented. Disabling for now."))
      legend$enabled <- FALSE
      return(legend)
    }
    
    # check labels
    if (!legend %has_name% "labels") {
      cli_alert_info(paste0("Legend ", i, " did not contain labels, inferring from the geom."))
      if (legend$geom == "pie" && legend %has_name% "palette") {
        legend$labels <- names(palettes[[legend$palette]])
      # } else if (legend$geom == "continuous") {
      #   legend$labels <- c("min", "max")
      # } else if (legend$geom == "discrete") {
      #   legend$labels <- c("min", "max")
      } else if (legend$geom %in% c("circle", "funkyrect", "rect")) {
        legend$labels <- c("0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1")
      } else if (legend$geom == "text") {
        cli_alert_warning(paste0("Legend ", i, " has geom 'text' but no specified labels, so disabling this legend for now."))
        legend$enabled <- FALSE
        return(legend)
      }
    }
    assert_that(
      is.character(legend$labels) || is.factor(legend$labels),
      msg = paste0("Legend '", i, "' has invalid labels.")
    )

    if (legend$geom %in% c("circle", "funkyrect", "rect")) {
      if (!legend %has_name% "size") {
        cli_alert_info(paste0("Legend ", i, " did not contain size, inferring from the labels."))
        legend$size <- seq(0, 1, length.out = length(legend$labels))
      }
      assert_that(
        is.numeric(legend$size),
        length(legend$size) == 1L || length(legend$size) == length(legend$labels),
        msg = paste0("Legend '", i, "' has invalid size value '", legend$size, "'.")
      )
    }
    if (length(legend$size) == 1L) {
      legend$size <- rep(legend$size, length(legend$labels))
    }
    
    if (legend %has_name% "colour") {
      legend$color <- legend$colour
      legend$colour <- NULL
    }
    if (!legend %has_name% "color" && legend %has_name% "palette") {
      cli_alert_info(paste0("Legend ", i, " did not contain color, inferring from the palette."))
      colors <- unname(palettes[[legend$palette]])
      legend$color <- colors[round(seq(1, length(colors), length.out = length(legend$labels)))]
    }
    assert_that(
      is.character(legend$color),
      length(legend$color) == 1L || length(legend$color) == length(legend$labels),
      msg = paste0("Legend ", i, " has invalid color value.")
    )
    if (length(legend$color) == 1L) {
      legend$color <- rep(legend$color, length(legend$labels))
    }

    legend
  })
}
