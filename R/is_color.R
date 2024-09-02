# Helper function to check if a character represents a color
#' @importFrom grDevices colors
is_color <- function(x) {
  x %in% colors() | grepl("^#[0-9A-Fa-f]{6}$", x)
}
