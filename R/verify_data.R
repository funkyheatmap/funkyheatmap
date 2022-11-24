#' Verify the integrity of the data object
#' 
#' @inheritParams funky_heatmap
#'  
#' @returns A verified data object
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
#' verify_data(data)
verify_data <- function(data) {
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  
  assert_that(
    is.data.frame(data),
    ncol(data) >= 1,
    nrow(data) >= 1,
    data %has_name% "id"
  )
  
  data
}