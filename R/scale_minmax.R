#' Scale a vector to the range [0, 1]
#' 
#' @param x A numeric vector
#' @return A numeric vector
#' @export
#' 
#' @examples
#' scale_minmax(c(1, 2, 3))
scale_minmax <- function(x) {
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  (x - minx) / (maxx - minx)
}
