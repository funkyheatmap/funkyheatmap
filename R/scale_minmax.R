scale_minmax <- function(x) {
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  (x - minx) / (maxx - minx)
}