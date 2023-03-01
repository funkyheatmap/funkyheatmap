score_to_funky_rectangle <- function(xmin, xmax, ymin, ymax, value) {
  midpoint <- .8
  
  if (is.na(value)) {
    return(NULL)
  }
  
  if (value >= midpoint) {
    # transform value to a 0.5 .. 1.0 range
    trans <- (value - midpoint) / (1 - midpoint) / 2 + .5
    corner_size <- (.9 - .8 * trans) * min(xmax - xmin, ymax - ymin)
  } else {
    x <- xmin / 2 + xmax / 2
    y <- ymin / 2 + ymax / 2
    corner_size <- .5
    
    # transform value to a 0.0 .. 0.5 range
    trans <- value / midpoint
    
    # scale xmin, xmax, ymin and ymax
    width <- (trans * .9 + .1) * min(xmax - xmin, ymax - ymin)
    xmin <- x - width / 2
    xmax <- x + width / 2
    ymin <- y - width / 2
    ymax <- y + width / 2
  }
  
  tibble(xmin, xmax, ymin, ymax, corner_size, value)
}