score_to_funky_rectangle <- function(xmin, xmax, ymin, ymax, value, midpoint = .5, name = NULL) {
  if (is.na(value)) {
    return(NULL)
  }
  
  if (is.null(name)) {
    name <- dynutils::random_time_string()
  }
  
  out <-
    if (value >= midpoint) {
      # transform value to a 0.5 .. 1.0 range
      trans <- (value - midpoint) / (1 - midpoint) / 2 + .5
      corner_size <- (.9 - .8 * trans) * min(xmax - xmin, ymax - ymin)
      
      funkyrect_rectangle(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        corner_size = corner_size
      )
    } else {
      # transform value to a 0.0 .. 0.5 range
      trans <- value / midpoint / 2
      
      tibble(
        x = xmin / 2 + xmax / 2,
        y = ymin / 2 + ymax / 2,
        r = (trans * .9 + .1) * min(xmax - xmin, ymax - ymin),
        start = 0,
        end = 2 * pi
      )
    }
  
  bind_rows(out) %>%
    mutate(name, value)
}

