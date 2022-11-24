
rounded_rectangle <- function(xmin, xmax, ymin, ymax, corner_size) {
  # rename for ease of use
  cs <- corner_size
  
  arc_bars <-
    tribble(
      ~x, ~y, ~r, ~start, ~end,
      xmax - cs, ymax - cs, cs, 0, pi / 2,
      xmax - cs, ymin + cs, cs, pi / 2, pi,
      xmin + cs, ymin + cs, cs, pi, pi * 3 / 2,
      xmin + cs, ymax - cs, cs, pi * 3 / 2, 2 * pi
    )
  
  polygons <-
    tribble(
      ~x, ~y, ~subgroup,
      xmin + cs, ymin, "bottom",
      xmax - cs, ymin, "bottom",
      xmax, ymin + cs, "right",
      xmax, ymax - cs, "right",
      xmax - cs, ymax, "top",
      xmin + cs, ymax, "top",
      xmin, ymax - cs, "left",
      xmin, ymin + cs, "left"
    )
  
  lst(arc_bars, polygons)
}