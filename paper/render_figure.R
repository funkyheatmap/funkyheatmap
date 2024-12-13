library(funkyheatmap)
library(tidyverse)

legends <- list(
  list(title = "Bar", palette = "qc", enabled = FALSE, geom = "bar"),
  list(title = "Bar", palette = "benchmark", enabled = FALSE, geom = "bar"),
  list(title = "Bar", palette = "scaling", enabled = FALSE, geom = "bar"),
  list(title = "Bar", palette = "stability", enabled = FALSE, geom = "bar"),
  list(title = "Score", palette = "overall", enabled = TRUE, geom = "funkyrect"),
  list(title = "Error reason", palette = "error_reasons", enabled = TRUE, geom = "pie", label_width = 8)
)

g <- funky_heatmap(
  data = dynbenchmark_data$data,
  column_info = dynbenchmark_data$column_info,
  column_groups = dynbenchmark_data$column_groups,
  row_info = dynbenchmark_data$row_info,
  row_groups = dynbenchmark_data$row_groups,
  palettes = dynbenchmark_data$palettes,
  legends = legends,
  position_args = position_arguments(
    col_annot_offset = 3.2
  )
)
ggsave("paper/figure1.svg", g, width = g$width, height = g$height)

df <- tibble::tribble(
  ~"Data type", ~Example, ~"Recommended geom",
  "Numerical data", "Scores from 0 to 1", "funkyrect",
  "Aggregated data", "The mean of scores", "bar",
  "Measurement data", "3MB or 4h", "rect + text overlay",
  "Categorical data", "R or Python", "text or image",
  "Proportional data", "80% success, 10% OOM, 10% failed", "pie"
)
knitr::kable(df)
