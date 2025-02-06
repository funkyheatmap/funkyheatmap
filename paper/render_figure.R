library(funkyheatmap)
library(tidyverse)
library(Cairo)

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

Cairo::CairoSVG("paper/figure1.svg", width = g$width, height = g$height)
g
dev.off()

df <- tibble::tribble(
  ~"Data type", ~Example, ~"Recommended geom",
  "Numerical data", "Scores from 0 to 1", "funkyrect",
  "Aggregated data", "The mean of scores", "bar",
  "Measurement data", "3MB or 4h", "rect + text overlay",
  "Categorical data", "R or Python", "text or image",
  "Proportional data", "80% success, 10% OOM, 10% failed", "pie"
)
knitr::kable(df)

data("mtcars")

data <- mtcars %>%
  rownames_to_column("id") %>%
  arrange(desc(mpg))

column_info <- tribble(
  ~id,     ~group,         ~name,                      ~geom,        ~palette,   
  "id",    "",             "",                         "text",       NA,         
  "mpg",   "overall",      "Miles / gallon",           "bar",        "palette1",
  "cyl",   "overall",      "Number of cylinders",      "bar",        "palette2",
  "disp",  "group1",       "Displacement (cu.in.)",    "funkyrect",  "palette1", 
  "hp",    "group1",       "Gross horsepower",         "funkyrect",  "palette1", 
  "drat",  "group1",       "Rear axle ratio",          "funkyrect",  "palette1", 
  "wt",    "group1",       "Weight (1000 lbs)",        "funkyrect",  "palette1", 
  "qsec",  "group2",       "1/4 mile time",            "circle",     "palette2", 
  "vs",    "group2",       "Engine",                   "circle",     "palette2", 
  "am",    "group2",       "Transmission",             "circle",     "palette2", 
  "gear",  "group2",       "# Forward gears",          "circle",     "palette2", 
  "carb",  "group2",       "# Carburetors",            "circle",     "palette2",
)

g2 <- funky_heatmap(
  data,
  column_info = column_info,
  position_args = position_arguments(expand_xmax = 4)
)

Cairo::CairoSVG("paper/figure2.svg", width = g2$width * 1.5, height = g2$height * 1.5)
g2
dev.off()
