library(tidyverse)
library(funkyheatmap)

data <- mtcars %>% 
  rownames_to_column("id") %>%
  arrange(desc(mpg)) %>%
  head(20) %>%
  add_column(type = c(rep("ice", 10), rep("electric", 10)))

column_info <- tribble(
  ~id,     ~group,         ~name,                      ~geom,        ~palette,    ~options,
  "id",    NA,             "",                         "text",       NA,          list(hjust = 0, width = 6),
  "mpg",   "overall",      "Miles / gallon",           "bar",        "palette1",  list(width = 4, legend = FALSE),
  "cyl",   "overall",      "Number of cylinders",      "bar",        "palette2",  list(width = 4, legend = FALSE),
  "disp",  "group1",       "Displacement (cu.in.)",    "funkyrect",  "palette1",  lst(),
  "hp",    "group1",       "Gross horsepower",         "funkyrect",  "palette1",  lst(),
  "drat",  "group1",       "Rear axle ratio",          "funkyrect",  "palette1",  lst(),
  "wt",    "group1",       "Weight (1000 lbs)",        "funkyrect",  "palette1",  lst(),
  "qsec",  "group2",       "1/4 mile time",            "circle",     "palette2",  lst(),
  "vs",    "group2",       "Engine",                   "circle",     "palette2",  lst(),
  "am",    "group2",       "Transmission",             "circle",     "palette2",  lst(),
  "gear",  "group2",       "# Forward gears",          "circle",     "palette2",  lst(),
  "carb",  "group2",       "# Carburetors",            "circle",     "palette2",  lst(),
  "type",  "group2",       "Type of engine",           "image",      NA,          lst(path = "tests/testthat/test_images", filetype = "png")
)

row_info <- data %>% transmute(id, group = "test")
row_groups <- tibble(Group = "Test", group = "test")

column_groups <- tribble( # tribble_start
  ~Category,  ~group,         ~palette,
  "Overall",  "overall",      "overall",
  "Group 1",  "group1",       "palette1",
  "Group 2",  "group2",       "palette2"
) # tribble_end

palettes <- tribble(
  ~palette,             ~colours,
  "overall",            grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Greys")[-1]))(101),
  "palette1",           grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Blues") %>% c("#011636")))(101),
  "palette2",           grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Reds")[-8:-9]))(101)
)

g <- funky_heatmap(
    data = data,
    column_info = column_info,
    column_groups = column_groups,
    row_info = row_info,
    row_groups = row_groups,
    palettes = palettes,
    expand = list(xmax = 4)
)
ggsave("test2.pdf", g, device = "pdf")
