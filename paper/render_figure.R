library(funkyheatmap)
library(tidyverse)
library(Cairo)
library(RColorBrewer)

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

# Load the data, add the `id` column and sort based on mpg
data("mtcars")
data <- mtcars %>%
  rownames_to_column("id") %>%
  arrange(desc(mpg)) %>%
  head(30)
# sort the columns in logical groupings (see column_info group column)
data <- data[, c("id", "qsec", "mpg", "wt", "cyl", "carb", "disp", 
                 "hp", "vs", "drat", "am", "gear")]

# change data to use images
# change the am: if 0 go to "automatic", if 1 go to "manual"
data[data$am == 0, "am"] <- "automatic"
data[data$am == 1, "am"] <- "manual"
# change the vs: if 0 go to "vengine", if 1 go to "straight"
data[data$vs == 0, "vs"] <- "vengine"
data[data$vs == 1, "vs"] <- "straight"

column_info_part1 <- tribble(
  ~id,     ~group,         ~name,                   ~geom,
  "id",    "",             "Model",                 "text",
  "qsec",  "Performance",  "1/4 mile time",         "bar",
  "mpg",   "Overall",      "Number of cylinders",   "bar",
  "wt",    "Overall",      "Weight (1000 lbs)",     "bar", 
  "cyl",   "Engine",       "Number of cylinders",   "rect",  
  "cyl",   "Engine",       "",                      "text",
  "carb",  "Engine",       "Carburetors",           "rect",
  "carb",  "Engine",       "",                      "text",    
  "disp",  "Engine",       "Displacement",          "funkyrect",
  "hp",    "Engine",       "Horsepower",            "funkyrect",
  "vs",    "Engine",       "Engine type",           "image",
  "drat",  "Transmission", "Rear axle ratio",       "funkyrect",
  "am",    "Transmission", "Transmission",          "image",
  "gear",  "Transmission", "# Forward gears",       "rect",
  "gear",  "Transmission", "",                      "text"
)

column_info_part2 <- tribble(
  ~palette,               ~options,
  NA,                    lst(),
  "perf_palette",         lst(),
  "overall_palette",      lst(),
  "overall_palette",      lst(), 
  "engine_palette",       lst(),  
  "black",              lst(overlay = TRUE),
  "engine_palette",       lst(),
  "black",              lst(overlay = TRUE),    
  "engine_palette",       lst(),
  "engine_palette",       lst(),
  "engine_palette",       lst(directory = "vignettes/images", extension = "png"),
  "transmission_palette", lst(),
  "transmission_palette", lst(directory = "vignettes/images", extension = "png"),
  "transmission_palette", lst(),
  "black",              lst(overlay = TRUE)  
)

column_info <- bind_cols(column_info_part1, column_info_part2)

column_groups <- tribble(
  ~category,       ~group,          ~palette,
  "Performance",   "Performance",   "perf_palette",
  "Overall",       "Overall",       "overall_palette",
  "Engine",        "Engine",        "engine_palette",
  "Transmission",  "Transmission",  "transmission_palette"
)

palettes <- list(
  perf_palette = "Blues",
  overall_palette = "Greens",
  engine_palette = "YlOrBr",
  transmission_palette = "Reds",
  black = c("black", "black"),
  funky_palette_grey = RColorBrewer::brewer.pal(9, "Greys")[-1] %>% rev()
)

row_info <- data %>% transmute(id, group = ifelse(grepl("Merc", id), "Mercedes", "Other"))
# sort Mercedes cars to the top of the data and the row_info dataframe
data <- data[order(row_info$group), ]
row_info <- row_info[order(row_info$group), ]

row_groups <- tibble(level1 = c("Mercedes", "Other cars"), group = c("Mercedes", "Other"))

legends <- list(
    list(
        palette = "perf_palette",
        geom = "bar",
        title = "1/4 mile time",
        labels = c(paste0(min(data$qsec), "s"), rep("", 8), paste0(max(data$qsec), "s"))
    ),
    list(
        palette = "overall_palette",
        geom = "bar",
        title = "Miles per gallon",
        labels = c(paste0(min(data$mpg), "mpg"), rep("", 8), paste0(max(data$mpg), "mpg"))
    ),
    list(
        palette = "overall_palette",
        geom = "bar",
        title = "Weight",
        labels = c(paste0(min(data$wt), "lbs"), rep("", 8), paste0(max(data$wt), "lbs"))
    ),
    list(
        palette = "funky_palette_grey",
        geom = "funkyrect",
        title = "Overall",
        enabled = TRUE,
        labels = c("0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1")
    ),
    list(
        palette = "engine_palette",
        enabled = FALSE
    ),
    list(
        palette = "transmission_palette",
        enabled = FALSE
    )
)


g2 <- funky_heatmap(
  data,
  column_info = column_info,
  column_groups = column_groups,
  row_info = row_info,
  row_groups = row_groups,
  palettes = palettes,
  legends = legends
)
print(getwd())
Cairo::CairoSVG("paper/figure2.svg", width = g2$width * 2, height = g2$height * 2)
g2
dev.off()

Cairo::CairoPDF("paper/figure2.pdf", width = g2$width * 1.5, height = g2$height * 1.5)
g2
dev.off()
