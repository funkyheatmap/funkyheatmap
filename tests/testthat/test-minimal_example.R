require(dplyr)
require(jsonlite)
require(readr)

DEBUG <- FALSE

dir <- "test_data/minimal_"
image_dir_test <- ""

if (DEBUG) {
  dir <- "tests/testthat/test_data/minimal_"
  image_dir_test <- "tests/testthat/"
}

data <- readr::read_tsv(paste0(dir, "data.tsv")) %>%
  mutate_at(
    c("categories1", "categories2", "categories3"),
    function(column) {
      lapply(column, function(val) {
        unlist(jsonlite::fromJSON(val))
      })
    }
  ) %>%
  mutate_at(vars(ends_with("_str")), as.character)
column_info <- readr::read_tsv(paste0(dir, "column_info.tsv"))
column_groups <- readr::read_tsv(paste0(dir, "column_groups.tsv"))
row_info <- readr::read_tsv(paste0(dir, "row_info.tsv"))
row_groups <- readr::read_tsv(paste0(dir, "row_groups.tsv"))
palettes <- jsonlite::read_json(paste0(dir, "palettes.json"), simplifyVector = TRUE)
palettes$pie <- unlist(palettes$pie)

if (DEBUG){
  column_info <- column_info %>% 
    mutate(directory = if_else(is.na(directory), NA, paste0(image_dir_test, "test_data")) )
  data <- data %>%
    mutate(image_full = paste0(image_dir_test, image_full))
}

legends <- list(
  list(
    title = "Bar",
    palette = "bar",
    enabled = TRUE,
    geom = "bar"
  ),
  list(
    title = "Text",
    palette = "black_text",
    enabled = TRUE,
    geom = "text",
    labels = c("propA", "propB", "propC"),
    values = c("A", "B", "C")
  ),
  list(
    title = "Image",
    palette = "black_text",
    enabled = TRUE,
    geom = "image",
    size = 3.88,
    labels = c(paste0(image_dir_test, "test_data/one.png"), paste0(image_dir_test, "test_data/two.png"), paste0(image_dir_test, "test_data/three.png")),
    values = c("One", "Two", "Three")
  )
)

test_that("minimal example funky_heatmap works", {
  g <- funkyheatmap::funky_heatmap(
    data = data,
    column_info = column_info,
    column_groups = column_groups,
    row_info = row_info,
    row_groups = row_groups,
    palettes = palettes,
    legends = legends
  )
  expect_true(ggplot2::is.ggplot(g))
  ggsave(nullfile(), g, device = "pdf")
})
