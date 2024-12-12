require(dplyr)
require(jsonlite)
require(readr)

dir <- "test_data/minimal_"
# dir <- "tests/testthat/test_data/minimal_"

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

legends <- list(
  list(
    title = "Text",
    palette = "black_text",
    enabled = TRUE,
    geom = "text",
    labels = c("propA", "propB", "propC"),
    values = c("Property of A", "Property of B", "Property of C")
  )
)

test_that("minimal example funky_heatmap works", {
  g <- funky_heatmap(
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
