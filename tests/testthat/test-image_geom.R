test_that("image geom with full paths", {
  data <- mtcars %>%
    rownames_to_column("id") %>%
    arrange(desc(mpg)) %>%
    head(20) %>%
    mutate(type = c(rep("test_images/ice.png", 10), rep("test_images/electric.png", 10)))
  column_info <- tribble(
    ~id, ~group, ~name, ~geom, ~palette, ~options,
    "id", NA, "", "text", NA_character_, list(hjust = 0, width = 6),
    "mpg", NA, "Miles / gallon", "bar", NA_character_, list(width = 4, legend = FALSE),
    "type", NA, "Type of engine", "image", NA_character_, lst()
  )
  g <- funky_heatmap(
    data = data,
    column_info = column_info,
    position_args = position_arguments(expand_xmax = 4)
  )
  expect_true(ggplot2::is.ggplot(g))
})
test_that("image geom with directory and extension options", {
  data <- mtcars %>%
    rownames_to_column("id") %>%
    arrange(desc(mpg)) %>%
    head(20) %>%
    mutate(type = c(rep("ice", 10), rep("electric", 10)))
  column_info <- tribble(
    ~id, ~group, ~name, ~geom, ~palette, ~options,
    "id", NA_character_, "", "text", NA_character_, list(hjust = 0, width = 6),
    "mpg", NA_character_, "Miles / gallon", "bar", NA_character_, list(width = 4, legend = FALSE),
    "type", NA_character_, "Type of engine", "image", NA_character_, list(directory = "test_images/", extension = "png")
  )

  g <- funky_heatmap(
    data = data,
    column_info = column_info,
    position_args = position_arguments(expand_xmax = 4)
  )
  expect_true(ggplot2::is.ggplot(g))
})
