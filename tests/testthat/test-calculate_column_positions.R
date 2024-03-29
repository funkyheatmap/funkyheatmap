column_info <- tribble(
  ~id, ~geom, ~group, ~width, ~overlay,
  "name", "text", "one", 1, FALSE,
  "x", "funkyrect", "two", 2, FALSE,
  "y", "funkyrect", "two", 3, FALSE,
  "z", "rect", "three", 2, FALSE,
  "z", "text", "three", 2, TRUE
)

test_that("test calculate_column_positions", {
  col_width <- 1
  col_space <- .1
  col_bigspace <- .5

  result <- calculate_column_positions(column_info, col_width, col_space, col_bigspace)

  expect_equal(nrow(result), nrow(column_info))
  expect_true(all(result$xmin + result$xwidth == result$xmax))
  expect_true(all(result$xmin + result$xwidth / 2 == result$x))

  # check spacing
  expect_equal(result$xsep[1:4], c(col_space, col_bigspace, col_space, col_bigspace))

  # Expect negative spacing for overlayed elements
  expect_equal(result$xsep[[5]], -2)
})

test_that("test calculate_column_positions with non-default values", {
  col_width <- 2
  col_space <- .5
  col_bigspace <- 10

  result <- calculate_column_positions(column_info, col_width, col_space, col_bigspace)

  expect_equal(nrow(result), nrow(column_info))
  expect_true(all(result$xmin + result$xwidth == result$xmax))
  expect_true(all(result$xmin + result$xwidth / 2 == result$x))

  # check spacing
  expect_equal(result$xsep[1:4], c(col_space, col_bigspace, col_space, col_bigspace))

  # Expect negative spacing for overlayed elements
  expect_equal(result$xsep[[5]], -2)
})
