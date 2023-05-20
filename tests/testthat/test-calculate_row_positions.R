test_that("calculate_row_positions computes row positions correctly", {
  row_info <- tribble(
    ~id, ~group,
    "foo1", "foo",
    "foo2", "foo",
    "bar1", "bar",
    "bar2", "bar"
  )

  row_height <- 1
  row_space <- .1
  row_bigspace <- .5

  result <- calculate_row_positions(row_info, row_height, row_space, row_bigspace)

  expect_equal(nrow(result), nrow(row_info))
  expect_true(all(result$ymin + row_height == result$ymax))
  expect_true(all(result$ymin + row_height / 2 == result$y))

  # check spacing
  expect_equal(result$ysep, c(row_space, row_space, row_bigspace, row_space))

  # Verify spacing calculation
  expect_equal(result$ysep[1], row_space)
  expect_equal(result$ysep[2], row_space)
  expect_equal(result$ysep[3], row_height + 2 * row_space)
  expect_equal(result$ysep[4], row_space)
})
