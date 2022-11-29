data <- tribble(
  ~id, ~name, ~x, ~y,
  "foo", "Foo", 0.5, 0.7,
  "bar", "Bar", 1.0, 0.1
)

test_that("verify_column_info works", {
  column_info <- tribble(
    ~id, ~geom,
    "name", "text",
    "x", "funkyrect",
    "y", "funkyrect"
  )

  out <- verify_column_info(column_info, data)

  expect_true(out %has_name% c("id", "geom", "name", "group", "palette", "width"))
})

test_that("verify_column_info works with no info", {
  out <- verify_column_info(NULL, data)

  expect_true(out %has_name% c("id", "geom", "name", "group", "palette", "width"))
})


test_that("parsing json works", {
  column_info <- tribble(
    ~id, ~geom, ~options,
    "name", "text", '{"width": 3}',
    "x", "funkyrect", '{}',
    "y", "funkyrect", '{"halign": 0.7}',
  )
  
  out <- verify_column_info(column_info, data)
  
  expect_true(out %has_name% c("id", "geom", "name", "group", "palette", "width"))
})