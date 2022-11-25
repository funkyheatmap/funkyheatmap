data <- tribble(
  ~id, ~name, ~x, ~y,
  "foo1", "Foo1", 0.5, 0.7,
  "foo2", "Foo2", 0.5, 0.8,
  "bar1", "Bar1", 1.0, 0.2,
  "bar2", "Bar2", 1.0, 0.1
)

test_that("verify_row_info works", {
  row_info <- tribble(
    ~id, ~group,
    "foo1", "foo",
    "foo2", "foo",
    "bar1", "bar",
    "bar2", "bar"
  )

  out <- verify_row_info(row_info, data)

  expect_named(out, c("id", "group"))
})

test_that("verify_row_info generates if need be", {
  out <- verify_row_info(NULL, data)

  expect_named(out, c("id", "group"))
})
