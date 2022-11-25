column_info <- tribble(
  ~id, ~geom, ~group,
  "name", "text", NA_character_,
  "foo1", "funkyrect", "foo",
  "foo2", "funkyrect", "foo",
  "bar1", "funkyrect", "bar",
  "bar2", "funkyrect", "bar"
)

test_that("verify_column_groups works", {
  column_groups <- tribble(
    ~group, ~level1,
    "foo", "Foo",
    "bar", "Bar"
  )

  out <- verify_column_groups(column_groups, column_info)

  expect_named(out, c("group", "level1", "palette"), ignore.order = TRUE)
})

test_that("verify_column_groups generates if need be", {
  out <- verify_column_groups(NULL, column_info)

  expect_named(out, c("group", "level1", "palette"), ignore.order = TRUE)
})
