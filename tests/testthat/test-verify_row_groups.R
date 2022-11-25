row_info <- tribble(
  ~id, ~group,
  "foo1", "foo",
  "foo2", "foo",
  "bar1", "bar",
  "bar2", "bar"
)

test_that("verify_row_groups works", {
  row_groups <- tribble(
    ~group, ~level1,
    "foo", "Foo",
    "bar", "Bar"
  )

  out <- verify_row_groups(row_groups, row_info)

  expect_named(out, c("group", "level1"))
})

test_that("verify_row_groups generates if need be", {
  out <- verify_row_groups(NULL, row_info)

  expect_named(out, c("group", "level1"))
})
