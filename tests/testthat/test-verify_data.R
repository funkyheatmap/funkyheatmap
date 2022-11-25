data <- tribble(
    ~id, ~name, ~x, ~y,
    "foo", "Foo", 0.5, 0.7,
    "bar", "Bar", 1.0, 0.1
  )

test_that("verify_data works", {
  out <- verify_data(data)
  expect_named(out, c("id", "name", "x", "y"))
})

test_that("verify_data throws error if id is not found", {
  expect_error({
    out <- verify_data(data[,-1])
  })
})