data <- tribble(
  ~id, ~name, ~x, ~y,
  "foo", "Foo", 0.5, 0.7,
  "bar", "Bar", 1.0, 0.1
)
column_info <- tribble(
  ~id, ~geom, ~palette,
  "name", "text", NA,
  "foo", "funkyrect", "pal1",
  "bar", "funkyrect", "pal2"
)

test_that("verify_palettes works with explicit form", {
  palettes <- list(
    pal1 = rev(brewer.pal(9, "Greys")[-1]),
    pal2 = rev(brewer.pal(9, "Reds")[-8:-9])
  )
  out <- verify_palettes(palettes, column_info, data)

  expect_named(out, c("pal1", "pal2"))
})

test_that("implicit palettes works", {
  palettes <- list(
    pal1 = "Greys",
    pal2 = "Reds"
  )
  out <- verify_palettes(palettes, column_info, data)

  expect_named(out, c("pal1", "pal2"))
  expect_gte(length(palettes$pal1), 1)
  expect_gte(length(palettes$pal2), 1)
})

test_that("passing a tibble should also work (for backwards compatibility)", {
  palettes <- tribble(
    ~palette, ~colours,
    "pal1", rev(brewer.pal(9, "Greys")[-1]),
    "pal2", rev(brewer.pal(9, "Reds")[-8:-9])
  )

  out <- verify_palettes(palettes, column_info, data)
  expect_named(out, c("pal1", "pal2"))
})
