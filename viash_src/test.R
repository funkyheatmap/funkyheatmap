library(readr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(funkyheatmap, warn.conflicts = FALSE)
library(assertthat, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)

data_path <- "data.tsv"
output_path <- "output.pdf"

data <- mtcars %>% rownames_to_column("id")
write_tsv(data, data_path)

out <- processx::run(
  meta[["executable"]],
  args = c(
    "--data", data_path,
    "--output", output_path
  ),
  echo = TRUE
)

assert_that(file.exists(output_path))