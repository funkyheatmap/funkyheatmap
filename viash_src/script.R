library(readr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(funkyheatmap, warn.conflicts = FALSE)

## VIASH START
par <- list(
  "data" = "data.tsv",
  "column_info" = NULL,
  "row_info" = NULL,
  "column_groups" = NULL,
  "row_groups" = NULL,
  "palettes" = NULL,
  "scale_column" = TRUE,
  "add_abc" = TRUE,
  "col_annot_offset" = 3.0,
  "row_annot_offset" = 0.5,
  "removed_entries" = NULL,
  "expand" = c(0, 2, 0, 0),
  "output" = "output.pdf"
)
## VIASH END

# read in tsv files and preprocess other inputs
preproc <- list(
  data = readr::read_tsv,
  column_info = readr::read_tsv,
  row_info = readr::read_tsv,
  column_groups = readr::read_tsv,
  row_groups = readr::read_tsv,
  palettes = yaml::read_yaml,
  expand = function(x) {
    setNames(x, c("xmin", "xmax", "ymin", "ymax"))
  }
)

for (name in names(par)) {
  if (!is.null(par[[name]]) && name %in% names(preproc)) {
    cat("Applying preproc function on ", name, "\n", sep = "")
    par[[name]] <- preproc[[name]](par[[name]])
  }
}

# remove output from par
output <- par[["output"]]
par[["output"]] <- NULL

# run funky heatmap
g <- rlang::exec(funky_heatmap, !!!par)

# save as pdf
ggsave(
  filename = output,
  plot = g,
  # device = cairo_pdf,
  width = g$width,
  height = g$height
)