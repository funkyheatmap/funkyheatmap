library(readr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(funkyheatmap, warn.conflicts = FALSE)

## VIASH START
par <- list(
  ...
)
## VIASH END

for (name in names(par)) {
  if (!is.null(par[[name]]) && is.character(par[[name]]) && file.exists(par[[name]])) {
    fun <-
      if (grepl("\\.tsv$", par[[name]])) {
        readr::read_tsv
      } else if (grepl("\\.yaml$", par[[name]])) {
        yaml::read_yaml
      } else if (grepl("\\.csv$", par[[name]])) {
        readr::read_csv
      } else if (grepl("\\.json$", par[[name]])) {
        jsonlite::fromJSON
      } else {
        NULL
      }

    if (!is.null(fun)) {
      ext <- tools::file_ext(par[[name]])
      cat("Trying to parse argument ", name, " as a ", ext, " (", par[[name]], ")\n", sep = "")
      par[[name]] <- fun(par[[name]])
    }
  }
}

# remove output from par
output <- par[["output"]]
par[["output"]] <- NULL

# remove NULLs from par
par <- par[sapply(par, Negate(is.null))]

# run funkyheatmap
g <- rlang::exec(funky_heatmap, !!!par)

# save as pdf
ggsave(
  filename = output,
  plot = g,
  width = g$width,
  height = g$height
)
