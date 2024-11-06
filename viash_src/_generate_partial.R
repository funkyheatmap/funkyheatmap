library(tidyverse)

html2md <- function(str) {
  file_html <- tempfile(fileext = ".html")
  file_md <- tempfile(fileext = ".md")
  readr::write_lines(str, file_html)
  out <- processx::run("pandoc", c(file_html, "-o", file_md))
  readr::read_lines(file_md)
}

# fetch data
pkg <- pkgdown::as_pkgdown()

# fetch funky heatmap topic
topic <- pkg$topics %>%
  filter(name == "funky_heatmap") %>%
  purrr::transpose() %>%
  .[[1]]

data <- pkgdown:::data_reference_topic(
  topic,
  pkg,
  examples_env = NULL,
  run_dont_run = FALSE
)

# generate description
fun_desc <- html2md(data$description$contents)
description <- paste(c(data$title, "", fun_desc), collapse = "\n")

# generate arguments
arg_desc <- html2md(data$sections[[1]]$contents)
arg_ix <- grep("^[^: ]", arg_desc)
arg_tib <-
  map2_df(arg_ix, c(arg_ix[-1] - 1, length(arg_desc)), function(from, to) {
    # remove links from argument names
    aname <- arg_desc[[from]] |> stringr::str_replace("\\[\\]\\(#.*", "")
    adesc <- arg_desc[seq(from + 2, to)] %>% sub("^....", "", .)
    tibble(name = aname, description = stringr::str_trim(paste(adesc, collapse = "\n")))
  }) %>%
  add_row(name = "output", description = "A funky heatmap.") %>%
  mutate(
    type = case_when(
      name %in% c("add_abc", "scale_column") ~ "boolean",
      name %in% c("col_annot_offset", "expand") ~ "double",
      TRUE ~ "file"
    ),
    direction = ifelse(name == "output", "output", "input"),
    required = name %in% c("input", "output"),
    example = map2(name, type, function(name, type) {
      if (name == "output") {
        paste0(name, ".pdf")
      } else if (type == "file") {
        paste0(name, ".yaml")
      } else {
        NA_character_
      }
    }),
    default = map(name, function(name) {
      if (name %in% c("scale_column", "add_abc")) {
        TRUE
      } else {
        NA_character_
      }
    }),
    multiple = name %in% c("expand"),
    multiple_sep = ifelse(multiple, ":", NA_character_),
    name = paste0("--", name)
  ) |>
  filter(!grepl("DEPRECATED", description))

arguments <- purrr::transpose(arg_tib) %>% map(function(x) {
  x[map_lgl(x, function(y) length(y) >= 1 && !is.na(y[[1]]))]
})

# generate authors
role_map <- c(aut = "Author", cre = "Maintainer", ctb = "Contributor")
authors <- map(pkg$desc$get_authors(), function(aut) {
  info <- as.list(aut$comment)
  names(info) <- tolower(names(info))
  list(
    name = paste0(aut$given, " ", aut$family),
    email = aut$email,
    info = info,
    roles = role_map[aut$role]
  )
})

# create config
config <- list(
  name = data$name,
  version = pkg$version,
  description = description,
  authors = authors,
  arguments = arguments
)

yaml::write_yaml(config, "viash_src/generated_partial.yaml")
