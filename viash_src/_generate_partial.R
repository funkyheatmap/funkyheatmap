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
    aname <- arg_desc[[from]]
    adesc <- arg_desc[seq(from + 2, to)] %>% sub("^....", "", .)
    tibble(name = aname, description = stringr::str_trim(paste(adesc, collapse = "\n")))
  }) %>%
  add_row(name = "output", description = "A funky heatmap.") %>%
  mutate(
    type = case_when(
      name %in% c("add_abc", "scale_column") ~ "boolean",
      name %in% c("col_annot_offset", "row_annot_offset", "expand") ~ "double",
      name %in% c("removed_entries") ~ "string",
      TRUE ~ "file"
    ),
    direction = ifelse(name == "output", "output", "input"),
    required = name %in% c("input", "output"),
    example = map2(name, type, function(name, type) {
      if (name == "palettes") {
        paste0(name, ".yaml")
      } else if (name == "output") {
        paste0(name, ".pdf")
      } else if (type == "file") {
        paste0(name, ".tsv")
      } else if (name == "removed_entries") {
        c("sample1", "sample2", "sample3")
      } else {
        NA_character_
      }
    }),
    default = map(name, function(name) {
      if (name %in% c("scale_column", "add_abc")) {
        TRUE
      } else if (name == "col_annot_offset") {
        3
      } else if (name == "row_annot_offset") {
        .5
      } else if (name == "expand") {
        c(0, 2, 0, 0)
      } else {
        NA_character_
      }
    }),
    multiple = name %in% c("removed_entries", "expand"),
    multiple_sep = ifelse(multiple, ":", NA_character_),
    name = paste0("--", name)
  )

arguments <- purrr::transpose(arg_tib) %>% map(function(x) {
  x[map_lgl(x, function(y) length(y) >= 1 && !is.na(y[[1]]))]
})

# generate authors
role_map <- c(aut = "Author", cre = "Maintainer")
authors <- map(pkg$desc$get_authors(), function(aut) {
  props <- as.list(aut$comment)
  names(props) <- tolower(names(props))
  list(
    name = paste0(aut$given, " ", aut$family),
    email = aut$email,
    props = props,
    roles = role_map[aut$role]
  )
})

# create config
config <- list(
  functionality = list(
    name = data$name,
    version = pkg$version,
    description = description,
    authors = authors,
    arguments = arguments
  )
)

yaml::write_yaml(config, "viash/generated_sections.yaml")


