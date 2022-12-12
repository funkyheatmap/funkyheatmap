library(readr)
library(stringr)
library(cli)
library(purrr)
library(googledrive)
d2b <- reticulate::import("doi2bib")

cli::cli_alert("google drive authentication")
googledrive::drive_auth(email = Sys.getenv("GOOGLE_DRIVE_EMAIL"))

cli::cli_alert("download manuscript from gdrive")
temp_manuscript <- tempfile()
dest_manuscript <- "manuscript/paper.qmd"
dest_library <- "manuscript/library.bib"

drive <- drive_download(
  as_id("1bQRWcRk8bgoYQA-CBynXgbFO9MU-mLdQBehX5lIEqeY"),
  type = "docx",
  overwrite = TRUE,
  path = temp_manuscript
)

cli::cli_alert("read docx and write to qmd")
doc <- officer::read_docx(drive$local_path)
content <- officer::docx_summary(doc)

content$text %>%
  # add spaces before citations
  # str_replace_all("([^ ])(\\[@[^\\]]*\\])", "\\1 \\2") %>% 
  # add newlines before sections
  str_replace_all("^#", "\n#") %>%
  write_lines(dest_manuscript)


cli::cli_alert("extract citation yaml")
manu_txt <- read_lines(dest_manuscript) %>% paste(collapse = "\n")
citations_yaml <- gsub(".*```\\{citations[^\n]*\n([^`]*)```.*", "\\1", manu_txt)
citations <- yaml::read_yaml(text = citations_yaml)$citations

cli::cli_alert("convert to bibtex")
bibs <- map2_chr(names(citations), citations, function(name, text) {
  bib <-
    if (grepl("^@", text)) {
      # text is already a bibtex
      text
    } else {
      out <- d2b$crossref$get_bib_from_doi(text)
      if (!out[[1]]) {
        cli::cli_alert_warning(paste0("Could not find doi '", text, "'"))
        ""
      } else {
        out[[2]]
      }
    }
  gsub("(@[a-zA-Z]+\\{)[A-Za-z0-9_-]+", paste0("\\1", name), bib)
})

cli::cli_alert("write library file")
write_lines(bibs, dest_library)
