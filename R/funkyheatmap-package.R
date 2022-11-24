#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
## usethis namespace: end
NULL


#' Generating funky heatmaps for benchmarks
#'
#' @importFrom tibble tibble enframe deframe lst tribble as_tibble
#' @importFrom dplyr filter select pull mutate group_by row_number ungroup case_when transmute slice left_join inner_join n bind_cols bind_rows rename summarise arrange desc first last 
#' @importFrom tidyr crossing gather
#' @importFrom purrr %>% %||% walk set_names map map_dbl map_lgl map_chr map_df map2 map2_dbl map2_lgl map2_chr map2_df pmap_df
#' @importFrom stringr str_count
#' @importFrom dynutils scale_minmax
#' @importFrom utils head tail
#' @import ggplot2
#'
#' @docType package
#' @name funkyheatmap-package
NULL
