#' Generating funky heatmaps for benchmarks
#'
#' Allows generating heatmap-likes visualisations for benchmark data
#' frames. Funky heatmaps can be fine-tuned by providing annotations of the
#' columns and rows, which allows assigning multiple palettes or geometries
#' or grouping rows and columns together in categories.
#'
#' To learn more about funkyheatmap, please visit the
#' [project page](https://funkyheatmap.github.io/funkyheatmap/).
#'
#' This package heavily depends on the functions provided by
#' [`ggplot2`](https://ggplot2.tidyverse.org/) and
#' [`ggforce`](https://ggforce.data-imaginist.com).
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom assertthat assert_that %has_name%
#' @importFrom tibble tibble enframe deframe lst tribble as_tibble rownames_to_column add_column
#' @importFrom dplyr filter select pull mutate group_by row_number ungroup
#' @importFrom dplyr case_when transmute slice left_join inner_join n bind_cols
#' @importFrom dplyr bind_rows rename summarise arrange desc first last distinct
#' @importFrom dplyr slice_head
#' @importFrom tidyr crossing gather unnest
#' @importFrom purrr %>% %||% walk set_names map map_dbl map_lgl map_chr map_df
#' @importFrom purrr map2 map2_dbl map2_lgl map2_chr map2_df pmap_df pmap
#' @importFrom stringr str_count str_to_title
#' @importFrom utils head tail
#' @importFrom stats na.omit
#' @importFrom cli cli_alert_warning cli_alert_info
#' @importFrom Rdpack reprompt
#' @importFrom cowplot draw_image
#' @import ggplot2
## usethis namespace: end
NULL
