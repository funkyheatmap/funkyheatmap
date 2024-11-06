if_list_to_tibble <- function(data) {
  if (is.list(data) && !is.data.frame(data)) {
    # transpose if data is not a list of named lists but
    # instead a named list of vectors and lists
    if (!is.null(names(data))) {
      data <- purrr::transpose(data)
    }

    # convert list of named lists to tibble
    data <- purrr::map_dfr(data, function(row) {
      tibble::as_tibble(purrr::map(row, function(value) {
        if (is.list(value) || length(value) != 1) {
          value <- list(value)
        }
        value
      }))
    })
  }
  data
}
