make_geom_data_processor <- function(
  data,
  column_pos,
  row_pos,
  scale_column,
  palettes
) {
  function(geom_types, fun) {
    column_sels <-
      column_pos %>%
      filter(.data$geom %in% geom_types) %>%
      select(-"group", -"name", -"do_spacing") %>%
      rename(
        column_id = "id",
        column_color = "id_color",
        column_size = "id_size"
      ) %>%
      add_column_if_missing(
        label = NA_character_,
        scale = TRUE
      )

    if (nrow(column_sels) == 0) {
      # return a tibble with one row but no columns
      return(tibble(a = 1) %>% slice(integer()))
    }

    map_df(seq_len(nrow(column_sels)), function(ri) {
      # cat("Processing ", ri, "\n", sep = "")
      column_sel <-
        column_sels %>%
        slice(ri) %>%
        mutate(label = ifelse(
          is.na(.data$label),
          .data$column_id,
          .data$label
        ))

      row_sel <-
        row_pos %>%
        select(row_id = "id", "ysep", "y", "ymin", "ymax")

      data_sel <-
        data %>%
        select(
          row_id = "id",
          value = !!column_sel$column_id
          # color_value = !!column_sel$column_color,
          # size_value = !!column_sel$column_size
        ) %>%
        mutate(
          column_id = column_sel$column_id,
          column_color = column_sel$column_color,
          column_size = column_sel$column_size,
        )
      if (!is.na(column_sel$column_color)) {
        data_sel$color_value <- data[[column_sel$column_color]]
      } else {
        data_sel$color_value <- NA
      }
      if (!is.na(column_sel$column_size)) {
        data_sel$size_value <- data[[column_sel$column_size]]
      } else {
        data_sel$size_value <- NA
      }

      labelcolumn_sel <-
        column_sel %>%
        filter(!is.na(.data$label))

      if (nrow(labelcolumn_sel) > 0) {
        label_sel <-
          data %>%
          mutate(row_id = .data$id) %>%
          select("row_id", !!labelcolumn_sel$label) %>%
          gather("label_column", "label_value", -"row_id") %>%
          left_join(
            labelcolumn_sel %>% select(label_column = "label", "column_id"),
            by = "label_column"
          ) %>%
          select(-"label_column")
        data_sel <-
          left_join(data_sel, label_sel, by = c("row_id", "column_id"))
      }

      dat <-
        data_sel %>%
        left_join(column_sel, by = "column_id") %>%
        left_join(row_sel, by = "row_id")

      # scale data, if need be
      if (scale_column && column_sel$scale) {
        if (is.numeric(dat$value)) {
          dat$value <- scale_minmax(dat$value)
        }
        if (!is.null(dat$color_value) && !all(is.na(dat$color_value))) {
          dat$color_value <- scale_minmax(dat$color_value)
        }
        if (!is.null(dat$size_value) && !all(is.na(dat$size_value))) {
          dat$size_value <- scale_minmax(dat$size_value)
        }
      }

      # apply function
      dat <- fun(dat)

      # determine colours
      if (!is.na(column_sel$palette)) {
        palette_sel <- palettes[[column_sel$palette]]

        col_value <-
          if (is.character(dat$color_value) | is.factor(dat$color_value)) {
            dat$color_value
          } else if (is.numeric(dat$color_value)) {
            round(dat$color_value * (length(palette_sel) - 1)) + 1
          } else {
            NA
          }

        dat <- dat %>%
          mutate(
            colour = ifelse(
              is.na(col_value),
              "#444444FF",
              palette_sel[col_value]
            )
          )
        # TODO: previously 'value' was being removed here. Should 'value', 'color_value' and 'size_value' be removed?
      }

      dat
    })
  }
}
