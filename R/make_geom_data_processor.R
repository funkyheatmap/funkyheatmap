make_geom_data_processor <- function(
  data,
  column_pos,
  row_pos,
  scale_column,
  palette_list
) {
  function(geom_types, fun) {
    column_sels <-
      column_pos %>%
      filter(.data$geom %in% geom_types) %>%
      select(-.data$group, -.data$name, -.data$do_spacing) %>%
      rename(column_id = .data$id) %>%
      add_column_if_missing(
        label = NA_character_,
        scale = TRUE
      )
    
    if (nrow(column_sels) == 0) {
      # return(tibble(a = 1) %>% slice(integer()))
      return(tibble())
    }
    
    map_df(seq_len(nrow(column_sels)), function(ri) {
      # cat("Processing ", ri, "\n", sep = "")
      column_sel <-
        column_sels %>%
        slice(ri) %>%
        mutate(label = ifelse(
          .data$geom == "text" & is.na(.data$label),
          .data$column_id,
          .data$label)
        )
      
      row_sel <-
        row_pos %>%
        select(row_id = .data$id, .data$ysep, .data$y, .data$ymin, .data$ymax)
      
      data_sel <-
        data %>%
        select(row_id = .data$id, value = !!column_sel$column_id) %>%
        mutate(column_id = column_sel$column_id)
      
      labelcolumn_sel <-
        column_sel %>%
        filter(!is.na(.data$label))
      
      if (nrow(labelcolumn_sel) > 0) {
        label_sel <-
          data %>%
          mutate(row_id = .data$id) %>%
          select(.data$row_id, !!labelcolumn_sel$label) %>%
          gather("label_column", "label_value", -.data$row_id) %>%
          left_join(
            labelcolumn_sel %>% select(label_column = "label", "column_id"), 
            by = "label_column"
          ) %>%
          select(-.data$label_column)
        data_sel <-
          left_join(data_sel, label_sel, by = c("row_id", "column_id"))
      }
      
      dat <-
        data_sel %>%
        left_join(column_sel, by = "column_id") %>%
        left_join(row_sel, by = "row_id")
      
      # scale data, if need be
      if (scale_column && column_sel$scale && is.numeric(dat$value)) {
        dat <-
          dat %>%
          group_by(.data$column_id) %>%
          mutate(value = scale_minmax(.data$value)) %>%
          ungroup()
      }
      
      # apply function
      dat <- fun(dat)
      
      # determine colours
      if (!is.na(column_sel$palette)) {
        palette_sel <- palette_list[[column_sel$palette]]
        
        if (is.character(dat$value) | is.factor(dat$value)) {
          dat <- dat %>% mutate(col_value = .data$value)
        } else if (is.numeric(dat$value)) {
          dat <- dat %>% mutate(
            col_value = round(.data$value * (length(palette_sel) - 1)) + 1
          )
        } else {
          dat$col_value <- NA
        }
        
        dat <- dat %>%
          mutate(
            colour = ifelse(
              is.na(.data$col_value), 
              "#444444FF", 
              palette_sel[col_value]
            )
          ) %>%
          select(-.data$value, -.data$col_value)
      }
      
      dat
    })
  }
}
