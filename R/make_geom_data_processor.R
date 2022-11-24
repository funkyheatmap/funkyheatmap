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
      filter(geom %in% geom_types) %>%
      select(-group, -name, -do_spacing) %>%
      rename(column_id = id) %>%
      add_column_if_missing(label = NA_character_, scale = TRUE)
    
    if (nrow(column_sels) == 0) {
      return(tibble(a = 1) %>% slice(integer()))
    }
    
    map_df(seq_len(nrow(column_sels)), function(ri) {
      # cat("Processing ", ri, "\n", sep = "")
      column_sel <-
        column_sels %>%
        slice(ri) %>%
        mutate(label = ifelse(geom == "text" & is.na(label), column_id, label))
      
      row_sel <-
        row_pos %>%
        select(row_id = id, ysep, y, ymin, ymax)
      
      data_sel <-
        data %>%
        select(row_id = id, value = !!column_sel$column_id) %>%
        mutate(column_id = column_sel$column_id)
      
      labelcolumn_sel <-
        column_sel %>%
        filter(!is.na(label))
      
      if (nrow(labelcolumn_sel) > 0) {
        label_sel <-
          data %>%
          mutate(row_id = id) %>%
          select(row_id, !!labelcolumn_sel$label) %>%
          gather(label_column, label_value, -row_id) %>%
          left_join(labelcolumn_sel %>% select(label_column = label, column_id), by = "label_column") %>%
          select(-label_column)
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
          group_by(column_id) %>%
          mutate(value = scale_minmax(value)) %>%
          ungroup()
      }
      
      # apply function
      dat <- fun(dat)
      
      # determine colours
      if (!is.na(column_sel$palette)) {
        palette_sel <- palette_list[[column_sel$palette]]
        
        if (is.character(dat$value) | is.factor(dat$value)) {
          dat <- dat %>% mutate(col_value = value)
        } else if (is.numeric(dat$value)) {
          dat <- dat %>% mutate(col_value = round(value * (length(palette_sel) - 1)) + 1)
        } else {
          dat$col_value <- NA
        }
        
        dat <- dat %>%
          mutate(colour = ifelse(is.na(col_value), "#444444FF", palette_sel[col_value])) %>%
          select(-value, -col_value)
      }
      
      dat
    })
  }
}
