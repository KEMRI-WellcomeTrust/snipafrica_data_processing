meta <- exportMetaData(snip_main_con)  ### Export meta data(data disctionary)
meta_expanded <- exportMetaData(snip_main_con) %>%
  rowwise() %>%
  mutate(
    field_name_expanded = if (field_type == "checkbox") {
      # extract numbers, convert to integer, sort them numerically
      nums <- str_extract_all(select_choices_or_calculations, "\\d+")[[1]]
      list(paste0(field_name, "___", sort(as.integer(nums))))
    } else {
      list(field_name)
    }
  ) %>%
  unnest(field_name_expanded) %>%
  mutate(field_name = field_name_expanded) %>%
  select(-field_name_expanded)


