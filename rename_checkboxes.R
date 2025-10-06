
rename_checkboxes <- function(df, meta) {
  
  # Step 1: expand metadata for checkboxes
  mapping <- meta %>%
    filter(field_type == "checkbox") %>%
    select(variable_name = field_name, choices = select_choices_or_calculations) %>%
    separate_rows(choices, sep = "\\|") %>%
    mutate(
      choices = str_trim(choices),
      choice_value = str_extract(choices, "^[^,]+"),
      choice_label = str_trim(str_remove(choices, "^[^,]+,"))
    ) %>%
    select(variable_name, choice_value, choice_label)
  
  # Step 2: rename dataset columns
  new_names <- names(df)
  
  for (i in seq_along(new_names)) {
    col <- new_names[i]
    
    if (str_detect(col, "___")) {
      base_var <- str_remove(col, "___.*$")
      choice_num <- str_extract(col, "(?<=___)\\d+$")
      
      label <- mapping %>%
        filter(variable_name == base_var,
               choice_value == choice_num) %>%
        pull(choice_label)
      
      if (length(label) == 1) {
        clean_var <- str_remove(base_var, "_scr$")   # drop `_scr`
        new_names[i] <- paste0(clean_var, "_", str_replace_all(label, "\\s+", "_"))
      }
    }
  }
  
  names(df) <- new_names
  return(df)
}