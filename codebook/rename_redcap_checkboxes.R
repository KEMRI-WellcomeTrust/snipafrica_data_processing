rename_redcap_checkboxes <- function(df, meta) {
  
  # Identify checkbox variables in metadata
  checkbox_vars <- meta %>%
    filter(field_type == "checkbox") %>%
    pull(field_name)
  
  # Loop through checkbox variables
  for (var in checkbox_vars) {
    
    # Extract choice labels from metadata (format: "1, Fever | 2, Cough | 3, Vomiting")
    choices <- str_split(meta$select_choices_or_calculations[meta$field_name == var], "\\|")[[1]]
    choices <- str_trim(choices)
    choice_map <- sapply(choices, function(x) str_split(x, ",")[[1]][2] %>% str_trim())
    names(choice_map) <- sapply(choices, function(x) str_split(x, ",")[[1]][1] %>% str_trim())
    
    # Select columns corresponding to this checkbox variable
    checkbox_cols <- grep(paste0("^", var, "___"), names(df), value = TRUE)
    
    # Rename columns
    new_names <- sapply(checkbox_cols, function(col) {
      suffix <- str_remove(col, paste0(var, "___"))
      label <- choice_map[suffix]
      if (is.na(label)) label <- suffix
      label <- make.names(label)  # make valid R names
      return(label)
    })
    
    df <- df %>% rename_at(vars(all_of(checkbox_cols)), ~ new_names)
  }
  
  return(df)
}