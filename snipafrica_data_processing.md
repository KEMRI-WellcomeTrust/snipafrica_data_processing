SNIP Africa Data processing
================
Data Team
2025-05-24

## Load libraries

``` r
library(tidyverse)
library(here)
library(lubridate)
library(kableExtra)
library(janitor)
library(table1)
library(readr)
library(ggplot2)
library(DT)
library(data.table)
library(haven)
library(scales)
library(readr)
library(stringr)
```

## Path to saving to server

``` r
file_path = "/srv/shiny-server/snip/processed_data/"
```

## Load datasets

``` r
weekly_screening <- read_csv(here("raw_data/weekly_screening.csv"))
entry <- read_csv(here("raw_data/entry.csv"))
exit <- read_csv(here("raw_data/exit.csv"))
bc_ast_entry <- read_csv(here("raw_data/bc_ast_entry.csv"))
bc_rm_entry <- read_csv(here("raw_data/bc_rm_entry.csv"))
csf_ast_entry <- read_csv(here("raw_data/csf_ast_entry.csv"))
csf_rm_entry <- read_csv(here("raw_data/csf_rm_entry.csv"))
```

## Weekly screening

``` r
date_vars <- c(
  "s_date_scr" ,"e_date_scr","date_crf_complete_scr")
               
# Process dates 
walk(date_vars, function(var) {
  weekly_screening[[var]] <<- as.Date(format(
    as.POSIXct(as.character(zap_labels(weekly_screening[[var]])), tz = "Africa/Nairobi"),
    tz = "Africa/Nairobi" ))})

# remove unwanted columns
weekly_screening <- weekly_screening %>% 
  select(-c(redcap_repeat_instance,redcap_event_name,redcap_repeat_instrument,redcap_data_access_group,redcap_event_name))

save(weekly_screening,file = paste0(file_path, "weekly_screening.csv") )
```

``` r
weekly_screening_summary <- weekly_screening %>%
  group_by(week_scr,site_scr) %>%
  summarise(
    Screened = sum(t_screen_scr,na.rm = TRUE),
    Eligible = sum(t_eligible_scr, na.rm = TRUE),
    Not_eligible = sum(t_not_eligible_scr, na.rm = TRUE),
    Enrolled = sum(t_eligible_enrol_scr, na.rm = TRUE),
    Not_Enrolled = sum(t_eligible_not_enrol_scr, na.rm = TRUE),
    Refused_missed_consent = sum(refuse_miss_consent_scr, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>% 
  adorn_totals(where = "row")

weekly_screening_summary <- weekly_screening_summary %>%
  rename(
    Site = site_scr,
    `Screened` = Screened,
    `Eligible` = Eligible,
    `Not Eligible` = Not_eligible,
    `Enrolled` = Enrolled,
    `Not Enrolled` = Not_Enrolled,
    `Refused/Missed Consent` = Refused_missed_consent)

#kbl(weekly_screening_summary, valign = "t", booktabs = T,linesep = "\\addlinespace") %>% 
#kable_styling(latex_options = "striped",full_width = T,font_size = 14) 
```
