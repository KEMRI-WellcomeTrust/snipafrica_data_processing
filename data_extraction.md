Data extraction
================
Data Team
2025-05-24

``` r
library(tidyverse)
library(redcapAPI)
library(lubridate)
library(here)
library(httr)
```

``` r
snip_token <-"6185C58565F31D631F12085E94042A1F"
url <- "https://apps.chainnetwork.org/studies/redcap_v14.3.3/API/"
httr::set_config(config(ssl_verifypeer = 0L))
snip_con <- redcapConnection(url = url, token = snip_token)
forms <- exportInstruments(rcon = snip_con)
arms <- exportArms(rcon = snip_con)
```

## Weekly screening data

``` r
weekly_screening <- exportRecordsTyped(
  rcon = snip_con,
  records = NULL,
  forms = "weekly_screening",
  survey = FALSE, 
  factors = FALSE,
  dag = T,
  checkboxLabels = FALSE
)
```

## All snip data

``` r
snip_df <- exportRecordsTyped(
  rcon = snip_con,
  records = NULL,
  forms = NULL,
  survey = FALSE, 
  factors = FALSE,
  dag = T,
  checkboxLabels = FALSE
)
# Replace "Unchecked" with NA in all factor columns
snip_df[sapply(snip_df, is.factor)] <- lapply(snip_df[sapply(snip_df, is.factor)], function(x) {
  x[x == "Unchecked"] <- NA
  return(x)
})
```

### Get datasets according to events

``` r
## weekly screening
write_csv(weekly_screening, 
          file = here("raw_data", "weekly_screening.csv"))

save(weekly_screening,
     file = here("raw_data", "weekly_screening.rda"))

## Entry
entry <- snip_df %>% dplyr::filter(redcap_event_name == "Entry (Arm 1: Main Database)") 
entry <- entry %>% dplyr::filter(is.na(redcap_repeat_instrument))
entry <- entry %>% select(where(~ any(!is.na(.))))
write_csv(entry, file = here("raw_data", "entry.csv"))
save(entry,file = here("raw_data", "entry.rda"))

## Exit 
exit <- snip_df %>% filter(redcap_event_name == "Exit (Arm 1: Main Database)")
exit <- exit %>% select(where(~ any(!is.na(.))))
write_csv(exit, file = here("raw_data","exit.csv"))
save(exit,file = here("raw_data", "exit.rda"))

##Blood culture AST - Entry
bc_ast_entry <- snip_df %>% filter(redcap_event_name == "Entry (Arm 1: Main Database)") 
bc_ast_entry <- bc_ast_entry %>% dplyr::filter(redcap_repeat_instrument == "Blood Culture AST")
bc_ast_entry <- bc_ast_entry %>% select(where(~ any(!is.na(.))))
write_csv(bc_ast_entry, file = here("raw_data","bc_ast_entry.csv"))

## Blood culture Resistant marker results - Entry
bc_rm_entry <- snip_df %>% filter(redcap_event_name == "Entry (Arm 1: Main Database)") 
bc_rm_entry <- bc_rm_entry %>% dplyr::filter(redcap_repeat_instrument == "Blood Culture Resistant Markers Results")
bc_rm_entry <- bc_rm_entry %>% select(where(~ any(!is.na(.))))
write_csv(bc_rm_entry, file = here("raw_data","bc_rm_entry.csv"))

## CSF culture AST - Entry
csf_ast_entry <- snip_df %>% filter(redcap_event_name == "Entry (Arm 1: Main Database)") 
csf_ast_entry <- csf_ast_entry %>% dplyr::filter(redcap_repeat_instrument == "CSF culture AST")
csf_ast_entry <- csf_ast_entry %>% select(where(~ any(!is.na(.))))
write_csv(csf_ast_entry, file = here("raw_data","csf_ast_entry.csv"))

## CSF culture Resistant marker results - Entry
csf_rm_entry <- snip_df %>% filter(redcap_event_name == "Entry (Arm 1: Main Database)") 
csf_rm_entry <- csf_rm_entry %>% dplyr::filter(redcap_repeat_instrument == "CSF culture resistant markers results")
csf_rm_entry <- csf_rm_entry %>% select(where(~ any(!is.na(.))))
write_csv(csf_rm_entry, file = here("raw_data","csf_rm_entry.csv"))


## Deterioration BC and CSF

# Patient progress chart - Details and daily chart

## BC results  , csf culture results, patient progress details are all in enrty data
```
