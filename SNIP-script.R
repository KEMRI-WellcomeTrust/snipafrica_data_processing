library(ggplot2)
library(dplyr)
library(tidyr)
library(flextable)

setwd("C:/Users/jkigo/OneDrive - Kemri Wellcome Trust/2025/My PhD/Snip dashboard/data")

data <- read.csv("./SNIPAfricaSurveillan_DATA_2025-04-12_1105.csv")

##Enter entry filters by disc with Eunice
exit<- data
#blood_culture_crf<- data %>% filter(redcap_event_name==)

##=======================================================
#Summary table 1
# Define today's date
today <- Sys.Date()

# Filter for last 7 days
last7 <- data %>% filter(date_adm >= today - 6 & date_adm <= today)

# Create the summary table
summary_table <- tibble(
  Metric = c(
    "Eligible cases",
    "Deterioration during initial episode of sepsis",
    "New episode of sepsis after antibiotic completion"
  ),
  `Last 7 Days` = c(
    sum(last7$ip_neonate_unit_adm == 1& last7$age_elig_adm == 1, na.rm = TRUE),
    sum(last7$event_type_det == 1, na.rm = TRUE),
    sum(last7$event_type_det == 2, na.rm = TRUE)
  ),
  Cumulative = c(
    sum(data$ip_neonate_unit_adm == 1& data$age_elig_adm == 1, na.rm = TRUE),
    sum(data$event_type_det == 1, na.rm = TRUE),
    sum(data$event_type_det == 2, na.rm = TRUE)
  )
) %>% flextable()
##===================================================

##Exit CRF

# Recode outcome_sc and final diagnosis to meaningful labels
exit <- exit %>%
  mutate(outcome_label = case_when(
    outcome_sc == 1 ~ "Discharged",
    outcome_sc == 2 ~ "Transferred",
    outcome_sc == 3 ~ "Died",
    outcome_sc == 4 ~ "Left against medical advice",
    outcome_sc == 5 ~ "Absconded",
    TRUE ~ NA_character_
  ))

# Bar chart
exit_outcome<- exit %>%
  filter(!is.na(outcome_label)) %>%
  ggplot(aes(x = outcome_label)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Patient Outcomes",
       x = "Outcome",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##Date inconsistencies
date_issues <- data %>%
  mutate(
    admission_issue = if_else(date_adm < dob_adm, "Date of admission is before date of birth", NA_character_),
    sample_issue = if_else(date_collect_bc < dob_adm, "Date of sample collection is before date of birth", NA_character_)
  ) %>%
  select(study_number_adm, admission_issue, sample_issue) %>%
  pivot_longer(cols = c(admission_issue, sample_issue), names_to = "type", values_to = "issue") %>%
  filter(!is.na(issue)) %>%
  select(study_number_adm, issue)



#CRF 5
#Timepoint
time_point_bc
time_point_bc

### date consistency


