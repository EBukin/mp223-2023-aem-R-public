library(tidyverse)
library(haven)
library(here)

# Plan type 1 = "Free plan"
# Plan type 2 = "Deductible plan"
# Plan type 3 = "Coinsurance plan"
# Plan type 4 = "Catastrophic plan" or "No Insurance"

pre_dta <- 
  here("..", "data-raw", "rand_initial_sample_2.dta") %>% read_dta() %>% 
  # glimpse() %>% 
  select(
    id = fam_identifier,
    plantype, 
    female, 
    non_white = blackhisp, 
    age, 
    education_years = educper, 
    income_family = income1cpi, 
    hospitalization = hosp, 
    general_health_index = ghindx,
    cholesterol = cholest, 
    blood_pressure = systol, 
    mental_health_index = mhi
  ) %>% 
  filter(id != "") %>% 
  filter(plantype %in% c(1, 4)) %>% 
  mutate(plan = ifelse(plantype == 1, "Free", "Catastrophic"))

pre_dta %>% write_excel_csv(here("rand-hie-pre-treatment.csv"))
