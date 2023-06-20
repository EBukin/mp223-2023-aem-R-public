library(tidyverse)
library(haven)
library(here)

# Plan type 1 = "Free plan"
# Plan type 2 = "Deductible plan"
# Plan type 3 = "Coinsurance plan"
# Plan type 4 = "Catastrophic plan" or "No Insurance"

pre_dta <- 
  here("..", "data-raw", "person_years.dta") %>% read_dta() 

ann_dta <-
  here("..", "data-raw", "annual_spend.dta") %>% 
  read_dta() %>% 
  left_join(pre_dta %>% select(person, year, plan, indv_start_year, ftf), by = c("person", "year")) %>% 
  semi_join(pre_dta, by = c("person", "year")) %>% 
  mutate(
    expyear = year + indv_start_year - 1,
    across(
      c(inpdol, outsum),
      ~ case_when(
        expyear == 1973 ~ . * 3.07,
        expyear == 1974 ~ . * 2.76,
        expyear == 1975 ~ . * 2.53,
        expyear	== 1976 ~ . * 2.39,
        expyear	== 1977 ~ . * 2.24,
        expyear	== 1978 ~ . * 2.09,
        expyear	== 1979 ~ . * 1.88,
        expyear	== 1980 ~ . * 1.65,
        expyear	== 1981 ~ . * 1.5,
        expyear	== 1982 ~ . * 1.41,
        expyear	== 1983 ~ . * 1.37,
        expyear	== 1984 ~ . * 1.31,
        expyear	== 1985 ~ . * 1.27,
        TRUE ~ NA_real_
      )
      ),
    total_exp = outsum + inpdol
  ) %>% 
  mutate(
    plantype = 
      case_when(plan == 24 ~ "Catastrophic", 
                plan >= 2 & plan <= 4 | plan >= 6 & plan <= 8   ~ "Free",
                TRUE ~ NA_character_)
  ) %>% 
  select(
    person, 
    plan = plantype,
    year = expyear, 
    total_exp,
    outpatient_exp = outsum,
    inpatient_exp = inpdol,
    admissions = totadm,
    face_to_face = ftf
  ) %>% 
  bind_rows(
    filter(., plan != "Catastrophic" | is.na(plan)) %>% 
      mutate(plan = "Any insurance")
  ) %>% 
  filter(!is.na(plan)) %>% 
  mutate(plan_type_2 = case_when(
    plan == "Catastrophic" ~ 0,
    plan == "Any insurance" ~ 1,
    TRUE ~ 2
  ))


datasummary_balance(. ~ plan, data = ann_dta)

# ann_dta %>% write_excel_csv(here("rand-hie-post-treatment.csv"))


# Pre-treatment data
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
  mutate(
    plan = ifelse(plantype == 1, "Free", NA_character_),
    plan = ifelse(plantype == 4, "Catastrophic", plan)
    ) %>% 
    bind_rows(
      filter(., plan != "Catastrophic" | is.na(plan)) %>% 
        mutate(plan = "Any insurance")
    ) %>% 
  # mutate(plan )
  filter(!is.na(plan)) %>% 
  mutate(plan_type_2 = case_when(
    plan == "Catastrophic" ~ 0,
    plan == "Any insurance" ~ 1,
    TRUE ~ 2
    ))

pre_dta %>% write_excel_csv(here("rand-hie-pre-treatment.csv"))


# pre_dta %>% write_excel_csv(here("rand-hie-pre-treatment.csv"))
