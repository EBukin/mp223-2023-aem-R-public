

library(tidyverse)

data("RiceFarms", package = "splm")
farm_dta <-
  RiceFarms %>% as_tibble() %>%
    # filter(size < 0.05)
  mutate(size = ifelse(size < 0.05, 0, size)) %>% 
  select(
    output = goutput, 
    status, 
    land = size, 
    labor = totlabor,
    labor_family = famlabor, 
    labor_hired = hiredlabor,
    seed, 
    urea, 
    phosphate, 
    pesticide,
    region)
  
farm_dta %>% write_csv("rice.csv")
