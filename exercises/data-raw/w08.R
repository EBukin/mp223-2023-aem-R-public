library(palmerpenguins)
library(tidyverse)

penguins %>% write_csv("penguins.csv")

# Rice farms
data("RiceFarms", package = "splm")
farm_dta <- 
  RiceFarms %>% as_tibble() %>%
  # mutate(id = as.character(id)) %>% 
  select(id, time, output = goutput, land = size, labor = totlabor,
         hiredlabor, famlabor, seed, urea, pest = pesticide,
         varieties, status, bimas, region, price) %>% 
  mutate(time = time + 1998)
farm_dta %>% write_csv("farm_panel.csv")
# Union wage

library(plm)
library(Ecdat)
data(Wages)


wage_dta <-
  pdata.frame(Wages, 595) %>%
  as_tibble() %>%
  mutate(
    across(
      # c(bluecol, south , smsa , married, union, black),
      c(union),
      ~ ifelse(. == "yes", 1, 0)
    ),
    female = ifelse(sex == "male", 1, 0),
    wage = exp(lwage),
    time = 81 + as.integer(time)
  ) %>% 
  rename(educ = ed, exper = exp, hours = wks,
         year = time) %>% 
  select(id, year, everything()) %>% 
  select(-sex)


wage_dta %>% write_csv("wage_unon_panel.csv")


library(PoEdata)
data("nls_panel", package="PoEdata")

nls_panel %>% 
  as_tibble() %>% 
  mutate(wage = exp(lwage) %>% round(2),
         exper = round(exper, 2),
         tenure = round(tenure, 2)) %>% 
  select(id, year, wage, hours, age, educ, exper, tenure, union, collgrad, married = msp, 
         never_married = nev_mar, black, south, cental_city = c_city) %>% 
  as_tibble()%>%
  write_csv("wage_unon_panel_v2.csv")

# Rice data

data(Rice)

plm
