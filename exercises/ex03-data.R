
library(wooldridge)
library(tidyverse)

dta_wage <- 
  wage2 %>%
  as_tibble() %>%
  mutate(intercept = 1,
         white = as.integer(!black)) %>% 
  select(wage, intercept, educ, exper, black, white)


set.seed(121)
dta_wage %>% 
  sample_n(20) %>% 
  write_csv(here::here("wage-small.csv"))


set.seed(121)
wage1 %>%
  as_tibble() %>%
  mutate(white = as.integer(!nonwhite)) %>% 
  # mutate(wage_h = wage / hours / 4) %>% 
  select(wage, educ, exper, black = nonwhite, white, female ) %>% 
  # select(-intercept, -white) %>% 
  # sample_n(12) %>% 
  write_csv(here::here(#"exercises", "ex03-regression-part1", 
    "wage-full.csv"))


read_csv("../data-raw/wine.csv") %>%
  as_tibble() %>%
  mutate(Price = exp(Price)) %>% 
  write_csv(here::here(#"exercises", "ex03-regression-part1", 
    "wine-clean.csv"))


#

alr4::MinnLand %>% as_tibble() %>% 
  write_excel_csv("land-prices.csv")
