library(tidyverse)

dta <- wooldridge::wage2 %>% as_tibble() 
# %>% 
#   select(wage, educ, exper, KWW,  IQ, tenure, age, married, black, sibs, brthord, meduc, feduc)


dta %>% write_csv("wage.csv")


# Wage 2.
install.packages("Ecdat")
library(Ecdat)
data(Wages)


library(plm)
wage_dta <-
  pdata.frame(Wages, 595) %>%
  as_tibble() %>%
  mutate(
    across(
      c(bluecol, south , smsa , married, union, black),
      ~ ifelse(. == "yes", 1, 0)
    ),
    female = ifelse(sex == "male", 1, 0),
    wage = exp(lwage)
  )

# union dta
sht <- lm(
  log(wage) ~ exp + wks + bluecol + ind + south + smsa + married + female + union,
  data = wage_dta)

lng <- lm(
  log(wage) ~ exp + wks + bluecol + ind + south + smsa + married + female + union + ed,
  data = wage_dta)

aux = lm(
  ed ~ exp + wks + bluecol + ind + south + smsa + married + female + union,
  data = wage_dta)

coef(sht)[["union"]] - coef(lng)[["union"]]
coef(lng)[["ed"]] *  coef(aux)[["union"]]

# library(faux)
wage_dta_ind <-
  pdata.frame(Wages, 595) %>%
  as_tibble() %>%
  # filter(time == 1)
  group_by(id) %>%
  summarise(
    wage = mean(exp(lwage)),
    across(
      c(exp, wks, ed),
      ~ round(mean(.), 0)
    ),
    across(
      c(bluecol, south , smsa , married, union, black, sex, ind),
      ~ unique(.)[[1]]
    ))

wage_dta_ind %>% 
  write_csv("wage_union.csv")


# Union dta
sht1 <- lm(
  log(wage) ~ exp + wks + bluecol + ind + south + smsa + married + sex + union,
  data = wage_dta_ind)

lng1 <- lm(
  log(wage) ~ exp + wks + bluecol + ind + south + smsa + married + sex + union + ed,
  data = wage_dta_ind)

aux1 = lm(
  ed ~ exp + wks + bluecol + ind + south + smsa + married + sex + union,
  data = wage_dta_ind)

coef(sht1)[["unionyes"]] - coef(lng1)[["unionyes"]]
coef(lng1)[["ed"]] *  coef(aux1)[["unionyes"]]

# Panel
sht2 <- plm(
  log(wage) ~ exp + wks + bluecol + ind + south + smsa + married + female + union,
  data = wage_dta,
  model = "within",
  effect = "individual",
  index = c("id", "time")
)


sht3 <- plm(
  log(wage) ~ exp + wks + bluecol + ind + south + smsa + married + female + union * ind,
  data = wage_dta,
  model = "within",
  effect = "individual",
  index = c("id", "time")
)

coef(sht2)[["union"]]

# feffs <- fixef(fit1)

modelsummary::modelsummary(
  list(
    sht = sht,
    sht1 = sht1,
    lng = lng,
    lng1 = lng1,
    plm = sht2,
    plm2 = sht3
  )
)
# 
# 
# 
# 
# Wag2 %>% 
#   mutate(iq = rnorm_pre(Wag2$fef, mu = 65, sd = 5, r = 0.8))
# 
# 
# # -
# lm(
#   log(wage   ) ~ ed + exp + I(exp ^2) +  union,
#   data = Wag2 %>% mutate(wage = wage / wks)
# )
# 
# 
# lm(
#   log(wage   ) ~  exp +  I(exp ^2) + union,
#   data = Wag2 %>% mutate(wage = wage / wks)
# )
# 
# 
# 
# Wag2$fef %>% hist()
# 
# fit_sht <- 
#   lm(
#     log(wage   ) ~ ed + exp + exp ^2 + wks + bluecol + married + sex + union + black,
#     data = Wag2
#   )
# 
# lm(
#   log(wage   ) ~  exp + exp ^2 + wks + bluecol + married + sex + union + black,
#   data = Wag2
# )
# 
# set.seed(123)
# Wag3 <- Wag2 %>% mutate(IQ = 2 * feffs - 2 * ed + runif(nrow(.), -2, 2))
# fit_lg <- 
#   lm(
#     log(wage   ) ~ ed + exp + exp ^2 + wks + bluecol + married + sex + union + black + IQ,
#     data = Wag3
#   )
# 
# modelsummary::datasummary_correlation(Wag3)
# fit_lg

