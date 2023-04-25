

# Wine example

library(tidyverse)
library(GGally)
library(modelsummary)
library(correlation)
library(performance)
wine_dta <- read_csv("wine-clean.csv")

datasummary_skim(wine_dta)
correlation(wine_dta) %>% summary()
wine_dta %>% select(-Year, -FrancePop) %>% ggpairs()

model1 <- lm(Price ~ Age + AGST + WinterRain + HarvestRain, data = wine_dta)
summary(model1)

modelsummary(
  list(`Price` = model1), 
  estimate = "{estimate}{stars} ({std.error})", 
  statistic = NULL, 
  output = "markdown")

check_model(model1, check = "linearity")

model2 <- lm(log(Price) ~ Age + AGST + WinterRain + HarvestRain, data = wine_dta)
summary(model2)
check_model(model2, check = "linearity")

model3 <- lm(log(Price) ~ Age, data = wine_dta)
modelsummary(
  list(`Price` = model1,
       `log(Price)` = model2,
       `log(Price)` = model3), 
  estimate = "{estimate}{stars} ({std.error})", 
  statistic = NULL, 
  output = "markdown")

# Regression manually

library(tidyverse)
wage_small <- read_csv("wage-small.csv")
glimpse(wage_small)
y <- wage_small[,1] %>% as.matrix()
y
x <- wage_small[,c(2,3)] %>% as.matrix()
x
betas <- solve(t(x) %*% x) %*% t(x) %*% y
betas
fit1 <- lm(wage ~ educ, data = wage_small)
fit1
x %*% betas
fitted(fit1)
y - x %*% betas
residuals(fit1)
epsilon <- y - x %*% betas

sigma <- t(epsilon) %*% epsilon / (nrow(epsilon) - 2)
sigma <- as.numeric(sigma)
sigma *  solve(t(x) %*% x)

sqrt(diag(sigma *  solve(t(x) %*% x)))
vcov(fit1)
sqrt(diag(vcov(fit1)))
summary(fit1)
wage_small_extended <- 
  wage_small %>% 
  mutate(
    fitted = fitted(fit1),
    error_terms = wage - fitted)
glimpse(wage_small_extended)


# Wage regressin


library(tidyverse)
wage_dta <- read_csv("wage-full.csv")
glimpse(wage_dta)

wage_dta <-
  wage_dta %>%
  mutate(caucasian = ifelse(black, "no", "yes"),
         caucasian = as.factor(caucasian))
glimpse(wage_dta)

library(modelsummary)
datasummary_skim(wage_dta)

library(GGally)
ggpairs(wage_dta)

ggpairs(wage_dta, aes(colour = caucasian))

fit1 <- lm(wage ~ educ + exper + black + female, data = wage_dta)
fit1

summary(fit1)

library(parameters)
parameters(fit1)

library(performance)
performance(fit1)

fitted_vector <- fitted(fit1)
fitted_vector[1:20]

resid_vector <- resid(fit1)
resid_vector[1:20]

plot(x = fitted_vector, y = resid_vector)

check_model(fit1, check = "linearity")

pred1 <-
  tibble(educ = 0,
         exper = 0,
         black = 0)
predict(fit1, pred1)

pred2 <-
  tibble(educ = c(0, 0),
         exper = c(0, 0),
         black = c(0, 1)
  )
predict(fit1, pred2)

pred3 <-
  tibble(educ = c(0, 0, 10),
         exper = c(0, 0, 25),
         black = c(0, 1, 1)
  )
pred3 %>% 
  mutate(predicted = predict(fit1, pred3))

library(ggeffects)

ggpredict(fit1, term = "educ")
ggpredict(fit1, term = "educ") %>% plot()

ggpredict(fit1, term = c("educ", "female"))
ggpredict(fit1, term = c("educ", "female")) %>% plot()


# Hedonic land prices

## 1. Load the data
dta <- 
  alr4::MinnLand %>% 
  as_tibble() 

glimpse(dta)

## 2. Draw boxplots of `acrePrice` versus `year`
dta %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() + 
  aes(x = year,  y = acrePrice) + 
  geom_boxplot() + 
  scale_y_log10()

## 3. Convert monetary values from current to constant prices
defl_dta <-
  tibble(
    year = 2002:2011,
    defl = c(77.47, 78.91, 81.03, 83.56, 86.09, 
             88.4, 90.12, 90.8, 91.86, 93.78)
  )
glimpse(defl_dta)

dta_const <- 
  dta %>% 
  left_join(defl_dta, by = "year") %>% 
  mutate(acrePrice = acrePrice / (defl / 100),
         year = as.factor(year)) %>% 
  select(-defl)
glimpse(dta_const)

dta_const %>% 
  ggplot() + 
  aes(x = year,  y = acrePrice) + 
  geom_boxplot() + 
  scale_y_log10() + 
  labs(y = "Price per acre in constant 2010 USD")


## 4. Produce summary statistics
dta_const %>% datasummary_skim(output = "markdown")


## 5. Produce a correlation matrix
library(correlation)
dta_const %>% 
  correlation() %>% 
  summary()

## 6. Fit the basic regression and summarize the results
fit1 <- lm(
  log(acrePrice) ~ crpPct + acres + region + 
    year + tillable + productivity, 
  data = dta_const
)
summary(fit1)
parameters(fit1)
performance(fit1)


## 7. Check linearity visually
check_model(fit1, check = "linearity")

## 11. Check multicollinearity
vif(fit1)

## 12. Check homoscedasticity visually
check_model(fit1, check = c("linearity", "homogeneity"))

## 13. Check homoscedasticity using statistical tests
library(lmtest)
bptest(fit1)

## 14. Correct standard errors
library(lmtest)
library(sandwich)
parameters(fit1, vcov = "HC3")