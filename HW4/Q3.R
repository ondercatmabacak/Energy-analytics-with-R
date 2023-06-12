library(fpp3)
library(lattice)
library(ggplot2)
library(tidyr)
df=global_economy %>%
  filter(Country == "Afghanistan")

##### PART A #####
df
xyplot(GDP/1.0e9 + Growth + CPI + Imports + Exports + Population/1.0e6~ Year, 
       data = df, type = "l", auto.key = TRUE)

##### PART B #####

fit_afg <- df %>%
  model(
    linear = TSLM(Population ~ trend()),
    piecewise = TSLM(Population ~ trend(knots=c(1980, 1989)))
  )

fit_afg %>%
  select(linear) %>%
  report()

fit_afg %>%
  select(piecewise) %>%
  report()

df %>%
  autoplot(Population) +
  geom_line(data = fitted(fit_afg),
            aes(y = .fitted, colour = .model)) +
  labs(y = "Population",
       title = "Total Population of Afghanistan between 1960-2017")

##### PART C #####
fc_afg <- fit_afg %>% forecast(h = 5)
df %>%
  autoplot(Population) +
  geom_line(data = fitted(fit_afg),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_afg, alpha = 0.5, level = 95) +
  labs(y = "Population",
       title = "Total Population of Afghanistan between 1960-2017")
