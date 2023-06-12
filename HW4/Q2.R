library(fpp3)
us_gas = us_gasoline %>%
  filter(year(Week)<=2004)
us_gas_05 = us_gasoline %>%
  filter(year(Week)<=2005)
us_gas
##### PART A #####

fit_usgas = us_gas %>%
  model(K1 = TSLM(Barrels ~ trend() + fourier(K = 1)),
        K2 = TSLM(Barrels ~ trend() + fourier(K = 2)),
        K3 = TSLM(Barrels ~ trend() + fourier(K = 3)),
        K4 = TSLM(Barrels ~ trend() + fourier(K = 4)),
        K5 = TSLM(Barrels ~ trend() + fourier(K = 5)),
        K6 = TSLM(Barrels ~ trend() + fourier(K = 6))
  )
augment(fit_usgas) %>%
  ggplot(aes(x=Week, y=Barrels)) +
  geom_line() +
  geom_line(aes(y=.fitted, col=.model)) +
  facet_grid(.model ~ .)+
  labs(y= "Barrels (Million per day)")

##### PART B #####
glance(fit) %>%
  select(.model, sigma2, log_lik, AIC, AICc, BIC, CV)

##### PART C #####
fit_usgas_K1 = us_gas %>%
  model(K1 = TSLM(Barrels ~ trend() + fourier(K = 1)))
fit_usgas_K1 %>% gg_tsresiduals()

fit_usgas_K2 = us_gas %>%
  model(K2 = TSLM(Barrels ~ trend() + fourier(K = 2)))
fit_usgas_K2 %>% gg_tsresiduals()

fit_usgas_K3 = us_gas %>%
  model(K3 = TSLM(Barrels ~ trend() + fourier(K = 3)))
fit_usgas_K3 %>% gg_tsresiduals()

fit_usgas_K4 = us_gas %>%
  model(K4 = TSLM(Barrels ~ trend() + fourier(K = 4)))
fit_usgas_K4 %>% gg_tsresiduals()

fit_usgas_K5 = us_gas %>%
  model(K5 = TSLM(Barrels ~ trend() + fourier(K = 5)))
fit_usgas_K5 %>% gg_tsresiduals()

fit_usgas_K6 = us_gas %>%
  model(K6 = TSLM(Barrels ~ trend() + fourier(K = 6)))
fit_usgas_K6 %>% gg_tsresiduals()

fit_usgas %>% augment()%>% features(.innov, ljung_box, lag = 10, dof = 0)

##### PART D #####
# Regression forecast

fit_barrels <- us_gas %>%
  model(TSLM(Barrels ~ trend() + season()))
fc_barrels <- forecast(fit_barrels,h=52)
fc_barrels %>%
  autoplot(us_gasoline) +
  labs(
    title = "Forecasts of gasoline product supplied using regression",
    y = "Barrels (Million per day)"
  )

#Scenario-based forecast
barrel_05=us_gasoline%>%
  filter(year(Week)==2005)
b_05 = barrel_05%>%
  mutate(Barrels=barrel_05$Barrels-0.1)

barrel_05
b_05

fit_barrels <- us_gas %>%
  model(
    lm = TSLM(Barrels ~ trend()+season())
  )
future_scenarios <- scenarios(
  Mean = new_data(us_gas, 52) %>%
    mutate(Barrels = mean(us_gas$Barrels)),
  Decrease = new_data(us_gas, 52) %>%
    mutate(Barrels= b_05),
  names_to = "Scenario")

fc <- forecast(fit_barrels, new_data = future_scenarios)

us_gasoline %>%
  autoplot(Barrels) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")
  us_gas

  fit_us_gas <- us_gas %>%
    model(
      linear = TSLM(Barrels ~ trend()),
      exponential = TSLM(log(Barrels) ~ trend()),
      piecewise = TSLM(Barrels ~ trend(knots=c()))
    )
  
  fit_us_gas %>%
    select(linear) %>%
    report()
  
  fit_us_gas %>%
    select(exponential) %>%
    report()
  
  fit_us_gas %>%
    select(piecewise) %>%
    report()
  fc_us_gas <- fit_us_gas %>% forecast(h = 52)

  us_gas_05 %>%
    autoplot(Barrels) +
    geom_line(data = fitted(fit_us_gas),
              aes(y = .fitted, colour = .model)) +
    autolayer(fc_us_gas, alpha = 0.5, level = 95) +
    labs(y = "Barrels (Millions per day)",
         title = "Forecasts of gasoline product supplied using regression")
  