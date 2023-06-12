# Ch7. Regression models
  
library(fpp3)

# The linear model with time series

## Multiple regression and forecasting

## Example: US consumption expenditure
us_change
us_change %>%
  pivot_longer(c(Consumption, Income), names_to="Series") %>%
  autoplot(value) +
  labs(y="% change")


## Example: US consumption expenditure

us_change %>%
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() + geom_smooth(method = "lm", se = FALSE)


## Example: US consumption expenditure

fit_cons <- us_change %>%
  model(lm = TSLM(Consumption ~ Income))

report(fit_cons)
?report


## Example: US consumption expenditure

us_change %>%
  gather("Measure", "Change", Consumption, Income, Production, Savings, Unemployment) %>%
  ggplot(aes(x = Quarter, y = Change, colour = Measure)) +
  geom_line() +
  facet_grid(vars(Measure), scales = "free_y") +
  labs(y="") +
  guides(colour="none")


### A scatterplot matrix of US consumption expenditure and the four predictors.

us_change %>%
  GGally::ggpairs(columns = 2:6)


## Example: US consumption expenditure

fit_consMR <- us_change %>%
  model(lm = TSLM(Consumption ~ Income + Production + Unemployment + Savings))
report(fit_consMR)


## Example: US consumption expenditure

augment(fit_consMR) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))


## Example: US consumption expenditure

augment(fit_consMR) %>%
  ggplot(aes(x=.fitted, y=Consumption)) +
  geom_point() +
  labs(y="Fitted (predicted values)",
       x="Data (actual values)",
       title ="Percentage change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)


## Evaluating augment(fit_consMR) %>%
  features(.innov, ljung_box, lag = 10, dof = 5)
### Example: US consumption expenditure

fit_consMR %>% gg_tsresiduals()



### Scatterplots of residuals versus each predictor.
us_change %>%
  left_join(residuals(fit_consMR), by = "Quarter") %>%
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") %>%
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

augment(fit_consMR) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")


#Spurious regression
fit <- aus_airpassengers %>%
  filter(Year <= 2011) %>%
  left_join(guinea_rice, by = "Year") %>%
  model(TSLM(Passengers ~ Production))
report(fit)



# Some useful predictors for linear models
## Example: Australian quarterly beer production
aus_production
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
recent_production
recent_production %>%
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))

report(fit_beer)

augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))

augment(fit_beer) %>%
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

#Fourier series
fourier_beer <- recent_production %>% 
  model(TSLM(Beer ~ trend() + fourier(K=2)))
report(fourier_beer)

recent_production %>% 
  model(
    f1 = TSLM(Beer ~ trend() + fourier(K=1)),
    f2 = TSLM(Beer ~ trend() + fourier(K=2)),
    season = TSLM(Beer ~ trend() + season())
  ) %>%
  glance()
#7.5 Selecting predictors
#glance(fit_consMR) %>%
#  select(adj_r_squared, CV, AIC, AICc, BIC)

# Fourier terms for cafe data

aus_cafe <- aus_retail %>% filter(
  Industry == "Cafes, restaurants and takeaway food services",
  year(Month) %in% 2004:2018
) %>% 
  summarise(Turnover = sum(Turnover))
aus_cafe %>% 
  autoplot(Turnover)
aus_cafe %>% 
  autoplot(log(Turnover))

fit <- aus_cafe %>% 
  model(
    K1 = TSLM(log(Turnover) ~ trend() + fourier(K = 1)),
    K2 = TSLM(log(Turnover) ~ trend() + fourier(K = 2)),
    K3 = TSLM(log(Turnover) ~ trend() + fourier(K = 3)),
    K4 = TSLM(log(Turnover) ~ trend() + fourier(K = 4)),
    K5 = TSLM(log(Turnover) ~ trend() + fourier(K = 5)),
    K6 = TSLM(log(Turnover) ~ trend() + fourier(K = 6))
  )

augment(fit) %>%
  filter(.model %in% c("K1","K2","K3")) %>%
  ggplot(aes(x=Month, y=Turnover)) +
  geom_line() +
  geom_line(aes(y=.fitted, col=.model)) +
  facet_grid(.model ~ .)

glance(fit) %>%
  select(.model, sigma2, log_lik, AIC, AICc, BIC)

# US consumption quarterly changes
fit_all <- us_change %>%
  model(
    TSLM(Consumption ~ Income + Production + Unemployment + Savings),
    TSLM(Consumption ~ Production + Unemployment + Savings),
    TSLM(Consumption ~ Income + Unemployment + Savings),
    TSLM(Consumption ~ Income + Production + Savings),
    TSLM(Consumption ~ Income + Production + Unemployment),
    TSLM(Consumption ~ Income + Production),
    TSLM(Consumption ~ Income + Unemployment),
    TSLM(Consumption ~ Income + Savings),
    TSLM(Consumption ~ Production + Unemployment),
    TSLM(Consumption ~ Production + Savings),
    TSLM(Consumption ~ Unemployment + Savings),
    TSLM(Consumption ~ Income),
    TSLM(Consumption ~ Production),
    TSLM(Consumption ~ Unemployment),
    TSLM(Consumption ~ Savings),
    TSLM(Consumption ~ 1),
  )

fit_all %>% 
  glance() %>% 
  select(.model, adj_r_squared, AICc, BIC, CV) %>%
  arrange(CV)

us_change %>% model(    
  TSLM(Consumption ~ Income*Savings + Production + Unemployment),
) %>% report()


# Scenario based forecasting
fit_consBest <- us_change %>%
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )
future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) %>%
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change %>%
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")

#7.6 Forecasting with regression
#Ex-ante versus ex-post forecasts
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))
fc_beer <- forecast(fit_beer)
fc_beer %>%
  autoplot(recent_production) +
  labs(
    title = "Forecasts of beer production using regression",
    y = "megalitres"
  )

# Building a predictive regression model
fit_cons <- us_change %>%
  model(TSLM(Consumption ~ Income))
new_cons <- scenarios(
  "Average increase" = new_data(us_change, 4) %>%
    mutate(Income = mean(us_change$Income)),
  "Extreme increase" = new_data(us_change, 4) %>%
    mutate(Income = 12),
  names_to = "Scenario"
)
fcast <- forecast(fit_cons, new_cons)

us_change %>%
  autoplot(Consumption) +
  autolayer(fcast) +
  labs(title = "US consumption", y = "% change")

# 7.7 Nonlinear regression
# Example: Boston marathon winning times
boston_men <- boston_marathon %>%
  filter(Year >= 1924) %>%
  filter(Event == "Men's open division") %>%
  mutate(Minutes = as.numeric(Time)/60)

boston_men %>% 
  autoplot(Minutes) +
  labs(y="Winning times in minutes")

# Dividd data into train and test sets
boston_men_train <- boston_men %>%
  filter(Year <= 2008)

fit_trends_linear <- boston_men_train %>%
  model(linear = TSLM(Minutes ~ trend()))

gg_tsresiduals(fit_trends_linear)

fit_trends <- boston_men_train %>%
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
  )
fc_trends <- fit_trends %>% forecast(h = 10)

fit_trends %>%
  select(linear) %>%
  report()

fit_trends %>%
  select(piecewise) %>%
  report()

fit_trends %>%
  select(exponential) %>%
  report()


boston_men %>%
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Minutes",
       title = "Boston marathon winning times")

#glance(fit_trends) %>%
#  select(.model, sigma2, log_lik, AIC, AICc, BIC)

fc_trends %>%
 autoplot(
   boston_marathon %>%
     filter(Year >= 1924) %>%
     filter(Event == "Men's open division") %>%
     mutate(Minutes = as.numeric(Time)/60),
    level = NULL
) +
    labs(y = "Minutes",
         title = "Boston marathon winning times"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(fc_trends, boston_men)

#cross-validation
boston_men_tr <- boston_men %>%
  stretch_tsibble(.init = 3, .step = 1) 

# TSCV accuracy
boston_men_tr %>%
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
  ) %>%
  forecast(h = 1) %>%
  accuracy(boston_men)


