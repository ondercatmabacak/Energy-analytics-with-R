# Chapter 5 - The forecaster's toolbox
library(fpp3)

?global_economy
global_economy

# Step 1. Data preparation (tidy)
gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)

# Step 2. Plot the data (visualize)
gdppc %>%
  filter(Country == "Sweden") %>%
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

# Step 3. Specify a model and train the model
fit <- gdppc %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

gdppc %>%
  distinct(Country)

#gdppc_SW <- gdppc %>%
#  filter(Country == "Sweden")

#gdppc_SW

#fit2 <- gdppc_SW %>%
#  model(trend_model = TSLM(GDP_per_capita ~ trend()))

#fit2

#Step 4. Evaluate the model

# Step 5. Produce forecasts
fit %>% forecast(h = "3 years")

# 5.2. Some simple forecasting methods
# Example. Quarterly production of selected commodities in Australia.
aus_production
?aus_production

bricks <- aus_production %>%
  filter_index("1970 Q1" ~ "2004 Q4")

bricks

bricks %>% model(MEAN(Bricks))
bricks %>% model(MEAN(Bricks)) %>% augment()

bricks %>% model(SNAIVE(Bricks ~ lag("year")))
bricks %>% model(SNAIVE(Bricks ~ lag("year"))) %>% augment()

bricks %>% model(RW(Bricks ~ drift()))
bricks %>% model(RW(Bricks ~ drift())) %>% augment()

#Example: Australian quarterly beer production
# Set training data from 1992 to 2006
train <- aus_production %>%
  filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    `Na?ve` = NAIVE(Beer),
    `Seasonal na?ve` = SNAIVE(Beer)
  )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit %>% forecast(h = 14)
# Plot forecasts against actual values
beer_fc %>%
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

#Example: Google's daily closing stock price
gafa_stock
?gafa_stock

# Re-index based on trading days
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)
# Fit the models
google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Na?ve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )
# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit %>%
  forecast(new_data = google_jan_2016)
# Plot the forecasts
google_fc %>%
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))

# 5.3 Fitted values and residuals
augment(beer_fit)

autoplot(google_2015, Close) +
  labs(y = "$US",
       title = "Google daily closing stock prices in 2015")

aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()
autoplot(aug, .innov) +
  labs(y = "$US",
       title = "Residuals from the na?ve method")

aug %>%
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")

aug %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "Residuals from the na?ve method")

google_2015 %>%
  model(NAIVE(Close)) %>%
  gg_tsresiduals()

# 5.4. Residual diagnostics
autoplot(google_2015, Close) +
  labs(y = "$US",
       title = "Google daily closing stock prices in 2015")

aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()
autoplot(aug, .innov) +
  labs(y = "$US",
       title = "Residuals from the na?ve method")

aug %>%
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")

aug %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "Residuals from the na?ve method")

google_2015 %>%
  model(NAIVE(Close)) %>%
  gg_tsresiduals()

#Portmanteau tests for autocorrelation
aug %>% features(.innov, box_pierce, lag = 10, dof = 0)

aug %>% features(.innov, ljung_box, lag = 10, dof = 0)

fit <- google_2015 %>% model(RW(Close ~ drift()))
tidy(fit)
?tidy

augment(fit) %>% features(.innov, ljung_box, lag=10, dof=1)

# 5.5 Distributional forecasts and prediction intervals
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  hilo()


google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  autoplot(google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

#Prediction intervals from bootstrapped residuals
fit <- google_2015 %>%
  model(NAIVE(Close))
sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)
sim

google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = sim) +
  labs(title="Google daily closing stock price", y="$US" ) +
  guides(colour = "none")

fc <- fit %>% forecast(h = 30, bootstrap = TRUE)
#fc
?forecast

autoplot(fc, google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10, bootstrap = TRUE, times = 1000) %>%
  hilo()


#5.6 Forecasting using transformations

?is.na 
# The generic function is.na indicates which elements are missing.
?lst

?prices
prices

prices %>%
  filter(!is.na(eggs)) %>%
  model(RW(log(eggs) ~ drift())) %>%
  forecast(h = 50) %>%
  autoplot(prices %>% filter(!is.na(eggs)),
           level = 80, point_forecast = lst(mean, median)
  ) +
  labs(title = "Annual egg prices",
       y = "$US (in cents adjusted for inflation) ")

#5.7 Forecasting with decomposition
?us_employment
us_employment

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade")
us_retail_employment

dcmp <- us_retail_employment %>%
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) %>%
  components() %>%
  select(-.model)

dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(y = "Number of people",
       title = "US retail employment")

?decomposition_model

fit_dcmp <- us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))


fit_dcmp %>%
  forecast() %>%
  autoplot(us_retail_employment)+
  labs(y = "Number of people",
       title = "US retail employment")

fit_dcmp %>% gg_tsresiduals()

#5.8 Evaluating point forecast accuracy
aus_production %>% filter(year(Quarter) >= 1995)

# Equivalently, the following syntax can be used:
aus_production %>% filter_index("1995 Q1" ~ .)

?filter
?filter_index

# Slice()
?slice

# Extract the last 20 observations (5 years)
aus_production %>%
  slice(n()-19:0)

?aus_retail
aus_retail
# Extract the first year of data from each time series in the data
aus_retail %>%
  group_by(State, Industry) %>%
  slice(1:12)

# Examples
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
beer_train <- recent_production %>%
  filter(year(Quarter) <= 2007)

beer_fit <- beer_train %>%
  model(
    Mean = MEAN(Beer),
    `Na?ve` = NAIVE(Beer),
    `Seasonal na?ve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit %>%
  forecast(h = 10)

beer_fc %>%
  autoplot(
    aus_production %>% filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(beer_fc, recent_production)

# Example:  Google stock price
# Re-index based on trading days
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)

google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Na?ve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))

google_fc <- google_fit %>%
  forecast(google_jan_2016)

accuracy(google_fc, google_stock)

# Evaluating distributional forecast accuracy
## Quantile scores
### Example: Google stock price example 
google_fc %>%
  filter(.model == "Na?ve") %>%
  autoplot(bind_rows(google_2015, google_jan_2016), level=80)+
  labs(y = "$US",
       title = "Google closing stock prices")

google_fc %>%
  filter(.model == "Na?ve", Date == "2016-01-04") %>%
  accuracy(google_stock, list(qs=quantile_score), probs=0.10)

## Winkler Score
google_fc %>%
  filter(.model == "Na?ve", Date == "2016-01-04") %>%
  accuracy(google_stock,
           list(winkler = winkler_score), level = 80)

## Continuous Ranked Probability Score
google_fc %>%
  accuracy(google_stock, list(crps = CRPS))

## Scale-free comparisons using skill scores
google_fc %>%
  accuracy(google_stock, list(skill = skill_score(CRPS)))



# Time series cross-valuation
# Time series cross-validation accuracy
google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1) %>%
  relocate(Date, Symbol, .id)

?relocate

google_2015_tr

# TSCV accuracy
google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 1) %>%
  accuracy(google_2015)

#filter(!is.na(Close))

# Training set accuracy
google_2015 %>%
  model(RW(Close ~ drift())) %>%
  accuracy()


#Example: Forecast horizon accuracy with cross-validation
google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 8) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup()
fc %>%
  accuracy(google_2015, by = c("h", ".model")) %>%
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()
