library(fpp3)
library(readr)
library(tsibble)
library(tsibbledata)
library(dplyr)

##### PREPERATION #####

nuc = readr::read_csv('/home/onder/R/FINAL_PROJECT/data/nuc_twh_1965_2020.csv', show_col_types = FALSE)

nuc_tsbl = nuc %>%
  mutate(Year=1965:2020)%>%
  as_tsibble(key= Entity, index=Year)
nuc_tsbl%>%
  tail(20)

##### FIRST PLOT #####

nuc_tsbl %>%
  autoplot(`Terawatt-hours`) + labs(title = "Global nuclear energy production", y= "Terawatts-hours")

nuc_tsbl %>%
  autoplot(log(`Terawatt-hours`)) + labs(title = "Logarithmic plot for Global nuclear energy production", y= "log(Terawatts-hours)")

nuc_tsbl %>%
  autoplot(sqrt(`Terawatt-hours`)) + labs(title = "Square root plot for Global nuclear energy production", y= "sqrt(Terawatts-hours)")

##### AUTO-CORRELATION FUNCTIONS #####
nuc_tsbl%>%
  ACF(`Terawatt-hours`, lag_max=55)%>%
  autoplot() + labs(title = "Autocorrelation Function")

nuc_tsbl%>%
  ACF(log(`Terawatt-hours`), lag_max=55)%>%
  autoplot() + labs(title = "Logaritmic  Autocorrelation Function")

nuc_tsbl%>%
  ACF(sqrt(`Terawatt-hours`), lag_max=55)%>%
  autoplot() + labs(title = "Square Root Autocorrelation Function")

##### Box-Cox Transformation ######

lambda = nuc_tsbl %>%
  features(`Terawatt-hours`, features = guerrero) %>%
  pull(lambda_guerrero)

nuc_tsbl %>%
  autoplot(box_cox(`Terawatt-hours`, lambda)) + labs(title = latex2exp::TeX(paste0(
    "Transformed gas production with $\\lambda$ = ", round(lambda,2))), y= "Terawatts-hours")

##### DECOMPOSITION MODELS ######

nuc_stl = nuc_tsbl %>%
  model( stl = STL(box_cox(`Terawatt-hours`, lambda))) 

nuc_stl %>%
  components() %>%
  autoplot()

##### CHOOSING MODEL and FORECAST ACCURACY ######

nuc_train =  nuc_tsbl %>%
  filter(Year < 2012)

nuc_train %>%
  tail()

nuc_fit <- nuc_train %>%
  model(
    Mean = MEAN(box_cox(`Terawatt-hours`, lambda)),
    `Naïve` = NAIVE(box_cox(`Terawatt-hours`, lambda)),
    Drift = RW(box_cox(`Terawatt-hours`, lambda) ~ drift()))

nuc_fc <- nuc_fit %>%
  forecast(h = 9)

nuc_fc %>%
  autoplot(nuc_tsbl) +
  labs(title="Global nuclear energy production", y="Terawatts-hours" )

accuracy(nuc_fc, nuc_tsbl)

#### ETS MODEL #####

ETS_A = nuc_train%>%
  model(
    AAN = ETS(box_cox(`Terawatt-hours`, lambda) ~ error("A") + trend("A") + season("N")),
    AAdN = ETS(box_cox(`Terawatt-hours`, lambda) ~ error("A") + trend("Ad") + season("N")),
    AMN = ETS(box_cox(`Terawatt-hours`, lambda) ~ error("A") + trend("M") + season("N")))
ETS_M = nuc_train%>%
  model(
    MAN = ETS(box_cox(`Terawatt-hours`, lambda) ~ error("M") + trend("A") + season("N")),
    MAdN = ETS(box_cox(`Terawatt-hours`, lambda) ~ error("M") + trend("Ad") + season("N")),
    MMN = ETS(box_cox(`Terawatt-hours`, lambda) ~ error("M") + trend("M") + season("N")))
ETS_auto = nuc_train%>%
  model(
    autoAN = ETS(box_cox(`Terawatt-hours`, lambda) ~ trend("A") + season("N")),
    autoAdN = ETS(box_cox(`Terawatt-hours`, lambda) ~ trend("Ad") + season("N")),
    autoMN = ETS(box_cox(`Terawatt-hours`, lambda) ~ trend("M") + season("N")))

tidy(ETS_A,ETS_M,ETS_auto)

glance(ETS_A)
glance(ETS_M)
glance(ETS_auto)

accuracy(ETS_A)
accuracy(ETS_M)
accuracy(ETS_auto)

components(ETS_A) %>%
  left_join(fitted(ETS_A), by = c(".model", "Year"))

components(ETS_M) %>%
  left_join(fitted(ETS_M), by = c(".model", "Year"))

components(ETS_auto) %>%
  left_join(fitted(ETS_auto), by = c(".model", "Year"))

ETS_A %>%
  forecast(h = 9) %>%
  autoplot(nuc_tsbl) +
  ylab("Terawatt-hours") + xlab("Year")

ETS_M %>%
  forecast(h = 9) %>%
  autoplot(nuc_tsbl) +
  ylab("Terawatt-hours") + xlab("Year")

ETS_auto %>%
  forecast(h = 9) %>%
  autoplot(nuc_tsbl) +
  ylab("Terawatt-hours") + xlab("Year")

##### ARIMA #####

### DIFFERENCING ###
nuc_tsbl %>% autoplot(difference(box_cox(`Terawatt-hours`, lambda))) +
  labs(title = "Differencing of Global nuclear energy production", x="Year", y="Terawatts-hours")

nuc_tsbl %>% autoplot(difference(difference(box_cox(`Terawatt-hours`, lambda)))) +
  labs(title = "Second Differencing of Global nuclear energy production", x="Year", y="Terawatts-hours")

nuc_tsbl %>%
  gg_tsdisplay(difference(box_cox(`Terawatt-hours`, lambda)), plot_type='partial')

nuc_tsbl %>%
  features(box_cox(`Terawatt-hours`, lambda), unitroot_kpss)
nuc_tsbl %>%
  features(difference(box_cox(`Terawatt-hours`, lambda)), unitroot_kpss)
nuc_tsbl %>%
  features(box_cox(`Terawatt-hours`, lambda), unitroot_ndiffs)

##### MODEL SEARCH #####

nuc_fit = nuc_train%>%
  model(arima020 =ARIMA(box_cox(`Terawatt-hours`, lambda) ~ pdq(0,2,0)),
        arima021 =ARIMA(box_cox(`Terawatt-hours`, lambda) ~ pdq(0,2,1)),
        arima022 =ARIMA(box_cox(`Terawatt-hours`, lambda) ~ pdq(0,2,2)),
        stepwise = ARIMA(box_cox(`Terawatt-hours`, lambda)))
nuc_fit %>%
  forecast(h = 9) %>%
  autoplot(nuc_tsbl)

##### RESIDUALS FOR ARIMA MODELS #####

nuc_fit %>%
  select(arima020) %>%
  gg_tsresiduals()

nuc_fit %>%
  select(arima021) %>%
  gg_tsresiduals()

nuc_fit %>%
  select(arima022) %>%
  gg_tsresiduals()

nuc_fit %>%
  select(stepwise) %>%
  gg_tsresiduals()

##### ACCURACY #####
accuracy(nuc_fit)
glance(nuc_fit)%>% arrange(AICc)

nuc_fit%>%
  augment() %>%
  features(.innov, box_pierce, lag = 10, dof = 0)

nuc_fit%>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 0)

#### FORECAST COMBINATION ######

STLF <- decomposition_model(
  STL(box_cox(`Terawatt-hours`, lambda) ~ trend(window = 7), robust = TRUE),
  ETS(season_adjust ~ season("N"))
)

nuc_models <- nuc_train %>%
  model(
    naive = NAIVE(box_cox(`Terawatt-hours`, lambda)),
    ets = ETS(box_cox(`Terawatt-hours`, lambda)~ error("A") + trend("A") + season("N")),
    stlf = STLF,
    arima022 = ARIMA(box_cox(`Terawatt-hours`, lambda) ~ pdq(0,2,2)),
    nnar = NNETAR(box_cox(`Terawatt-hours`, lambda)),
    aicc = VAR(box_cox(`Terawatt-hours`, lambda)),
    bic = VAR(box_cox(`Terawatt-hours`, lambda), ic = "bic")
  ) %>%
  mutate(combination = (naive + ets + stlf + arima022+ nnar + aicc + bic) / 7)

nuc_combine <- nuc_models %>%
  forecast(h = 9)

nuc_combine%>%
  autoplot(nuc_tsbl) +
  labs(y = "Terawatt-hours", title = "Global Nuclear Energy Production")

##### RESIDUALS #####

nuc_models%>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()

#### other tests #####

nuc_models%>%
  augment() %>%
  features(.innov, box_pierce, lag = 10, dof = 0)

nuc_models%>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 0)

accuracy(nuc_combine, nuc_tsbl)
##### REAL FORECASTING #####

##### Forecast for 2030 #####

nuc_model <- nuc_tsbl %>%
  model(
    naive = NAIVE(box_cox(`Terawatt-hours`, lambda)),
    ets = ETS(box_cox(`Terawatt-hours`, lambda)~ error("A") + trend("A") + season("N")),
    stlf = STLF,
    arima022 = ARIMA(box_cox(`Terawatt-hours`, lambda) ~ pdq(0,2,2)),
    nnar = NNETAR(box_cox(`Terawatt-hours`, lambda)),
    aicc = VAR(box_cox(`Terawatt-hours`, lambda)),
    bic = VAR(box_cox(`Terawatt-hours`, lambda), ic = "bic")
  ) %>%
  mutate(combination = (naive + ets + stlf + arima022+ nnar + aicc + bic) / 7)

nuc_2030 <- nuc_model %>%
  forecast(h = 10)

nuc_2030%>%
  autoplot(nuc_tsbl) +
  labs(y = "Terawatt-hours", title = "Global Nuclear Energy Production Forecasts for 2030")

##### RESIDUALS of nuc_tsbl training #####

nuc_model%>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()

#### other tests #####

nuc_model%>%
  augment() %>%
  features(.innov, box_pierce, lag = 10, dof = 0)

nuc_model%>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 0)

##### Forecast for 2050 #####

nuc_2050 <- nuc_model %>%
  forecast(h = 30)

nuc_2050 %>%
  autoplot(nuc_tsbl) +
  labs(y = "Terawatt-hours", title = "Global Nuclear Energy Production Forecasts for 2050")

##### RESIDUALS #####

nuc_model%>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()

#### other tests #####

nuc_model%>%
  augment() %>%
  features(.innov, box_pierce, lag = 10, dof = 0)

nuc_model%>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 0)
