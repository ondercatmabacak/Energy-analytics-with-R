library(fpp3)
aus_livestock

pigs = aus_livestock %>%
  filter(Animal == "Pigs", State=="New South Wales")%>%
  mutate(Count= Count/1000)

pigs
##### PART A #####

##### autoplot #####  
pigs%>%
  autoplot(Count) +
  labs(y = "Counts (Thousand)",
       title = "Number of Pigs slaughtered in New South Wales - Aussieland")
##### gg_season ######
pigs%>%
  gg_season(Count, labels= "both")+
  labs(y = "Counts (Thousand)",
       title = "GG_SEASON: Number of Pigs slaughtered in New South Wales - Aussieland")

##### gg_subseries ######
pigs%>%
  gg_subseries(Count)+
  labs(y = "Counts (Thousand)",
       title = "GG_SUBSERIES: Number of Pigs slaughtered in New South Wales - Aussieland")

##### gg_lag ######
pigs %>%
  gg_lag(Count, geom = "point") +
  labs(x = "lag(Count, k)")

##### acf #####
pigs%>%
  ACF(Count) %>%
  autoplot() + 
  labs(title= "ACF plot: Number of Pigs slaughtered in New South Wales - Aussieland")

##### BOX-COX #####

lambda = pigs %>%
  features(Count, features = guerrero) %>%
  pull(lambda_guerrero)
pigs %>%
  autoplot(box_cox(Count, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Number of Pigs slaughtered in New South Wales with $\\lambda$ = ",
         round(lambda,4))))

##### STL #####
dcmp <- pigs %>%
  model(stl = STL(Count))
components(dcmp)
components(dcmp)%>%
  as_tsibble() %>%
  autoplot(Count, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Count (thousands)",
    title = "STL: Number of Pigs slaughtered in New South Wales"
  )

##### STL decomposition #####
components(dcmp)%>% autoplot() + 
  labs(title= "STL decomposition: Number of Pigs slaughtered in New South Wales - Aussieland")

##### seasonally adjusted #####

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Count, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Count (thousands)",
       title = "Seasonally Adjusted Number of Pigs slaughtered in New South Wales")

##### Moving Averages #####
pigs %>%
  mutate(
    `12-MA` = slider::slide_dbl(Count, mean,.before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,.before = 1, .after = 0, .complete = TRUE))%>%
  autoplot(Count) +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Count (Thousand)",
       title = "Moving Averages (2x12-MA) for the Number of Pigs slaughtered in New South Wales") +
  guides(colour = guide_legend(title = "series"))

##### Additive Decomposition #####

pigs %>%
  model(
    classical_decomposition(Count, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition Number of Pigs slaughtered in New South Wales")

##### multiplicative Decomposition #####

pigs %>%
  model(
    classical_decomposition(Count, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Multiplicative decomposition Number of Pigs slaughtered in New South Wales")

##### X-11 #####
x11_pigs <- pigs %>%
  model(x11 = X_13ARIMA_SEATS(Count ~ x11())) %>%
  components()
autoplot(x11_pigs) +
  labs(title ="Decomposition of Number of Pigs slaughtered in New South Wales using X-11.")

##### Trend-cycle vs Seasonally adjusted data #####

x11_pigs %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Count, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Count (thousands)",
       title = "Number of Pigs slaughtered in New South Wales") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

##### SEATS #####

seats_pigs <- pigs %>%
  model(seats = X_13ARIMA_SEATS(Count ~ seats())) %>%
  components()
autoplot(seats_pigs) +
  labs(title =
         "Decomposition of Number of Pigs slaughtered in New South Wales using SEATS")

##### STL Decomposition #####

pigs %>%
  model(
    STL(Count ~ trend(window = 13) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


##### PART B #####

test = pigs %>%
  slice_tail(n=72)
test
train = pigs %>%
  slice_head(n=486)
train

##### PART C #####

pigs_fit <- train %>%
  model(
    Mean = MEAN(Count),
    `Naïve` = NAIVE(Count),
    `Seasonal naïve` = SNAIVE(Count),
    Drift = NAIVE(Count ~ drift())
  )
# Generate forecasts for 72 months
pigs_fc <- pigs_fit %>% forecast(h = 72)
# Plot forecasts against actual values
pigs_fc %>%
  autoplot(train, level = NULL) +
  autolayer(test, colour = "black") +
  labs(y = "Count(Thousand)",title = "Pigs slaughtered in New South Wales") +
  guides(colour = guide_legend(title = "Forecast"))

##### Naive #####
fit <- train %>%
  model(NAIVE(Count))
sim <- fit %>% generate(h = 72, times = 5, bootstrap = TRUE)

train %>%
  model(NAIVE(Count)) %>%
  forecast(h = 72) %>%
  autoplot(pigs) +
  labs(title="Pigs slaughtered in New South Wales", y="Count(Thousand)" )

train %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Count)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = sim) +
  labs(title="Pigs slaughtered in New South Wales", y="Count(Thousand)" ) +
  guides(colour = "none")

##### PART D ######

pigs_fit %>% augment()

pigs %>%
  model(NAIVE(Count)) %>%
  gg_tsresiduals()

pigs %>%
  model(SNAIVE(Count)) %>%
  gg_tsresiduals()