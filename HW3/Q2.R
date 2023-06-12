library(fpp3)
?hh_budget

AUS = hh_budget%>%filter(Country == "Australia")
AUS
CAN = hh_budget%>%filter(Country == "Canada")
CAN
JAP = hh_budget%>%filter(Country == "Japan")
JAP
USA = hh_budget%>%filter(Country == "USA")
USA
##### PART A ######
AUS_test = AUS %>%slice_tail(n=4)
AUS_test
AUS_train = AUS %>%slice_head(n=18)
AUS_train

CAN_test = CAN %>%slice_tail(n=4)
CAN_test
CAN_train = CAN %>%slice_head(n=18)
CAN_train

JAP_test = JAP %>%slice_tail(n=4)
JAP_test
JAP_train = JAP %>%slice_head(n=18)
JAP_train

USA_test = USA %>%slice_tail(n=4)
USA_test
USA_train = USA %>%slice_head(n=18)
USA_train

#####PART B ######

##### AUSSIELAND #####
AUS_fit <- AUS_train %>%
  model(
    Mean = MEAN(Wealth),
    `Naïve` = NAIVE(Wealth),
    Drift = NAIVE(Wealth ~ drift())
  )
# Generate forecasts for 4 years
AUS_fc <- AUS_fit %>% forecast(h = 4)
# Plot forecasts against actual values
AUS_fc %>%
  autoplot(AUS_train, level = NULL) +
  labs(y = "Wealth as a percentage of net disposable income ",
       title = "Household budget characteristics in Aussieland") +
  guides(colour = guide_legend(title = "Forecast"))

##### CANADA #####
CAN_fit <- CAN_train %>%
  model(
    Mean = MEAN(Wealth),
    `Naïve` = NAIVE(Wealth),
    Drift = NAIVE(Wealth ~ drift())
  )
# Generate forecasts for 4 years
CAN_fc <- CAN_fit %>% forecast(h = 4)
# Plot forecasts against actual values
CAN_fc %>%
  autoplot(CAN_train, level = NULL) +
  labs(y = "Wealth as a percentage of net disposable income ",
       title = "Household budget characteristics in Canada") +
  guides(colour = guide_legend(title = "Forecast"))

##### JAPAN #####
JAP_fit <- JAP_train %>%
  model(
    Mean = MEAN(Wealth),
    `Naïve` = NAIVE(Wealth),
    Drift = NAIVE(Wealth ~ drift())
  )
# Generate forecasts for 4 years
JAP_fc <- JAP_fit %>% forecast(h = 4)
# Plot forecasts against actual values
JAP_fc %>%
  autoplot(JAP_train, level = NULL) +
  labs(y = "Wealth as a percentage of net disposable income ",
       title = "Household budget characteristics in Japan") +
  guides(colour = guide_legend(title = "Forecast"))

##### USA #####
USA_fit <- USA_train %>%
  model(
    Mean = MEAN(Wealth),
    `Naïve` = NAIVE(Wealth),
    Drift = NAIVE(Wealth ~ drift())
  )
# Generate forecasts for 4 years
USA_fc <- USA_fit %>% forecast(h = 4)
# Plot forecasts against actual values
USA_fc %>%
  autoplot(USA_train, level = NULL) +
  labs(y = "Wealth as a percentage of net disposable income ",
       title = "Household budget characteristics in Aussieland") +
  guides(colour = guide_legend(title = "Forecast"))

##### PART C #####

accuracy(AUS_fc, AUS)
accuracy(CAN_fc, CAN)
accuracy(JAP_fc, JAP)
accuracy(USA_fc, USA)

##### PART D ######

AUS_fit %>% augment()
CAN_fit %>% augment()
JAP_fit %>% augment()
USA_fit %>% augment()

AUS %>%
  model(NAIVE(Wealth ~ drift())) %>%
  gg_tsresiduals()+
  labs(title = "Time series residuals for Aussieland")
CAN %>%
  model(NAIVE(Wealth ~ drift())) %>%
  gg_tsresiduals()+
  labs(title = "Time series residuals for Canada")

JAP %>%
  model(NAIVE(Wealth ~ drift())) %>%
  gg_tsresiduals()+
  labs(title = "Time series residuals for Japan")

USA %>%
  model(NAIVE(Wealth ~ drift())) %>%
  gg_tsresiduals()+
  labs(title = "Time series residuals for USA")