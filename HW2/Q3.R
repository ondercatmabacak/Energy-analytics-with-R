library(fpp3)
?aus_production
gas = tail(aus_production, 5*4) %>% select(Gas)
gas
gas %>%
  autoplot(Gas)

##### part b ######
gas %>%
  model(
    classical_decomposition(Gas, type = "multiplicative")) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical multiplicative decomposition of Quarterly gas production in Australia")

##### part d #####
gas_model = gas %>%
  model(stl = STL(Gas))
components(gas_model)

gas %>%
  autoplot(Gas, color='gray') +
  autolayer(components(gas_model), season_adjust, color='#D55E00') +
  labs(y='Gas Production', title='Quarterly gas production in Australia')
gas
##### part e #####
ifif = if_else(gas$Gas == 194, 300, gas$Gas)

gas_if = gas %>%
  mutate(Gas=ifif)

gas_if_model = gas_if %>%
  model(stl = STL(Gas))
components(gas_if_model)

gas_if %>%
  autoplot(Gas, color='gray') +
  autolayer(components(gas_if_model), season_adjust, color='#D55E00') +
  labs(y='Gas Production', title='Quarterly gas production in Australia')
