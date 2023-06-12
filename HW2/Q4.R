library(fpp3)

?canadian_gas
canadian_gas
##### part a #####

##### AUTOPLOT ######
canadian_gas%>%
  autoplot(Volume)+
  labs(y = "Gas production (billions of meter cubes )",
       title = "Monthly Canadian gas production")

##### GG_SEASON ######
canadian_gas%>%
  gg_season(Volume, labels= "both")+
  labs(y = "Gas production (billions of meter cubes )",
       title = "Season plot: Monthly Canadian gas production")

##### GG_SUBSERIES ######
canadian_gas%>%
  gg_subseries(Volume)+
  labs(y = "Gas production (billions of meter cubes )",
       title = "Subseries plot: Monthly Canadian gas production")

##### STL Decomposition #####
canadian_gas %>%
  model(STL(Volume ~ season(window='periodic'), robust=TRUE)) %>%
  components() %>% autoplot() +
  labs(y="Gas production (billions of meter cubes )", title="STL decomposition: Monthly Canadian gas production")

##### Seasonal component #####
can_gas_model = canadian_gas%>%
  model(stl = STL(Volume))
can_gas_comp = components(can_gas_model)
can_gas_comp
can_gas_comp %>%
  gg_season(season_year, labels= "both")+
  labs(y = "Gas production (billions of meter cubes )",
       title = "Season plot: Monthly Canadian gas production", subtitle = "Season Year - STL")
canadian_gas %>%
  autoplot(Volume, color='gray') +
  autolayer(components(can_gas_model), season_adjust, color='#D55E00') +
  labs(y='Gas Production', title='Quarterly gas production in Australia')

##### X11 and SEATS #####
x11_canadian_gas <- canadian_gas %>%
  model(x11 = X_13ARIMA_SEATS(Volume ~ x11())) %>%
  components()
autoplot(x11_canadian_gas) +
  labs(title =
         "Monthly Canadian gas production using X-11.")

SEATS_canadian_gas <- canadian_gas %>%
  model(seats = X_13ARIMA_SEATS(Volume ~ seats())) %>%
  components()
autoplot(SEATS_canadian_gas) +
  labs(title =
         "Monthly Canadian gas production using SEATS")
