library(fpp3)

?aus_livestock
##### BOX-COX TRANSFORMATIONS ######
aus_livestock %>%
  select(Month, State, Count) %>%
  filter(Animal== "Bulls, bullocks and steers", State=="Victoria") %>%
  autoplot(Count) +
  labs(title="Number of Bulls, Bullocks and Steers according to States between 1976-2018", subtitle = "Victoria")

Victoria = aus_livestock %>%
  select(Month, State, Count) %>%
  filter(Animal== "Bulls, bullocks and steers", State=="Victoria")

lambda = Victoria %>%
  features(Count, features = guerrero) %>%
  pull(lambda_guerrero)

Victoria %>%
  autoplot(box_cox(Count, lambda)) +
  labs(y = "", title = latex2exp::TeX(paste0("Box-Cox Transformed gas production with $\\lambda$ = ", lambda)), subtitle = "Victoria")

##### VICTORIAN ELECTRIC DEMAND #####
vic_elec

vic_elec %>%
  select(Time, Demand) %>%
  autoplot(Demand) +
  labs(title="Half-hourly electricity demand for Victoria, Australia")

lambda = vic_elec %>%
  features(Demand, features = guerrero) %>%
  pull(lambda_guerrero)

vic_elec %>%
  autoplot(box_cox(Demand, lambda)) +
  labs(y = "", title = latex2exp::TeX(paste0("Box-Cox Transformed gas production with $\\lambda$ = ", lambda)), subtitle= "Victorian Electricity Demand")



##### AUSSIE-LAND GAS PRODUCTION ##### 
aus_production
gas_pro = aus_production %>%
  select(Quarter, Gas) 

gas_pro%>%
  autoplot(Gas) +
  labs(title="Quarterly production of selected commodities in Australia", y="Gas (petajoules)")

lambda = gas_pro %>%
  features(Gas, features = guerrero) %>%
  pull(lambda_guerrero)

gas_pro %>%
  autoplot(box_cox(Gas, lambda)) +
  labs(y = "", title = latex2exp::TeX(paste0("Box-Cox Transformed gas production with $\\lambda$ = ", lambda)), subtitle = "Australia Gas Production")
