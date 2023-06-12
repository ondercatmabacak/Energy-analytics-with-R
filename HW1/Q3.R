library(fpp3)

##### US_EMPLOYMENT #####
tot_pri = us_employment %>%
  filter(Title=="Total Private")%>%
  mutate(Employed = Employed/1000)
tot_pri
##### AUTOPLOT ######

tot_pri%>%
  autoplot(Employed)+
  labs(y = "Number of Employees (Thousand)",
       title = "Total Private Employment in US")

##### GG_SEASON ######
tot_pri%>%
  gg_season(Employed, labels= "both")+
  labs(y = "Number of Employees (Thousand)",
       title = "Seasonal plot: Total Private Employment in US")

##### GG_SUBSERIES ######
tot_pri%>%
  gg_subseries(Employed)+
  labs(y = "Number of Employees (Thousand)",
       title = "Subseries plot: Total Private Employment in US")

##### GG_LAG ######
tot_pri %>%
  gg_lag(Employed, geom = "point") +
  labs(x = "lag(Employed, k)")

##### ACF #####
tot_pri%>%
      ACF(Employed) %>%
  autoplot() + labs(title= "ACF plot: Total Private Employment in US")

##### HO2 #####
ho2 = PBS %>%
  filter(ATC2=="H02")%>%
  mutate(Cost = Cost/1000)%>%
  select(Month, Concession, ATC2, Type, Cost)

ho2_Co_Cop = ho2 %>%
  filter(Concession=="Concessional", Type=="Co-payments" )
ho2_Co_Cop

ho2_Co_Saf = ho2 %>%
  filter(Concession=="Concessional", Type=="Safety net" )
ho2_Co_Saf

ho2_Gen_Cop = ho2 %>%
  filter(Concession=="General", Type=="Co-payments" )
ho2_Gen_Cop

ho2_Gen_Saf = ho2 %>%
  filter(Concession=="General", Type=="Safety net" )
ho2_Gen_Saf

##### AUTOPLOT #####
ho2%>%
  autoplot(Cost)+
  labs(y = " Cost (Thousand Dollars)",
       title = "Monthly Medicare Cost in Australia")

##### GG_SEASON #####
ho2%>%
  gg_season(Cost, labels= "both")+
  labs(y = "Cost (Thousand Dollars)",
       title = "Seasonal plot: Monthly Medicare Cost in Australia")

##### GG_SUBSERIES #####
ho2%>%
  gg_subseries(Cost)+
  labs(y = "Cost (Thousand Dollars)",
       title = "Seasonal plot: Monthly Medicare Cost in Australia")

##### GG_LAG #####
ho2_Co_Cop%>%
  gg_lag(Cost, geom = "point")+
  labs(y = "Cost (Thousand Dollars)",
       title = "Seasonal plot: Monthly Medicare Cost in Australia (Concessional vs Co-payments)")

ho2_Co_Saf%>%
  gg_lag(Cost, geom = "point")+
  labs(y = "Cost (Thousand Dollars)",
       title = "Seasonal plot: Monthly Medicare Cost in Australia (Concessional vs Safety net)")

ho2_Gen_Cop%>%
  gg_lag(Cost, geom = "point")+
  labs(y = "Cost (Thousand Dollars)",
       title = "Seasonal plot: Monthly Medicare Cost in Australia (General vs Co-payments)")

ho2_Gen_Saf%>%
  gg_lag(Cost, geom = "point")+
  labs(y = "Cost (Thousand Dollars)",
       title = "Seasonal plot: Monthly Medicare Cost in Australia (General vs Safety net)")

#### ACF #####
ho2%>%
  ACF(Cost) %>%
  autoplot() + labs(title= "ACF plot: Total Private Employment in US")

##### US_GASOLINE #####
us_gasoline

##### AUTOPLOT #####
us_gasoline%>%
  autoplot(Barrels)+
  labs(y = " Number of Barrels (million per day)",
       title = "US finished motor gasoline product supplied")

##### GG_SEASON #####
us_gasoline%>%
  gg_season(Barrels, labels= "both")+
  labs(y = " Number of Barrels (million per day)",
       title = "US finished motor gasoline product supplied")

##### GG_SUBSERIES #####
us_gasoline%>%
  gg_subseries(Barrels)+
  labs(y = " Number of Barrels (million per day)",
       title = "US finished motor gasoline product supplied")

##### GG_LAG #####
us_gasoline%>%
  gg_lag(Barrels, geom = "point")+
  labs(y = " Number of Barrels (million per day)",
       title = "US finished motor gasoline product supplied")

##### ACF #####
us_gasoline%>%
  ACF(Barrels) %>%
  autoplot() + labs(title= "ACF plot: US finished motor gasoline product supplied")
