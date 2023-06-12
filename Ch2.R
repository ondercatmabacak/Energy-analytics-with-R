library(fpp3)

#2.1. tsibble objects
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

y


z <- tibble(
  Month = c('2019 Jan', '2019 Feb', '2019 Mar', '2019 Apr', '2019 May'),
  Observation = c(50, 23, 34, 30, 25),
)

z

#> # A tibble: 5 x 2
#>   Month    Observation
#>   <chr>          <dbl>
#> 1 2019 Jan          50
#> 2 2019 Feb          23
#> 3 2019 Mar          34
#> 4 2019 Apr          30
#> 5 2019 May          25


z = as_tsibble(mutate(z, Month = yearmonth(Month)), index = Month)

z

z %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

# What the function does is to pass the left hand side of the operator to the first argument of the right hand side of the operator

z

#> # A tsibble: 5 x 2 [1M]
#>      Month Observation
#>      <mth>       <dbl>
#> 1 2019 Jan          50
#> 2 2019 Feb          23
#> 3 2019 Mar          34
#> 4 2019 Apr          30
#> 5 2019 May          25

olympic_running
#> # A tsibble: 312 x 4 [4Y]
#> # Key:       Length, Sex [14]
#>     Year Length Sex    Time
#>    <int>  <int> <chr> <dbl>
#>  1  1896    100 men    12  
#>  2  1900    100 men    11  
#>  3  1904    100 men    11  
#>  4  1908    100 men    10.8
#>  5  1912    100 men    10.8
#>  6  1916    100 men    NA  
#>  7  1920    100 men    10.8
#>  8  1924    100 men    10.6
#>  9  1928    100 men    10.8
#> 10  1932    100 men    10.3
#> # . with 302 more rows

olympic_running %>% distinct(Sex)

distinct(olympic_running, Sex)

?PBS
PBS
#> # A tsibble: 67,596 x 9 [1M]
#> # Key:       Concession, Type, ATC1, ATC2 [336]
#>       Month Concession  Type   ATC1  ATC1_desc  ATC2  ATC2_desc  Scripts  Cost
#>       <mth> <chr>       <chr>  <chr> <chr>      <chr> <chr>        <dbl> <dbl>
#>  1 1991 Jul Concession. Co-pa. A     Alimentar. A01   STOMATOLO.   18228 67877
#>  2 1991 Aug Concession. Co-pa. A     Alimentar. A01   STOMATOLO.   15327 57011
#>  3 1991 Sep Concession. Co-pa. A     Alimentar. A01   STOMATOLO.   14775 55020
#>  4 1991 Oct Concession. Co-pa. A     Alimentar. A01   STOMATOLO.   15380 57222
#>  5 1991 Nov Concession. Co-pa. A     Alimentar. A01   STOMATOLO.   14371 52120
#>  6 1991 Dec Concession. Co-pa. A     Alimentar. A01   STOMATOLO.   15028 54299
#>  7 1992 Jan Concession. Co-pa. A     Alimentar. A01   STOMATOLO.   11040 39753
#>  8 1992 Feb Concession. Co-pa. A     Alimentar. A01   STOMATOLO.   15165 54405
#>  9 1992 Mar Concession. Co-pa. A     Alimentar. A01   STOMATOLO.   16898 61108
#> 10 1992 Apr Concession. Co-pa. A     Alimentar. A01   STOMATOLO.   18141 65356
#> # . with 67,586 more rows

PBS %>%
  filter(ATC2 == "A10")
#> # A tsibble: 816 x 9 [1M]
#> # Key:       Concession, Type, ATC1, ATC2 [4]
#>       Month Concession  Type   ATC1  ATC1_desc  ATC2  ATC2_desc Scripts   Cost
#>       <mth> <chr>       <chr>  <chr> <chr>      <chr> <chr>       <dbl>  <dbl>
#>  1 1991 Jul Concession. Co-pa. A     Alimentar. A10   ANTIDIAB.   89733 2.09e6
#>  2 1991 Aug Concession. Co-pa. A     Alimentar. A10   ANTIDIAB.   77101 1.80e6
#>  3 1991 Sep Concession. Co-pa. A     Alimentar. A10   ANTIDIAB.   76255 1.78e6
#>  4 1991 Oct Concession. Co-pa. A     Alimentar. A10   ANTIDIAB.   78681 1.85e6
#>  5 1991 Nov Concession. Co-pa. A     Alimentar. A10   ANTIDIAB.   70554 1.69e6
#>  6 1991 Dec Concession. Co-pa. A     Alimentar. A10   ANTIDIAB.   75814 1.84e6
#>  7 1992 Jan Concession. Co-pa. A     Alimentar. A10   ANTIDIAB.   64186 1.56e6
#>  8 1992 Feb Concession. Co-pa. A     Alimentar. A10   ANTIDIAB.   75899 1.73e6
#>  9 1992 Mar Concession. Co-pa. A     Alimentar. A10   ANTIDIAB.   89445 2.05e6
#> 10 1992 Apr Concession. Co-pa. A     Alimentar. A10   ANTIDIAB.   97315 2.23e6
#> # . with 806 more rows

PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost)
#> # A tsibble: 816 x 4 [1M]
#> # Key:       Concession, Type [4]
#>       Month Concession   Type           Cost
#>       <mth> <chr>        <chr>         <dbl>
#>  1 1991 Jul Concessional Co-payments 2092878
#>  2 1991 Aug Concessional Co-payments 1795733
#>  3 1991 Sep Concessional Co-payments 1777231
#>  4 1991 Oct Concessional Co-payments 1848507
#>  5 1991 Nov Concessional Co-payments 1686458
#>  6 1991 Dec Concessional Co-payments 1843079
#>  7 1992 Jan Concessional Co-payments 1564702
#>  8 1992 Feb Concessional Co-payments 1732508
#>  9 1992 Mar Concessional Co-payments 2046102
#> 10 1992 Apr Concessional Co-payments 2225977
#> # . with 806 more rows

PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost))
#> # A tsibble: 204 x 2 [1M]
#>       Month  TotalC
#>       <mth>   <dbl>
#>  1 1991 Jul 3526591
#>  2 1991 Aug 3180891
#>  3 1991 Sep 3252221
#>  4 1991 Oct 3611003
#>  5 1991 Nov 3565869
#>  6 1991 Dec 4306371
#>  7 1992 Jan 5088335
#>  8 1992 Feb 2814520
#>  9 1992 Mar 2985811
#> 10 1992 Apr 3204780
#> # . with 194 more rows

PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6)
#> # A tsibble: 204 x 3 [1M]
#>       Month  TotalC  Cost
#>       <mth>   <dbl> <dbl>
#>  1 1991 Jul 3526591  3.53
#>  2 1991 Aug 3180891  3.18
#>  3 1991 Sep 3252221  3.25
#>  4 1991 Oct 3611003  3.61
#>  5 1991 Nov 3565869  3.57
#>  6 1991 Dec 4306371  4.31
#>  7 1992 Jan 5088335  5.09
#>  8 1992 Feb 2814520  2.81
#>  9 1992 Mar 2985811  2.99
#> 10 1992 Apr 3204780  3.20
#> # . with 194 more rows

PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC / 1e6) -> a10

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison <- prison %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)

prison
#> # A tsibble: 3,072 x 6 [1Q]
#> # Key:       State, Gender, Legal, Indigenous [64]
#>    State Gender Legal    Indigenous Count Quarter
#>    <chr> <chr>  <chr>    <chr>      <dbl>   <qtr>
#>  1 ACT   Female Remanded ATSI           0 2005 Q1
#>  2 ACT   Female Remanded ATSI           1 2005 Q2
#>  3 ACT   Female Remanded ATSI           0 2005 Q3
#>  4 ACT   Female Remanded ATSI           0 2005 Q4
#>  5 ACT   Female Remanded ATSI           1 2006 Q1
#>  6 ACT   Female Remanded ATSI           1 2006 Q2
#>  7 ACT   Female Remanded ATSI           1 2006 Q3
#>  8 ACT   Female Remanded ATSI           0 2006 Q4
#>  9 ACT   Female Remanded ATSI           0 2007 Q1
#> 10 ACT   Female Remanded ATSI           1 2007 Q2
#> # . with 3,062 more rows

prison %>% distinct(State)
prison %>% distinct(Gender)
prison %>% distinct(Indigenous)
prison %>% distinct(Legal)

# More examples:
global_economy
?global_economy

tourism
?tourism


# 2.2. Time plots
melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)

autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

#More examples:
autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")


?ansett
ansett %>%
  autoplot(Passengers)

ansett %>%
  filter(Class=="Economy") %>%
  autoplot(Passengers)

ansett %>%
  filter(Class=="Economy")

ansett %>%
  filter(Airports=="MEL-SYD") %>%
  autoplot(Passengers)

ansett %>%
  filter(Airports=="MEL-SYD")


?filter
?summarise
?select

########
# 2.3. Time series patterns
# Seasonal plots
?gg_season

a10 %>%
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales") +
  expand_limits(x = ymd(c("1972-12-28", "1974-06-04")))

?vic_elec
vic_elec %>% gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MW", title="Electricity demand: Victoria")

# The position and justification of legends are controlled by the theme setting legend.position, and the value can be right, left, top, bottom, none (no legend), or a numeric position

vic_elec %>% gg_season(Demand, period = "year") +
  labs(y="MW", title="Electricity demand: Victoria")

a10 %>%
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

?tourism
tourism
distinct(tourism, State)

holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

holidays

autoplot(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

gg_season(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")


holidays %>%
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

###########################
# 2.6. Scatterplots
# Half-hourly electricity demand in Victoria, Australia, for 2014.
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Demand) +
  labs(y = "GW",
       title = "Half-hourly electricity demand: Victoria")

# Half-hourly temperature in Melbourne, Australia, for 2014
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

# Half-hourly electricity demand plotted against temperature for 2014 in Victoria, Australia
vic_elec %>%
  filter(year(Time) == 2014) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")

# Scatterplot matrices
# Quarterly visitor nights for the states and territories of Australia
visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))
visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")

# A scatterplot matrix of the quarterly visitor nights in the sates and territories of Australia
visitors %>%
  pivot_wider(values_from=Trips, names_from=State) %>%
  GGally::ggpairs(columns = 2:9)

# 2.7 Lag plots
# Lagged scatterplots for quarterly beer production
?aus_production
aus_production
recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)

recent_production
recent_production %>%
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

# 2.8 Autocorrelation
?ACF

# autocorrelation coefficients for the beer production data
recent_production %>% ACF(Beer, lag_max = 9)

# autocorrelation plot of the quarterly beer production data
recent_production %>%
  ACF(Beer) %>%
  autoplot() + labs(title="Australian beer production")

# ACF of the monthly Australian antidiabetic drug sales
PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC / 1e6) -> a10

autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

a10 %>%
  ACF(Cost, lag_max = 48) %>%
  autoplot() +
  labs(title="Australian antidiabetic drug sales")

#White noise time series
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + labs(title = "White noise", y = "")

# ACF for the white noise series
y %>%
  ACF(wn) %>%
  autoplot() + labs(title = "White noise")
