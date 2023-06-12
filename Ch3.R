# Chapter 3 - Time series decomposition
library(fpp3)

# Population adjustments
?global_economy
global_economy
#global_economy %>%
#  filter(Country == "Australia") %>%
#  autoplot(GDP) +
#  labs(title= "GDP", y = "$US")

# This can be seen in the global_economy dataset, where a common transformation of GDP is GDP per-capita
global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP/Population) +
  labs(title= "GDP per capita", y = "$US")

# Inflation Adjustments
?aus_retail
aus_retail
aus_retail %>% distinct(Industry)

print_retail <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") %>%
  group_by(Industry) %>%
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))

print_retail

?global_economy
global_economy
aus_economy <- global_economy %>%
  filter(Code == "AUS")

aus_economy

#For example, looking at aggregate annual "newspaper and book" retail turnover from aus_retail, and adjusting the data for inflation using CPI from global_economy allows us to understand the changes over time.
print_retail %>%
  left_join(aus_economy, by = "Year") %>%
  mutate(Adjusted_turnover = Turnover / CPI * 100) %>%
  pivot_longer(c(Turnover, Adjusted_turnover),
               values_to = "Turnover") %>%
  mutate(name = factor(name,
                       levels=c("Turnover","Adjusted_turnover"))) %>%
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry",
       y = "$AU")

?left_join      # The mutating joins add columns from y to x, matching rows based on the keys
?pivot_longer   # "lengthens" data, increasing the number of rows and decreasing the number of columns

# Mathematical transformation
?features
lambda <- aus_production %>%
  features(Gas, features = guerrero) %>%
  pull(lambda_guerrero)   # pull() is similar to $. It's mostly useful because it looks a little nicer in pipes

?aus_production
aus_production
aus_production %>%
  autoplot(box_cox(Gas, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))

##3.2. Time series components
?us_employment
us_employment
us_employment %>% distinct(Title)
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment

autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
components(dcmp)

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

components(dcmp) %>% autoplot()

#3.3. Moving Averages
?global_economy
global_economy
global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

?slide_dbl
aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
                               .before = 2, .after = 2, .complete = TRUE)
  )

aus_exports %>%
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports")

#Moving Averages of moving averages
?aus_production
aus_production
beer <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  select(Quarter, Beer)

beer

beer_ma <- beer %>%
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                               .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE)
  )

beer_ma

# Example: Employment in the US retail sector
us_retail_employment
us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )

us_retail_employment_ma

us_retail_employment_ma %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")
