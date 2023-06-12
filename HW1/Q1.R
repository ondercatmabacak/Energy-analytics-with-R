library(dplyr)
library(lubridate)
library(tsibble)

##### PART B ######

ustotal = as_tsibble(USgas::us_total, index=year,key=state)
ustotal

##### PART C ######

print = ustotal %>%
  filter(state==c("Maine","Vermont","New Hampshire","Massachusetts","Connecticut","Rhode Island"))%>%
  mutate(y=y/1000)
autoplot(print, y) +
  labs(y = "Consumption (Thousand)",
       title = "Annual natural gas consumption by state for the New England area")

##### To increase resolution for individual states ######

print%>%
  ggplot(aes(x = year, y = y)) +
  geom_line() +
  facet_grid(vars(state), scales = "free_y") +
  labs(title = "Annual natural gas consumption by state for the New England area",
       y= "Consumption (Thousand)")

