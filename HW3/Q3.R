library(fpp3)
?aus_production
brick = aus_production%>%
  select(Quarter, Bricks)%>%
  filter_index("1956 Q1" ~ "2005 Q2")  # 2005 Q3 & Q4 are NA.
##### PART A #####
##### STL decomposition #####
brick %>%
  model(
    STL(Bricks ~ trend(window = 13) +
          season(window = "periodic"),
        robust = FALSE)) %>%
  components() %>%
  autoplot()+
  labs(title = 'STL Decomposition: Clay Brick Production in Aussieland')

##### PART B #####

##### STL #####
dcmp <- brick %>%
  model(STL(Bricks ~ trend(window = 13) +
              season(window = "periodic"),
            robust = FALSE))
components(dcmp)
components(dcmp)%>%
  as_tsibble() %>%
  autoplot(Bricks, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Count (Millions)",
    title = "STL: Clay Brick Production in Aussieland"
  )

##### seasonally adjusted #####

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Bricks, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  #geom_line(aes(y=trend), colour = "#D55E00") +
  labs(y = "Count (Millions)",
       title = "Seasonally Adjusted Number of Clay Brick Production in Aussieland")

##### PART C #####

dcmp1 <- brick %>%
  model(STL(Bricks ~ season(window = "periodic"), robust = FALSE)) %>%
  components() %>%
  select(-.model)

dcmp1 %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp1) +
  labs(y = "Count (Millions)",
       title = "Forecast for Seasonally Adjusted Clay Brick Production in Aussieland")

##### PART D #####

fit_dcmp1 <- brick %>%
  model(stlf = decomposition_model(
    STL(Bricks ~ season(window = "periodic"), robust = FALSE),
    NAIVE(season_adjust)
  ))

fit_dcmp1 %>%
  forecast() %>%
  autoplot(brick)+
  labs(y = "Count (Millions)",
       title = "STL Decomposition model forecast for Clay Brick Production in Aussieland")

##### PART E #####
dcmp11 <- brick %>%
  model(STL(Bricks ~ season(window = "periodic"), robust = FALSE))
dcmp11%>% gg_tsresiduals()
fit_dcmp1 %>% gg_tsresiduals()

##### PART F #####

fit_dcmp2 <- brick %>%
  model(stlf = decomposition_model(
    STL(Bricks ~ season(window = "periodic"), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_dcmp2 %>%
  forecast() %>%
  autoplot(brick)+
  labs(y = "Count (Millions)",
       title = "STL Decomposition model forecast for Clay Brick Production in Aussieland")

##### PART G #####

brick_train = brick %>%
  filter_index("1956 Q1" ~ "2003 Q2")
brick_train
brick_test = brick %>%
  filter_index("2003 Q3" ~ "2005 Q2")
brick_test

##### SNAIVE #####

brick_fit <- brick_train %>%
  model(`Seasonal naïve` = SNAIVE(Bricks))
# Generate forecasts for 72 months
brick_fc <- brick_fit %>% forecast(h = 8)
# Plot forecasts against actual values
brick_fc %>%
  autoplot(brick_train, level = NULL) +
  autolayer(brick_test, colour = "black") +
  labs(y = "Count(Millions)",title = "SNAIVE: Clay Brick Production in Aussieland") +
  guides(colour = guide_legend(title = "Forecast"))

##### Decomposition model #####

brick_dcmp <- brick_train %>%
  model(stlf = decomposition_model(
    STL(Bricks ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

brick_dcmp_fc <- brick_dcmp %>% forecast(h = 8)
# Plot forecasts against actual values
brick_dcmp_fc %>%
  autoplot(brick_train, level = NULL) +
  autolayer(brick_test, colour = "black") +
  labs(y = "Count(Millions)",title = "Decomposition Model: Clay Brick Production in Aussieland") +
  guides(colour = guide_legend(title = "Forecast"))

brick_fit%>% gg_tsresiduals()
brick_dcmp%>% gg_tsresiduals()