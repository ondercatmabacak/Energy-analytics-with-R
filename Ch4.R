# Chapter 4 - Time series features
library(fpp3)

?features

#Some simple statistics (Descriptive satistics)
tourism %>%
  features(Trips, list(mean = mean)) %>%       #Lists are the R objects which contain elements of different types like ??? numbers, strings, vectors and another list inside it
  arrange(mean)

?arrang  # Order a data frame by its columns.

# Compute more features at a time: five summary statistics
tourism %>% features(Trips, quantile)

# 4.2. ACF features
tourism %>% features(Trips, feat_acf)

# 4.3 STL Features
tourism %>%
  features(Trips, feat_stl)

tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

?facet_wrap     #facet_wrap() wraps a 1d sequence of panels into 2d

tourism %>%
  features(Trips, feat_stl) %>%
  filter(
    seasonal_strength_year == max(seasonal_strength_year)
  ) %>%
  left_join(tourism, by = c("State", "Region", "Purpose")) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))

?left_join
?facet_grid

# Example: Exploring Australian tourism data
# All of the features included in the feasts package can be computed in one line like this.
tourism_features_ASB <- tourism %>%
  filter(Region=="Adelaide", State=="South Australia", Purpose=="Business") %>%
  features(Trips, feature_set(pkgs = "feasts"))

tourism_features_ASB
tourism_features_ASB$seasonal_peak_year

tourism_features <- tourism %>%
  features(Trips, feature_set(pkgs = "feasts"))

tourism_features
tourism_features$seasonal_peak_year

#Pairwise plots of all seasonal features for the Australian tourism
library(glue)
?select_at
?contains
?ggpairs
?GGally   
?glue

tourism_features %>%
  select_at(vars(contains("season"), Purpose)) %>%
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) %>%
  GGally::ggpairs(mapping = aes(colour = Purpose))


library(broom)
pcs <- tourism_features %>%
  select(-State, -Region, -Purpose) %>%
  prcomp(scale = TRUE) %>%
  augment(tourism_features)    # Add all pcs to the tourism_features object

pcs$.fittedPC1      # To access each component in the pcs use $
?augment      

pcs %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

?theme

outliers <- pcs %>%
  filter(.fittedPC1 > 10) %>%
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)

outliers


outliers %>%
  left_join(tourism, by = c("State", "Region", "Purpose")) %>%
  mutate(
    Series = glue("{State}", "{Region}", "{Purpose}",
                  .sep = "\n\n")
  ) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")

