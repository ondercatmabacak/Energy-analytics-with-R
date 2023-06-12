library(fpp3)

dgoog = gafa_stock %>%
  filter(Symbol=="GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))
dgoog

dgoog_open = dgoog%>%
  mutate(diff = difference(Open))
dgoog_open

dgoog_high = dgoog%>%
  mutate(diff = difference(High))
dgoog_high

dgoog_low= dgoog%>%
  mutate(diff = difference(Low))
dgoog_low

dgoog %>%
  ACF(diff) %>%
  autoplot() + labs(title= "ACF plot:  daily changes in Google closing stock prices (Close)")

dgoog_open %>%
  ACF(diff) %>%
  autoplot() + labs(title= "ACF plot:  daily changes in Google closing stock prices (Open)")

dgoog_high %>%
  ACF(diff) %>%
  autoplot() + labs(title= "ACF plot:  daily changes in Google closing stock prices (High)")

dgoog_low %>%
  ACF(diff) %>%
  autoplot() + labs(title= "ACF plot:  daily changes in Google closing stock prices(Low)")
