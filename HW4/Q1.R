library(fpp3)
library(ggplot2)
#Monthly souvenirs sales at beach resort town in Queensland, Australia.
# Large influx during Christmas and local surfing festival 
#possible seasonalities on March expect 1987
?souvenirs
souvenirs
##### PART A #####
souvenirs%>%autoplot()
##### log plot#####
souvenirs%>%autoplot(log(Sales))
##### autocorrelation function
souvenirs%>%ACF(Sales)%>%autoplot()

##### SEATS ##### 
#Since the data setis monthly, I wanted to see if SEATS would give any different results from STL
souvenirs_dcmp <- souvenirs %>%
  model(seats = X_13ARIMA_SEATS(Sales ~ seats())) %>%
  components()
autoplot(souvenirs_dcmp) +
  labs(title =
         "Decomposition of Monthly souvenirs sales using SEATS")

##### STL Decomposition%>%
souvenirs%>%model(
    STL(Sales ~ trend(window = 13) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()
##### PART B #####
souvenirs %>%
  features(Sales, features = guerrero) %>%
  pull(lambda_guerrero)
souvenirs %>%
  autoplot(box_cox(Sales, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Monthly souvenirs sales with $\\lambda$ = ",
         round(lambda,2))))

##### PART C ######
dummy_fest = souvenirs%>%
  mutate(festival = month(Month) == 3 & year(Month) != 1987) 
fore_data = data.frame(dummy_fest = rep(0, 84))
fore_data[3,] = 1

dfdf=dummy_fest%>%
  mutate(d_fest = fore_data$dummy_fest)
fit_sales = dfdf%>%
  model(TSLM(log(Sales) ~ trend() + season() - festival - d_fest))

fit_sales %>%augment()%>%
  ggplot(aes(x = Month, y = log(Sales))) +
  labs(y = "Sales",
       x = "Month") +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y~x', se = FALSE)

##### PART D #####
fit_svnr%>%augment()%>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = .resid))
#### PART E ######
res<-as.data.frame(residuals(fit_svnr))
res
svnr_date<-seq(as.Date("1987/1/1"),as.Date("1993/12/1"),by="month")
length(svnr_date)

res$Month<-svnr_date
boxplot(res$.resid ~ reorder(format(res$Month,'%B'),res$Month), outline = FALSE)
augment(fit_svnr)%>%  # Something is missing, I couldn't find how to plot individual months
  ggplot(aes(x = Month , y = .resid )) +
  geom_boxplot()

##### PART F #####
# check coeffs
coef(fit_svnr)
##### PART G #####
augment(fit_svnr)%>% features(.innov, ljung_box, lag = 10, dof = 0)

##### PART H #####
souvenirs%>%
  tail()
# Building a predictive regression model
svnr_fit <- souvenirs %>%
  model(TSLM(Sales ~ trend() + season()))
new_svnr <- scenarios(
  "Average increase" = new_data(souvenirs, 36) %>%
    mutate(Sales = mean(souvenirs$Sales)),
  "Extreme increase" = new_data(souvenirs, 36) %>%
    mutate(Sales = 1.0e6),
  names_to = "Scenario"
)
fcast <- forecast(svnr_fit, new_svnr)

souvenirs %>%
  autoplot(Sales) +
  autolayer(fcast) +
  labs(title = "Monthly souvenirs sales", y = "Sales")
