library(fpp3)
library(zoo)
#### PART A ######
data = readxl::read_excel("/home/onder/R/HW1/tourism.xlsx")
#### PART B ######
fun <- function(Quarter)
fun_v = Vectorize(fun, "Quarter")
fun_v(data$Quarter)
data$Quarter <- yearquarter(data$Quarter)
data
data_tsbl = data%>%
  as_tsibble(index = Quarter, key = c(Region, State, Purpose))
data_tsbl
#### comparison ###
tourism

##### PART C ####
library(plyr)
data_mean= ddply(data_tsbl, .(Region, Purpose), summarize,  Mean_Trips=mean(Trips))
data_mean
colMax(data_mean)

##### PART D #####

ddply(data_tsbl, .(Region, Purpose), summarize,  Sum_Trips=sum(Trips))
