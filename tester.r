library(tidyverse)
source("ashraeRidge.r")
daily_data <- readRDS("daily_data.rds")
daily_data <- daily_data %>% ungroup()

daily_dataDate <- addDayOfYear(daily_dataDate)


lambdas = c()
mses = c()
for(lambda in seq(0,5,.1)){
  rmse <- kfoldRidgeTest(daily_dataDate,k=12,lambda)
  mses <- append(rmse,mses)
  lambdas <- append(lambda,lambdas)
}
vanmse <-kfoldVanillaTest(daily_dataDate)
print(vanmse)
ggplot(data=NULL, aes(x=lambdas,y=mses))+
  geom_point()+
  geom_line()+
  geom_point(data=NULL,aes(x=c(0),y=c(vanmse)),color='red')