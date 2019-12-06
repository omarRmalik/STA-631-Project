library(lmridge)
library(lubridate)

addDayOfYear <- function(df){
  daily_data %>%
    mutate(dayOfYear=as.Date(ymd(date)) - dmy(paste('01-01-',year(ymd(date)),sep="")))
}

buildRidgeModel <- function (df,lambda=0){
  lmridge(meter_reading ~ dayOfYear+air_temperature+cloud_coverage+ 
            dew_temperature+precip_depth_1_hr+sea_level_pressure+
            wind_direction+wind_speed,df,K = lambda)
}
testModel <- function(df,mod){
  preds <- predict(mod,df)
  mse <- sqrt(mean((preds-df$meter_reading)^2))
  mse
}
buildVanillaModel <- function(df){
  lm(meter_reading ~ dayOfYear+air_temperature+cloud_coverage+ 
            dew_temperature+precip_depth_1_hr+sea_level_pressure+
            wind_direction+wind_speed,df)
}

kfoldVanillaTest <- function(daily_dataDate,k=8){
  #kfold tester for the vanilla regression
  #returns the calculated mse
  sliceSize <- nrow(daily_dataDate)/k
  mse <- 0
  for(i in seq(0,k-1)){
      
    for(mtype in daily_dataDate %>% distinct(meter) %>% pull(meter)){
      testStart <- i*sliceSize + 1
      testEnd <- (i+1)*sliceSize
      testDf <- daily_dataDate %>% slice(testStart:testEnd)
      testDf <- testDf
      trainDfHead <- daily_dataDate %>% slice(0:max(testStart-1,0))
      trainDfTail <- daily_dataDate %>% slice(testEnd+1:nrow(daily_dataDate))
      if(nrow(trainDfHead) == 0){
        trainDf <- trainDfTail
      }else if(nrow(trainDfTail)==0){
        trainDf <- trainDfHead
      }else{
        trainDf<-rbind(trainDfHead,trainDfTail)
      }
      
      trainDf <- trainDf %>% filter(meter==mtype)
      testDf <- testDf %>% filter(meter==mtype)
      mod <- buildVanillaModel(trainDf)
      mse <- testModel(testDf,mod)+ mse
    }
  }
  mse <- (mse/k)/daily_dataDate %>% distinct(meter) %>% nrow()
 
}

kfoldRidgeTest <- function(daily_dataDate,k=8){
  #kfold tester for the ridge regression
  #tests all lambdas between 0 & 10 at a .2 increment
  #returns a ggplot of the mse's by lambda
  sliceSize <- nrow(daily_dataDate)/k
  lambdas = c()
  mses = c()
  for(lambda in seq(0,10,0.2)){
    mse <- 0
    for(i in seq(0,k-1)){
      
      for(mtype in daily_dataDate %>% distinct(meter) %>% pull(meter)){
        testStart <- i*sliceSize + 1
        testEnd <- (i+1)*sliceSize
        testDf <- daily_dataDate %>% slice(testStart:testEnd)
        testDf <- testDf
        trainDfHead <- daily_dataDate %>% slice(0:max(testStart-1,0))
        trainDfTail <- daily_dataDate %>% slice(testEnd+1:nrow(daily_dataDate))
        if(nrow(trainDfHead) == 0){
          trainDf <- trainDfTail
        }else if(nrow(trainDfTail)==0){
          trainDf <- trainDfHead
        }else{
          trainDf<-rbind(trainDfHead,trainDfTail)
        }
        trainDf <- trainDf %>% filter(meter==mtype)
        testDf <- testDf %>% filter(meter==mtype)
        mod <- buildRidgeModel(trainDf,lambda = lambda)
        mse <- testModel(testDf,mod)+ mse
      }
    }
    mse <- (mse/k)/daily_dataDate %>% distinct(meter) %>% nrow()
    lambdas <- append(lambdas,lambda)
    mses <- append(mses,mse)
  }
  ggplot(data=NULL,aes(x=lambdas,y=mses))+
    geom_line()+
    geom_point()
  
  
}