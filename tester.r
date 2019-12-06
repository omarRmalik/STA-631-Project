library(tidyverse)
source("ashraeRidge.r")
daily_data <- readRDS("daily_data.rds")
daily_data <- daily_data %>% ungroup()

daily_dataDate <- addDayOfYear(daily_dataDate)

kfoldRidgeTest(daily_dataDate,k=12)
vanmse <-kfoldVanillaTest(daily_dataDate)

