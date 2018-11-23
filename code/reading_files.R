

library(tidyverse)
library(dplyr)
library(lubridate)

uk_bank_holidays <- read.csv('SML_data/uk_bank_holidays.csv',stringsAsFactors = F,sep = ',')
uk_bank_holidays$is_holiday <- 1
uk_bank_holidays <- select(uk_bank_holidays,c(1,3))
colnames(uk_bank_holidays) <- c('date','is_holiday')

weather_daily_darksky <- read.csv('SML_data/weather_daily_darksky.csv',stringsAsFactors = F,sep = ',')
weather_daily_darksky$date <- date(weather_daily_darksky$temperatureMinTime)
weather_daily_darksky <- weather_daily_darksky[order(weather_daily_darksky$date),]
weather_daily_darksky <- select(weather_daily_darksky,c('date','temperatureLow'))
colnames(weather_daily_darksky) <- c('date','temperature_day')

weather_hourly_darksky <- read.csv('SML_data/weather_hourly_darksky.csv',stringsAsFactors = F,sep = ',')
weather_hourly_darksky <- weather_hourly_darksky[order(weather_hourly_darksky$time),]
weather_hourly_darksky$hour <- hour(weather_hourly_darksky$time)
weather_hourly_darksky$date <- date(weather_hourly_darksky$time)
weather_hourly_darksky <- select(weather_hourly_darksky,c('date','hour','temperature'))
colnames(weather_hourly_darksky) <- c('date','hour','temperature_hour')

informations_households <- read.csv('SML_data/informations_households.csv',stringsAsFactors = F,sep = ',')
informations_households$ACORN_group <- gsub('ACORN-', '', informations_households$Acorn)
informations_households[informations_households$ACORN_group=="",]$ACORN_group=NaN
informations_households <- select(informations_households,c(1,2,6,4))
colnames(informations_households) <- c('ID','tariff_type','ACORN_group','ACORN_type')

#data_path = "SML_data/halfhourly_dataset/"
data_path = "SML_data/hal"
files <- list.files(path = data_path)
all_data <- rep(list(NaN),length(files))

#for (i in 1:2){
for (i in 1:length(files)){
  print(i)
  hh_temp <- read.csv(paste0(data_path,files[i]),stringsAsFactors = F,sep = ',')
  colnames(hh_temp) <- c("ID","datetime","energy_kWh")
  hh_temp$date <- date(hh_temp$datetime)
  hh_temp$year <- year(hh_temp$datetime)
  hh_temp$month <- month(hh_temp$datetime)
  hh_temp$day <- day(hh_temp$datetime)
  hh_temp$hour <- hour(hh_temp$datetime)
  hh_temp$minute <- minute(hh_temp$datetime)
  hh_temp <- merge(hh_temp, informations_households, by = "ID",all.x = TRUE)
  hh_temp <- merge(hh_temp, weather_daily_darksky, by = "date",all.x = TRUE)
  hh_temp <- merge(hh_temp, weather_hourly_darksky, by = c("date","hour"), all.x = TRUE)
  hh_temp <- merge(hh_temp,uk_bank_holidays, by = 'date',all = TRUE)
  hh_temp[is.na(hh_temp$is_holiday),]$is_holiday = 0
  hh_temp$is_weekend = 0
  hh_temp[is.weekend(hh_temp$date),]$is_weekend = 1
  hh_temp$is_we_hd = 0
  hh_temp[hh_temp$is_weekend==1|hh_temp$is_holiday==1,]$is_we_hd = 1
  hh_temp <- select(hh_temp,c('ID','datetime','date','year','month','day','hour','minute','energy_kWh','temperature_day','temperature_hour','ACORN_group','ACORN_type','tariff_type','is_we_hd')) 
  all_data[[i]] <- hh_temp
}
save(all_data,file="SML_data/tidy_data/list_data.Rda")
hh_data<-do.call("rbind", all_data)
save(all_data,file="SML_data/tidy_data/hh_data.Rda")

#load("SML_data/tidy_data/hh_data.Rda")
