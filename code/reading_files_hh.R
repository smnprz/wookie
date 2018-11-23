

library(tidyverse)
library(dplyr)
library(lubridate)

uk_bank_holidays <- read.csv('SML_data/uk_bank_holidays.csv',stringsAsFactors = F,sep = ',')
uk_bank_holidays$is_holiday <- 1
uk_bank_holidays$Bank.holidays <- date(uk_bank_holidays$Bank.holidays)
uk_bank_holidays <- select(uk_bank_holidays,c(1,3))
colnames(uk_bank_holidays) <- c('date','is_holiday')
uk_bank_holidays_dates <- unique(uk_bank_holidays$date)

weather_daily_darksky <- read.csv('SML_data/weather_daily_darksky.csv',stringsAsFactors = F,sep = ',')
weather_daily_darksky$date <- date(weather_daily_darksky$sunriseTime)
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
data_path = "SML_data/hhblock_dataset/"
files <- list.files(path = data_path)
all_data <- rep(list(NaN),length(files))

#for (i in 1:2){
for (i in 1:length(files)){
  print(i)
  hh_temp <- read.csv(paste0(data_path,files[i]),stringsAsFactors = F,sep = ',')
  colN <- colnames(hh_temp)
  colN[1] <- 'ID'
  colN[2] <- 'date'
  colnames(hh_temp) <- colN
  hh_temp$daily_total <- rowSums(hh_temp[,3:50])
  hh_temp$date <- date(hh_temp$date)
  hh_temp$year <- year(hh_temp$date)
  hh_temp$month <- month(hh_temp$date)
  hh_temp$day <- day(hh_temp$date)
  hh_temp <- merge(hh_temp, informations_households, by = "ID",all.x = TRUE)
  hh_temp <- merge(hh_temp, weather_daily_darksky, by = "date",all.x = TRUE)
  hh_temp$is_we_hd <- 0
  hh_temp[hh_temp$date %in% uk_bank_holidays_dates,]$is_we_hd=1
  hh_temp[is.weekend(hh_temp$date),]$is_we_hd = 1
  hh_temp$ID <- gsub('MAC', '', hh_temp$ID)
  hh_temp$ID <- as.numeric(hh_temp$ID)
  all_data[[i]] <- hh_temp
}
hh_data<-do.call("rbind", all_data)
save(hh_data,file="SML_data/tidy_data/halfhourly_data.Rda")
#load("SML_data/tidy_data/halfhourly_data.Rda")
