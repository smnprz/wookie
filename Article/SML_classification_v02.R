# LOAD DATA ----
library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)
library(janitor)
library(matrixStats)
library(imputeTS)
library(data.table)
library(psych)
library(plm)
library(magrittr)
library(purrr)
library(Hmisc)
library(Rmisc)
library(factoextra)
library(neuralnet)
library(modelr)
library(dplyr)
library(broom)
library(caret)
library(modelr)
library(nnet)
library(rpart)
library(partykit)
library(e1071)

setwd("D:/OneDrive - CELSIA S.A E.S.P/0 - London/R/091 Energy Data Analysis/wookie")
load("Article/hh_data_no_features.Rda")

print(sum(rowSums(hh_data_ss[,3:50]==0,na.rm = TRUE)>23)/nrow(hh_data_ss))
hh_data <- hh_data_ss[(rowSums(hh_data_ss[,3:50]==0,na.rm = TRUE)<24),]

# GENERATE DATE FRAMES----

create_df_wd_we <- function(df_all,n_month,kWh_T,bol_min_max,periods){
  hh_data <- df_all
  
  #add max and min if required
  hh_data$daily_total <- rowSums(hh_data[,3:(periods+2)])
  if (n_month!=0){
    hh_data <- hh_data[hh_data$month == n_month,]
  }
  hh_data <- hh_data[hh_data$daily_total >= kWh_T,]
  hh_data_temp <- hh_data
  hh_data <- hh_data[,1:(periods+2)]
  hh_data$ACORN_type <- hh_data_temp$ACORN_type
  hh_data$is_we_hd <- hh_data_temp$is_we_hd 
  
  #add max and min if required
  if (bol_min_max){
    hh_data$hh_max_mean <- rowMaxs(as.matrix(hh_data[,3:(periods+2)],na.rm = TRUE))
    #hh_data$hh_min <- rowMins(as.matrix(hh_data[,3:50],na.rm = TRUE))
  }
  
  #Summarize weekday
  hh_data_av_wd <- hh_data[hh_data$is_we_hd==0,] %>%
    group_by(ID,ACORN_type) %>%
    dplyr::summarise_if(is.numeric,mean,na.rm=TRUE)
  
  # #Summarize weekday max
  # hh_data_max_wd <- hh_data[hh_data$is_we_hd==0,] %>%
  #   group_by(ID,ACORN_type) %>%
  #   dplyr::summarise(hh_max = max(hh_max_mean,na.rm = TRUE))
  
  #Summarize weekend
  hh_data_av_we <- hh_data[hh_data$is_we_hd==1,] %>%
    group_by(ID,ACORN_type) %>%
    dplyr::summarise_if(is.numeric,mean,na.rm=TRUE)
  
  # #Summarize weekend max
  # hh_data_max_we <- hh_data[hh_data$is_we_hd==1,] %>%
  #   group_by(ID,ACORN_type) %>%
  #   dplyr::summarise(hh_max = max(hh_max_mean,na.rm = TRUE))

  #Merge weekday and weekend
  hh_data_sum <- merge(hh_data_av_wd,hh_data_av_we,by=c('ID','ACORN_type'))
  # hh_data_sum <- merge(hh_data_sum,hh_data_max_wd,by=c('ID','ACORN_type'))
  # hh_data_sum <- merge(hh_data_sum,hh_data_max_we,by=c('ID','ACORN_type'))
  
  #Delete ID column
  #hh_data_sum <- hh_data_sum[,-1]
  
  return(hh_data_sum)
}

delete_zeros <- function(df_all,n_zeros,periods){
  hh_data <- df_all
  hh_data$count_zeros <- rowSums(hh_data[,3:(periods+2)]==0)
  hh_data <- hh_data[hh_data$count_zeros <= n_zeros]
  hh_data$count_zeros <- NULL
  return(hh_data)
}

create_df_one_group <- function(df_all,str_group){
  df_all$ACORN_type <- as.character(df_all$ACORN_type)
  df_all[df_all$ACORN_type!=str_group,]$ACORN_type <- 'Other'
  df_all[df_all$ACORN_type==str_group,]$ACORN_type <- 'Selection'
  df_all$ACORN_type <- as.factor(df_all$ACORN_type)
  return(df_all)
}

merge_df_months <- function(df_all,n_month_1,n_month_2,kWh_T,bol_min_max,periods){
  df_month_1 <- create_df_wd_we(df_all,n_month_1,kWh_T,bol_min_max,periods)
  df_month_2 <- create_df_wd_we(df_all,n_month_2,kWh_T,bol_min_max,periods)
  df_month <- merge(df_month_1,df_month_2,by = c('ID','ACORN_type'))
  df_month <- df_month[,-1]
  return(df_month)
}

normalize_rows <- function(df_all_norm,periods){
  df_all_norm$max_row <- rowMaxs(data.matrix(df_all_norm[,3:(periods+2)]))
  df_all_norm[,3:(periods+2)] <- sweep(df_all_norm[,3:(periods+2)],df_all_norm[,"max_row"],MARGIN=1,"/")
  return(df_all_norm)
}

aggregate_hour <- function(df_all){
  df_hour <- df_all
  for (h in 1:24){
    print(h)
    df_hour[,h+2] = df_all[,1+2*h]+df_all[,2+2*h]
  }
  df_hour <- df_hour[,c(1:26,51:ncol(df_hour))]
  return(df_hour)
}

hh_data_av_wd <- hh_data[hh_data$is_we_hd==0,] %>%
  group_by(ID,ACORN_type) %>%
  dplyr::summarise_if(is.numeric,mean,na.rm=TRUE)
hh_data_av_wd <- hh_data_av_wd[,c(1:50)]

hh_data_av_we <- hh_data[hh_data$is_we_hd==1,] %>%
  group_by(ID,ACORN_type) %>%
  dplyr::summarise_if(is.numeric,mean,na.rm=TRUE)
hh_data_av_we <- hh_data_av_we[,c(1:50)]

hh_data_av_all <- hh_data %>%
  group_by(ID,ACORN_type) %>%
  dplyr::summarise_if(is.numeric,mean,na.rm=TRUE)
hh_data_av_all <- hh_data_av_all[,c(1:50)]

hh_data_p25_all <- hh_data %>%
  group_by(ID,ACORN_type) %>%
  dplyr::summarise_if(is.numeric,quantile,probs=0.25)
hh_data_p25_all <- hh_data_p25_all[,c(1:50)]

hh_data_p75_all <- hh_data %>%
  group_by(ID,ACORN_type) %>%
  dplyr::summarise_if(is.numeric,quantile,probs=0.75)
hh_data_p75_all <- hh_data_p75_all[,c(1:50)]

hh_data_max_all <- hh_data %>%
  group_by(ID,ACORN_type) %>%
  dplyr::summarise_if(is.numeric,max,na.rm=TRUE)
hh_data_max_all <- hh_data_max_all[,c(1:50)]

hh_data_min_all <- hh_data %>%
  group_by(ID,ACORN_type) %>%
  dplyr::summarise_if(is.numeric,min,na.rm=TRUE)
hh_data_min_all <- hh_data_min_all[,c(1:50)]

hh_data_summ_all <- hh_data
hh_data_summ_all$hh_max <- rowMaxs(as.matrix(hh_data[,3:50],na.rm = TRUE))
hh_data_summ_all$hh_min <- rowMins(as.matrix(hh_data[,3:50],na.rm = TRUE))
#hh_data_summ_all$hh_p75 <- quantile(as.matrix(hh_data[,3:50],na.rm = TRUE,probs=0.75))
#hh_data_summ_all$hh_p75 <- quantile(as.matrix(hh_data[,3:50],na.rm = TRUE,probs=0.75))

df_2_months_ana %>%
  group_by(ACORN_type) %>%
  dplyr::summarise(rat = mean(ratio))

ggplot(df_2_months_ana,aes(y=ratio,color=ACORN_type))+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_boxplot(alpha=0.2) +
  labs(title = "Daily consumption we data", colour = "ACORN type",y = "kW", x = 'hour')+
  ylim(0,3)

# DATA PLOTS -------

hh_data_av_all_long <- melt(data = hh_data_av_we, id.vars = c("ID",'ACORN_type'), measure.vars = colnames(hh_data_av_all)[3:50])
hh_data_av_all_long$hour <- as.character(hh_data_av_all_long$variable)
hh_data_av_all_long$hour <- as.numeric(gsub("^.*?_","",hh_data_av_all_long$hour))
str(hh_data_av_all_long)

hh_data_av_all_long_sum <- hh_data_av_all_long %>%
  group_by(variable,ACORN_type) %>%
  dplyr::summarise(value=mean(value, na.rm = TRUE),hour=mean(hour))

#PLot all IDs average consumption colored by ACORN group
ggplot(hh_data_av_all_long,aes(x=hour, y=value,group=ID,color=ACORN_type))+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_line(alpha=0.2) +
  labs(title = "Daily consumption we data", colour = "ACORN type",y = "kW", x = 'hour')+
  xlim(0, 48)+
  ylim(0,3)

#PLot average of average consumption colored by ACORN group
ggplot(hh_data_av_all_long_sum)+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_line(aes(x=hour, y=value,group=ACORN_type,color=ACORN_type),alpha=0.5) +
  labs(title = "Daily consumption we", colour = "ACORN type",y = "kWh-day", x = 'hour')+
  xlim(0, 48)

hh_data_av_all_long_daily <- hh_data_av_all_long %>%
  group_by(ID,ACORN_type) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE))

#PLot histogram by group
ggplot(hh_data_av_all_long_daily,aes(x=value,color=ACORN_type))+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_density(alpha=0.5, size=1.5) +
  labs(title = "Daily consumption", colour = "ACORN type",x = "kWh-day", y = 'density')+
  xlim(0, 48)

#PLot histogram all data
ggplot(hh_data_av_all_long_daily,aes(x=value))+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_histogram(stat = 'density',alpha=0.5, size=1.5) +
  labs(title = "Daily consumption", colour = "ACORN type",x = "kWh-day", y = 'density')

#hourly consumption cumulative histogram
hh_data_av_all_long_cum <- hh_data_av_all_long[order(-hh_data_av_all_long$value),] 
hh_data_av_all_long_cum$cum_sum <- cumsum(hh_data_av_all_long_cum$value)

ggplot(hh_data_av_all_long_cum,aes(x=value))+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_histogram(aes(y = cumsum(..count..)),alpha=0.5, bins = 100) +
  labs(title = "hourly consumption cumulative histogram", x = "kW", y = 'count')+
  xlim(0,2)

#daily consumption cumulative histogram
hh_data_av_all_long_daily_cum <- hh_data_av_all_long_daily[order(-hh_data_av_all_long_daily$value),] 
hh_data_av_all_long_daily_cum$cum_sum <- cumsum(hh_data_av_all_long_daily_cum$value)

ggplot(hh_data_av_all_long_daily_cum,aes(x=value))+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_histogram(aes(y = cumsum(..count..)),alpha=0.5, bins = 1000) +
  labs(title = "daily consumption cumulative histogram", x = "kWh-day", y = 'count')+
  xlim(0,40)

#CREATE COUNT DATAFRAME
#Affluent,Adversity Comfortable
data_value = hh_data_av_all_long_daily_cum$value
data_value = hh_data_av_all_long_daily_cum[hh_data_av_all_long_daily_cum$ACORN_type=='Comfortable',]$value
bins=c(0:60,1000)*0.5
a = hist(data_value,breaks = bins)
count = data.frame(a$breaks[0:(length(bins)-1)],a$counts)
a

#CREATE COUNT DATAFRAME WITH ALL DAYS
#Affluent,Adversity Comfortable
hh_data_day <- hh_data
hh_data_day$daily_total <- rowSums(hh_data_day[,3:50])

data_value = hh_data_day$daily_total
data_value = hh_data_day[hh_data_av_all_long_daily_cum$ACORN_type=='Comfortable',]$daily_total
bins=c(0:60,1000)*0.5
a = hist(data_value,breaks = bins)
count = data.frame(a$breaks[0:(length(bins)-1)],a$counts)
a

##SCATTER PLOT OF MAX vs DAILY CONSUMPTION
hh_data_av_all_long_daily_max <- hh_data_av_all_long %>%
  group_by(ID,ACORN_type) %>%
  dplyr::summarise(consumption = sum(value, na.rm = TRUE),peak = max(value,na.rm = TRUE))

ggplot(hh_data_av_all_long_daily_max,aes(x=consumption, y=peak,group=ID,color=ACORN_type))+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_point(alpha=0.4) +
  labs(title = "Hourly peak vs. daily consumption", colour = "ACORN type",x = "kWh-day", y = 'peak (kW)')+
  xlim(0,40)+
  ylim(0,1)

#Filter consumption dyas lower than 1 before computing the average
hh_data_av_fil <- hh_data[hh_data$daily_total>=1.5,] %>%
  group_by(ID,ACORN_type) %>%
  dplyr::summarise_if(is.numeric,mean,na.rm=TRUE)

hh_data_av_fil <- hh_data_av_fil[,c(1:50)]
hh_data_av_fil_long <- melt(data = hh_data_av_fil, id.vars = c("ID",'ACORN_type'), measure.vars = colnames(hh_data_av_fil)[3:50])
hh_data_av_fil_long$hour <- as.character(hh_data_av_fil_long$variable)
hh_data_av_fil_long$hour <- as.numeric(gsub("^.*?_","",hh_data_av_fil_long$hour))
str(hh_data_av_fil_long)
ggplot(hh_data_av_fil_long,aes(x=hour, y=value,group=ID,color=ACORN_type))+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_line(alpha=0.2) +
  labs(title = "Average consumption by ID (deleting days <= 1.5)", colour = "ACORN type",y = "kW", x = 'hour')+
  xlim(0, 48)+
  ylim(0,3)

mean(hh_data_av_fil_long$value)

#Monthlt analysis
hh_data_month <- hh_data
hh_data_month$month <- substr(hh_data_month$date, 6, 7)
hh_data_month$month <- as.factor(hh_data_month$month)
hh_data_month$daily_total <- rowSums(hh_data_month[,3:50])
hh_data_av_month <- hh_data_month %>%
  group_by(ID,ACORN_type,month) %>%
  dplyr::summarise_if(is.numeric,mean,na.rm=TRUE)

hh_data_av_month_long <- melt(data = hh_data_av_month, id.vars = c("ID",'ACORN_type','month'), measure.vars = colnames(hh_data_av_month)[52])
#hh_data_av_month_long$hour <- as.character(hh_data_av_month_long$variable)
#hh_data_av_month_long$hour <- as.numeric(gsub("^.*?_","",hh_data_av_month_long$hour))
str(hh_data_av_month_long)
hh_data_av_month_long$variable<- as.numeric(hh_data_av_month_long$variable)

ggplot(hh_data_av_month_long,aes(x=value,color=ACORN_type))+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_density(alpha=0.5, size=0.5) +
  labs(title = "Daily consumption",x = "kWh-day", y = 'density')+
  xlim(0, 48)+
  facet_wrap(~ month,ncol=4)

ggplot(hh_data_av_month_long,aes(x=value,color=ACORN_type))+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_(alpha=0.5, size=0.5) +
  labs(title = "Daily consumption",x = "kWh-day", y = 'density')+
  xlim(0, 48)+
  facet_wrap(~ month,ncol=4)


hh_data_month <- normalize_rows(hh_data)
hh_data_month_av_all_long <- melt(data = hh_data_month, id.vars = c("ID",'ACORN_type','month'), measure.vars = colnames(hh_data_month)[3:50])
hh_data_month_av_all_long$hour <- as.character(hh_data_month_av_all_long$variable)
hh_data_month_av_all_long$hour <- as.numeric(gsub("^.*?_","",hh_data_month_av_all_long$hour))
str(hh_data_month_av_all_long)

hh_data_av_month_all_long_sum <- hh_data_month_av_all_long %>%
  group_by(variable,ACORN_type,month) %>%
  dplyr::summarise(value=mean(value, na.rm = TRUE),hour=mean(hour))

hh_data_av_month_all_long_sum$month <- as.numeric(hh_data_av_month_all_long_sum$month) 

#PLot average of average consumption colored by ACORN group
ggplot(hh_data_av_month_all_long_sum)+#, colour=ACORN_type,group=ACORN_type)) + 
  geom_line(aes(x=hour, y=value,color=ACORN_type),alpha=0.5) +
  labs(title = "Hourly consumption (normalized)", colour = "ACORN type",y = "kWh-day", x = 'hour')+
  xlim(0, 48)+
  facet_wrap(~ month,ncol=4)

# NEURAL NET ------
scale_df <- function(df_all){
  ACORN_type <- df_all[,1:1]
  input_hh <- df_all[,2:ncol(df_all)]
  
  standardize <- function(x){(x-min(x))/(max(x)-min(x))}
  input_hh <- as.data.frame(input_hh)
  input_hh_norm <- as.data.frame(apply(input_hh, 2, standardize))
  input_hh_scale <- as.data.frame(scale(input_hh))
  
  input_hh_nn <- input_hh_scale
  input_hh_nn <- cbind(ACORN_type,input_hh_nn)
  
  return(input_hh_nn)
}

ACORN_class_nnet <- function(df_all,n_neruons){
  input_hh <- df_all
  output_hh <- input_hh[,1:2]
  output_hh$adv <- 0
  output_hh$aff <- 0
  output_hh$com <- 0
  output_hh[output_hh$ACORN_type=="Adversity",]$adv <- 1
  output_hh[output_hh$ACORN_type=="Affluent",]$aff <- 1
  output_hh[output_hh$ACORN_type=="Comfortable",]$com <- 1
  output_hh <- output_hh[,c('ACORN_type','adv','aff','com')]
  
  input_hh <- input_hh[,2:ncol(input_hh)]
  
  standardize <- function(x){(x-min(x))/(max(x)-min(x))}
  input_hh <- as.data.frame(input_hh)
  input_hh_norm <- as.data.frame(apply(input_hh, 2, standardize))
  input_hh_scale <- as.data.frame(scale(input_hh))
  
  input_hh_nn <- input_hh
  
  f <- as.formula(paste("adv + aff + com ~", paste(names(input_hh_nn), collapse = " + ")))
  
  input_hh_nn$adv <- output_hh$adv
  input_hh_nn$aff <- output_hh$aff
  input_hh_nn$com <- output_hh$com
  
  nn <- neuralnet(f,
                  data = input_hh_nn,
                  hidden = n_neruons,
                  rep = 1,
                  linear.output = FALSE,
                  act.fct = 'logistic',
                  threshold = 0.1,
                  #learningrate = 40,
                  lifesign = 'minimal')
  
  pr.nn <- compute(nn, input_hh_nn[, 1:(ncol(input_hh_nn)-3)])
  pr.nn_ <- pr.nn$net.result
  
  original_values <- max.col(input_hh_nn[, (ncol(input_hh_nn)-2):ncol(input_hh_nn)])
  pr.nn_2 <- max.col(pr.nn_)
  print(mean(pr.nn_2 == original_values))
  
  pred <- factor(as.factor(pr.nn_2),c(1,2,3))
  or <- factor(as.factor(original_values),c(1,2,3))
  
  CM<-confusionMatrix(pred,or)
  CMnn <- t(CM$table)
  return(CMnn)
}

ACORN_class_nnet_train <- function(df_all,n_neruons,T){
  input_hh <- df_all
  output_hh <- input_hh[,1:2]
  output_hh$adv <- 0
  output_hh$aff <- 0
  output_hh$com <- 0
  output_hh[output_hh$ACORN_type=="Adversity",]$adv <- 1
  output_hh[output_hh$ACORN_type=="Affluent",]$aff <- 1
  output_hh[output_hh$ACORN_type=="Comfortable",]$com <- 1
  output_hh <- output_hh[,c('ACORN_type','adv','aff','com')]
  
  input_hh <- input_hh[,2:ncol(input_hh)]
  
  input_hh_nn <- input_hh
  
  f <- as.formula(paste("adv + aff + com ~", paste(names(input_hh_nn), collapse = " + ")))
  
  input_hh_nn$adv <- output_hh$adv
  input_hh_nn$aff <- output_hh$aff
  input_hh_nn$com <- output_hh$com
  
  nn <- neuralnet(f,
                  data = input_hh_nn,
                  hidden = n_neruons,
                  rep = 1,
                  linear.output = FALSE,
                  act.fct = 'logistic',
                  threshold = 0.1*T,
                  stepmax = 150000,
                  #learningrate = 40,
                  lifesign = 'full')
  
  return(nn) 
}

ACORN_class_nnet_test <- function(df_all,nn){
  input_hh <- df_all
  output_hh <- input_hh[,1:2]
  output_hh$adv <- 0
  output_hh$aff <- 0
  output_hh$com <- 0
  output_hh[output_hh$ACORN_type=="Adversity",]$adv <- 1
  output_hh[output_hh$ACORN_type=="Affluent",]$aff <- 1
  output_hh[output_hh$ACORN_type=="Comfortable",]$com <- 1
  output_hh <- output_hh[,c('ACORN_type','adv','aff','com')]
  
  input_hh <- input_hh[,2:ncol(input_hh)]
  
  standardize <- function(x){(x-min(x))/(max(x)-min(x))}
  input_hh <- as.data.frame(input_hh)
  input_hh_norm <- as.data.frame(apply(input_hh, 2, standardize))
  input_hh_scale <- as.data.frame(scale(input_hh))
  
  input_hh_nn <- input_hh
  
  input_hh_nn$adv <- output_hh$adv
  input_hh_nn$aff <- output_hh$aff
  input_hh_nn$com <- output_hh$com
  
  pr.nn <- compute(nn, input_hh_nn[, 1:(ncol(input_hh_nn)-3)])
  pr.nn_ <- pr.nn$net.result
  
  original_values <- max.col(input_hh_nn[, (ncol(input_hh_nn)-2):ncol(input_hh_nn)])
  pr.nn_2 <- max.col(pr.nn_)
  print(mean(pr.nn_2 == original_values))
  
  pred <- factor(as.factor(pr.nn_2),c(1,2,3))
  or <- factor(as.factor(original_values),c(1,2,3))
  
  CM<-confusionMatrix(pred,or)
  CMnn <- t(CM$table)
  return(CMnn)
}

ACORN_class_nnet_cv <- function(df_all,n_neurons,T){
  #CROS VALIDATION
  set.seed(500)
  k <- 10
  outs <- NULL
  proportion <- 1-1/k
  
  ACORN_type <- df_all[,1:1]
  input_hh <- df_all[,2:ncol(df_all)]
  
  # standardize <- function(x){(x-min(x))/(max(x)-min(x))}
  # input_hh <- as.data.frame(input_hh)
  # input_hh_norm <- as.data.frame(apply(input_hh, 2, standardize))
  # input_hh_scale <- as.data.frame(scale(input_hh))
  
  input_hh_nn <- input_hh
  input_hh_nn <- cbind(ACORN_type,input_hh_nn)
  #Randomly shuffle the data
  input_hh_nn <- input_hh_nn[sample(nrow(input_hh_nn)),]
  folds <- cut(seq(1,nrow(input_hh_nn)),breaks=k,labels=FALSE)
  for(i in 1:k)
  {
    
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_cv <- input_hh_nn[testIndexes, ]
    train_cv <- input_hh_nn[-testIndexes, ]
    
    bol_run <- TRUE
    T <- 1
    while(bol_run){
      print(bol_run)
      out = tryCatch({
        nn_cv <- ACORN_class_nnet_train(train_cv,n_neurons,T)
        CM <- ACORN_class_nnet_test(test_cv,nn_cv)
        out <- (sum(diag(CM))/sum(CM))
      }, error = function(e) {
        out <- FALSE
      }
      )
      if (out==FALSE){
        T = T + 1
      }else{
        bol_run <- FALSE
      }
    }
    outs[i] <- out
  }
  return(outs)
}

df_nn <- create_df_wd_we(hh_data,12,0,TRUE,48)
df_nn <- df_nn[,-1]
# df_nn <- scale_df(df_nn)

hh_data_hour <- aggregate_hour(hh_data)
df_nn_h <- create_df_wd_we(hh_data_hour,12,0,TRUE,24)
df_nn_h <- df_nn_h[,-1]
# df_nn_h <- scale_df(df_nn_h)

outs_h <- ACORN_class_nnet_cv(df_nn_h,c(5,5))
outs_hh <- ACORN_class_nnet_cv(df_nn,c(5,5))


outs_months_cv <- NULL
for(n in 0:12){
  print(n)
  df_month <- create_df_wd_we(hh_data,n,0,TRUE)
  bol_run <- TRUE
  T <- 1
  while(bol_run){
    outs_cv = tryCatch({
      outs_cv = ACORN_class_nnet_cv(df_month,c(3,3),T)
    },error = function(e) {
      outs_cv = FALSE
      }
    )
    if (outs_cv==FALSE){
      T = T + 1
    }else{
      bol_run <- FALSE
    }
  }
  outs_months_cv[n+1] <- mean(outs_cv)
}

# NEURAL NET FOR TWO GROUPS ONLY------
ACORN_class_nnet_train_2_classes <- function(df_all,n_neruons,T){
  input_hh <- df_all
  output_hh <- input_hh[,1:2]
  output_hh$sel <- 0
  output_hh$oth <- 0
  output_hh[output_hh$ACORN_type=="Selection",]$sel <- 1
  output_hh[output_hh$ACORN_type=="Other",]$oth <- 1
  output_hh <- output_hh[,c('ACORN_type','sel','oth')]
  
  input_hh <- input_hh[,2:ncol(input_hh)]
  
  input_hh_nn <- input_hh
  
  f <- as.formula(paste("sel + oth ~", paste(names(input_hh_nn), collapse = " + ")))
  
  input_hh_nn$sel <- output_hh$sel
  input_hh_nn$oth <- output_hh$oth
  
  nn <- neuralnet(f,
                  data = input_hh_nn,
                  hidden = n_neruons,
                  rep = 1,
                  linear.output = FALSE,
                  act.fct = 'logistic',
                  threshold = 0.1*T,
                  #learningrate = 40,
                  stepmax = 150000,
                  lifesign = 'full')
  
  return(nn) 
}

ACORN_class_nnet_test_2_classes <- function(df_all,nn){
  input_hh <- df_all
  output_hh <- input_hh[,1:2]
  output_hh$sel <- 0
  output_hh$oth <- 0
  output_hh[output_hh$ACORN_type=="Selection",]$sel <- 1
  output_hh[output_hh$ACORN_type=="Other",]$oth <- 1
  output_hh <- output_hh[,c('ACORN_type','sel','oth')]
  
  input_hh <- input_hh[,2:ncol(input_hh)]
  
  input_hh_nn <- input_hh
  
  pr.nn <- compute(nn, input_hh_nn)
  pr.nn_ <- pr.nn$net.result
  
  input_hh_nn$sel <- output_hh$sel
  input_hh_nn$oth <- output_hh$oth
  
  original_values <- max.col(input_hh_nn[, (ncol(input_hh_nn)-1):ncol(input_hh_nn)])
  pr.nn_2 <- max.col(pr.nn_)
  print(mean(pr.nn_2 == original_values))
  
  pred <- factor(as.factor(pr.nn_2),c(1,2))
  or <- factor(as.factor(original_values),c(1,2))
  
  CM<-confusionMatrix(pred,or)
  CMnn <- t(CM$table)
  #return(sum(diag(CMnn))/sum(CMnn))
  return(CMnn)
}

ACORN_class_nnet_cv_2_classes <- function(df_all,n_neurons){
  #CROS VALIDATION
  set.seed(500)
  k <- 10
  outs <- NULL
  proportion <- 1-1/k
  
  ACORN_type <- df_all[,1:1]
  input_hh <- df_all[,2:ncol(df_all)]
  
  input_hh_nn <- input_hh
  input_hh_nn <- cbind(ACORN_type,input_hh_nn)
  #Randomly shuffle the data
  input_hh_nn <- input_hh_nn[sample(nrow(input_hh_nn)),]
  folds <- cut(seq(1,nrow(input_hh_nn)),breaks=k,labels=FALSE)
  for(i in 1:k)
  {
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_cv <- input_hh_nn[testIndexes, ]
    train_cv <- input_hh_nn[-testIndexes, ]
    
    bol_run <- TRUE
    T <- 1
    while(bol_run){
      print(bol_run)
      out = tryCatch({
        nn_cv <- ACORN_class_nnet_train_2_classes(train_cv,n_neurons,T)
        CM <- ACORN_class_nnet_test_2_classes(test_cv,nn_cv)
        out <- (sum(diag(CM))/sum(CM))
      }, error = function(e) {
        out <- FALSE
      }
      )
      if (out==FALSE){
        T = T + 1
      }else{
        bol_run <- FALSE
      }
    }
    outs[i] <- out
  }
  
  return(outs)
}

#hh_adverse <- create_df_one_group(hh_data,'Adversity')
#df_month <- create_df_wd_we(hh_adverse,0,0,TRUE)
#df_month_scale <- scale_df(df_month)

hh_adverse <- create_df_one_group(hh_data,'Adversity')

df_2_months <- merge_df_months(hh_adverse,1,7,0,TRUE)
df_2_months_scale <- scale_df(df_2_months)

df_month <- create_df_wd_we(hh_adverse,12,0,TRUE)
df_month <- df_month[-1]
df_month_scale <- scale_df(df_month)

outs_cv_2 = ACORN_class_nnet_cv_2_classes(df_2_months_scale,c(5,5))
outs_cv_1 = ACORN_class_nnet_cv_2_classes(df_month_scale,c(5,5))

df_all <- df_2_months_scale

df_nn <- df_month_scale
outs_cv = ACORN_class_nnet_cv_2_classes(df_nn,c(5,5))
nn <- ACORN_class_nnet_train_2_classes(df_nn,c(5,5),2)
CM <- ACORN_class_nnet_test_2_classes(df_nn,nn)


df_2_months_ana <- df_2_months
df_2_months_ana$tot_daily <- rowSums(df_2_months_ana[,2:49])
df_2_months_ana$ratio <- df_2_months_ana$hh_max.x.x/df_2_months_ana$hh_max.x.y
ggplot(data=df_2_months_ana,aes(x=hh_max.x.x,y=hh_max.x.y,color=ACORN_type))+
  geom_point()+
  xlim(0,6)+
  ylim(0,6)

ggplot(data=df_2_months_ana,aes(x=hh_max.x.x,y=ratio,color=ACORN_type))+
  geom_point()+
  xlim(0,7)+
  ylim(0,10)



# DECISICION TREES ----

get_df_dt <- function(df,str_name,periods){
  df$tot_day <- rowSums(df[,3:(periods+2)])
  df$hh_max <- rowMaxs(as.matrix(df[,3:(periods+2)]))
  hh_dt_all <- df %>%
    group_by(ID,ACORN_type) %>%
    dplyr::summarise(tot_day=mean(tot_day),hh_max=mean(hh_max))
  ID <- hh_dt_all$ID
  ACORN_type <- hh_dt_all$ACORN_type
  df2 <- hh_dt_all[,3:ncol(hh_dt_all)]
  colnames(df2) <- paste(colnames(df2), str_name, sep = "_")
  hh_dt_all <- cbind(ACORN_type,df2)
  hh_dt_all <- cbind(ID,hh_dt_all)
  return(hh_dt_all)
  #hh_dt_all <- hh_dt_all[,2:ncol(hh_dt)]
}

#hh_dt_all <- merge(get_df_dt(hh_dt),get_df_dt(hh_dt[hh_dt$is_we_hd==0,]),by=c('ID','ACORN_type'))
#hh_dt_all <- merge(hh_dt_all,get_df_dt(hh_dt[hh_dt$is_we_hd==1,]),by=c('ID','ACORN_type'))
hh_dt <- hh_data_hour
per <- 24
hh_dt_all_1 <- merge(get_df_dt(hh_dt[hh_dt$is_we_hd==0,],'weekday',per),get_df_dt(hh_dt[hh_dt$is_we_hd==1,],'weekend',per),by=c('ID','ACORN_type'))
hh_dt_all_2 <- merge(get_df_dt(hh_dt[hh_dt$month==12,],'winter',per),get_df_dt(hh_dt[hh_dt$month==7,],'summer',per),by=c('ID','ACORN_type'))
hh_dt_all <- merge(hh_dt_all_1,hh_dt_all_2,by=c('ID','ACORN_type'))
#hh_dt_all$ratio_peak_ws <- hh_dt_all$hh_max_winter/hh_dt_all$hh_max_summer
hh_dt_all <- hh_dt_all[,2:ncol(hh_dt_all)]

# input_hh <- hh_dt_all
# output_hh <- input_hh[,1:2]
# output_hh$sel <- 0
# output_hh[output_hh$ACORN_type==sel_type,]$sel <- 1
# output_hh$oth <- 1 - output_hh$sel
# output_hh <- output_hh[,c('ACORN_type','sel','oth')]
# 
# input_hh <- input_hh[,2:ncol(input_hh)]
# 
# input_hh_nn <- input_hh
# 
# f <- as.formula(paste("sel ~", paste(names(input_hh_nn), collapse = " + ")))
# 
# #input_hh_nn$sel <- output_hh$sel
# #input_hh_nn$oth <- output_hh$oth
# sel <- output_hh$sel
# input_hh_nn <- cbind(sel,input_hh_nn)
# 
# fit <- rpart(f,method="class",data=input_hh_nn)
# fit <- rpart(sel ~ .,method="class",data=input_hh_nn)
# bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
# tree.pruned <- prune(fit, cp = 0.01000000000)
# conf.matrix <- table(input_hh_nn$sel, predict(tree.pruned,type="class"))
# plot(tree.pruned)
# text(tree.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
# 
# printcp(fit) # display the results 
# plotcp(fit) # visualize cross-validation results 
# summary(fit) # detailed summary of splits
# 

sel_type <- 'Adversity'
hh_dt_all$ACORN_type <- as.character(hh_dt_all$ACORN_type)
hh_dt_all[hh_dt_all$ACORN_type==sel_type,]$ACORN_type <- 'Selection'
hh_dt_all[hh_dt_all$ACORN_type!='Selection',]$ACORN_type <- 'Other'
hh_dt_all$ACORN_type <- as.factor(hh_dt_all$ACORN_type)

hh_tree <- hh_dt_all
#hh_tree <- merge_df_months(hh_data,1,7,0,TRUE)
fit_2 <- ctree(ACORN_type ~ .,data = hh_tree)
plot(fit_2)
treepre <- predict(fit_2,newdata = hh_tree,type='response')
confusionMatrix(treepre,hh_tree$ACORN_type)
t <- table(treepre,hh_tree$ACORN_type)
sum(diag(t))/(sum(t))

tree_CV <- function(df_all){
  #CROS VALIDATION
  set.seed(500)
  k <- 10
  outs <- NULL
  proportion <- 1-1/k
  
  input_hh_nn <- df_all
  #Randomly shuffle the data
  input_hh_nn <- input_hh_nn[sample(nrow(input_hh_nn)),]
  folds <- cut(seq(1,nrow(input_hh_nn)),breaks=k,labels=FALSE)
  for(i in 1:k)
  {
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_cv <- input_hh_nn[testIndexes, ]
    train_cv <- input_hh_nn[-testIndexes, ]
    fit <- ctree(ACORN_type ~ .,data = train_cv)
    treepre <- predict(fit,newdata = test_cv,type='response')
    t <- table(treepre,test_cv$ACORN_type)
    outs[i] <- sum(diag(t))/(sum(t))
  }
  return(outs)
}

outs_tree <- tree_CV(hh_tree)


# NAIVE BAYES CLASSIFICATION ----

bayes_CV <- function(df_all){
  #CROS VALIDATION
  set.seed(500)
  k <- 10
  outs <- NULL
  proportion <- 1-1/k

  input_hh_nn <- df_all
  #Randomly shuffle the data
  input_hh_nn <- input_hh_nn[sample(nrow(input_hh_nn)),]
  folds <- cut(seq(1,nrow(input_hh_nn)),breaks=k,labels=FALSE)
  for(i in 1:k)
  {
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_cv <- input_hh_nn[testIndexes, ]
    train_cv <- input_hh_nn[-testIndexes, ]
    Naive_Bayes_Model=naiveBayes(ACORN_type ~., data=train_cv)
    NB_Predictions=predict(Naive_Bayes_Model,test_cv)
    t <- table(NB_Predictions,test_cv$ACORN_type)
    outs[i] <- sum(diag(t))/(sum(t))
  }
  return(outs)
}

df_nn <- create_df_wd_we(hh_data,12,0,TRUE,48)
df_nn <- df_nn[,-1]
# df_nn <- scale_df(df_nn)

hh_data_hour <- aggregate_hour(hh_data)
df_nn_h <- create_df_wd_we(hh_data_hour,12,0,TRUE,24)
df_nn_h <- df_nn_h[,-1]

out_BC_hh <- bayes_CV(df_nn)
out_BC_h <- bayes_CV(df_nn_h)

Naive_Bayes_Model=naiveBayes(ACORN_type ~., data=df_nn_h)
NB_Predictions=predict(Naive_Bayes_Model,df_nn_h)
t <- table(NB_Predictions,df_nn_h$ACORN_type)
print(sum(diag(t))/(sum(t)))
t

#df_nn_h_1 <- create_df_one_group(df_nn_h,'Adversity')

# LOGISTIC REGRESSION -----

#Select relevant features
test <- multinom(ACORN_type ~ ., data = df_nn_h)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

f<-as.matrix((colSums(p<=0.05)>=1))
f<-colnames(p)[f]
f<- f[2:length(f)]
f <- as.formula(paste("ACORN_type ~", paste(f,collapse = '+')))

test <- multinom(f, data = df_nn_h)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
f<-as.matrix((colSums(p<=0.05)>=1))
f<-colnames(p)[f]
f<- f[2:length(f)]
f <- as.formula(paste("ACORN_type ~", paste(f,collapse = '+')))

test <- multinom(f, data = df_nn_h)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

pred <- predict(test, newdata = df_nn_h, "probs")
pred_class <- max.col(pred)
pred_class <- colnames(pred)[pred_class]
t <- table(pred_class,df_nn_h$ACORN_type)
print(sum(diag(t))/(sum(t)))
t

f_opt <- f

#Cross validate

log_reg_CV <- function(df_all,f_opt){
  #CROS VALIDATION
  set.seed(500)
  k <- 10
  outs <- NULL
  proportion <- 1-1/k
  
  input_hh_nn <- df_all
  #Randomly shuffle the data
  input_hh_nn <- input_hh_nn[sample(nrow(input_hh_nn)),]
  folds <- cut(seq(1,nrow(input_hh_nn)),breaks=k,labels=FALSE)
  for(i in 1:k)
  {
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_cv <- input_hh_nn[testIndexes, ]
    train_cv <- input_hh_nn[-testIndexes, ]
    test <- multinom(f_opt, data = train_cv)
    pred <- predict(test, newdata = test_cv, "probs")
    pred_class <- max.col(pred)
    pred_class <- colnames(pred)[pred_class]
    t <- table(pred_class,test_cv$ACORN_type)
    outs[i] <- sum(diag(t))/(sum(t))
  }
  return(outs)
}

out_log <- log_reg_CV(df_nn_h,f_opt)


# OPTIMAL NN 1 LAYER -----

nnet_one_ACORN <- function(df_input,n_hidden){
  #input <- hh_data_av_all
  #n_hidden <- 19
  
  input <- df_input
  class_output <- class.ind(input$ACORN_type)
  
  input_hh <- input[,3:ncol(input)]
  input_hh <- as.data.frame(input_hh)
  input_hh_scale <- as.data.frame(scale(input_hh))
  input_hh_nn <- input_hh_scale
  
  ANN = nnet(input_hh_nn, class_output, size=n_hidden, maxit = 1000,softmax=TRUE,MaxNWts = 1e6)
  
  t = table(predict(ANN, input_hh_scale, type="class"),input$ACORN_type)
  print(sum(diag(t))/sum(t))
  
  return(t)
}

t_av <- nnet_one_ACORN(hh_data_av_all,9)
nnet_one_ACORN_cv(hh_data_av_all,5)
plot(1-av_data_results$out_neu_all)
plot(1-av_data_results$out_neu_cv)

df_all <- list(hh_data_av_all,hh_data_av_we,hh_data_av_wd,hh_data_p25_all,hh_data_p75_all,hh_data_max_all,hh_data_min_all) %>% reduce(left_join, by = c("ID",'ACORN_type'))
t_all <- nnet_one_ACORN(df_all,3)
plot(1-all_data_results$out_neu_all)
plot(1-all_data_results$out_neu_cv)

nnet_one_ACORN_cv(df_all,9)


out_neu_cv <- NULL
out_neu_all <- NULL
for(neur in 1:20)
{
  outs_cv <- nnet_one_ACORN_cv(df_all,neur)
  out_neu_cv[neur] <- mean(outs_cv)
  
  outs_all <- nnet_one_ACORN(df_all,neur)
  out_neu_all[neur] <- sum(diag(outs_all))/sum(outs_all)
}
plot(1-out_neu_cv)
plot(1-out_neu_all)


nnet_one_ACORN_cv <- function(df_input,n_hidden){
  #input_hh_nn <- df_input
  #Randomly shuffle the data
  df_input <- df_input[sample(nrow(df_input)),]
  #Transform Dataset
  output_nn_class <- df_input$ACORN_type
  output_nn <- class.ind(output_nn_class)
  input_hh <- df_input[,3:ncol(df_input)]
  input_hh <- as.data.frame(input_hh)
  input_hh_scale <- as.data.frame(scale(input_hh))
  input_nn <- input_hh_scale
  
  #CROS VALIDATION SETUP
  set.seed(500)
  k <- 10
  outs <- NULL
  t <- NULL
  proportion <- 1-1/k
  
  folds <- cut(seq(1,nrow(input_hh_nn)),breaks=k,labels=FALSE)
  for(i in 1:k)
  {
    testIndexes <- which(folds==i,arr.ind=TRUE)
    
    test_cv_input <- input_nn[testIndexes, ]
    train_cv_input <- input_nn[-testIndexes, ]
    test_cv_output <- output_nn[testIndexes, ]
    train_cv_output <- output_nn[-testIndexes, ]
    test_cv_output_class <- output_nn_class[testIndexes]
    train_cv_output_classs <- output_nn_class[-testIndexes]
    
    ANN = nnet(train_cv_input, train_cv_output, size=n_hidden, maxit = 500,softmax=TRUE,MaxNWts = 1e6)
    
    t = table(predict(ANN, test_cv_input, type="class"),test_cv_output_class)
    pre <- sum(diag(t))/sum(t)
    print(pre)
    outs[i] <- pre
  }
  return(outs)
}

