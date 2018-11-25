

# LOAD LIBRARIES ----
library(tidyverse)
library(data.table)
library(psych)
library(plm)
library(magrittr)
library(purrr)
library(Hmisc)
library(ggplot2)
library(Rmisc)
library(lubridate)

# LOAD DATA ----
load("../Data/SML_data/tidy_data/halfhourly_data.Rda")

# REMOVE MISSING VALUES
hh_data_or <- hh_data
hh_data <- hh_data_or[!is.na(hh_data_or$daily_total),]

# TREAT ID, YEAR, MONTH, TARIFF TYPE, ACORN GROUP, ACORN TYPE AND WEEKEND AS FACTOR 
hh_data$ID <- factor(hh_data$ID)
hh_data$year <- factor(hh_data$year)
hh_data$month <- factor(hh_data$month)
hh_data$day <- factor(hh_data$day)
hh_data$tariff_type <- factor(hh_data$tariff_type)
hh_data$ACORN_group <- factor(hh_data$ACORN_group)
hh_data$ACORN_type <- factor(hh_data$ACORN_type)
hh_data$is_we_hd <- factor(hh_data$is_we_hd)

#REMOVING IDS WITH WRONG LABEL IN ACORN_TYPE
hh_data %>%
  group_by(ACORN_type) %>%
  summarise(daily_consumption = mean(daily_total), sd_daily_consuption = sd(daily_total), sd_mean = sd(daily_total)/mean(daily_total), n = n())

nrow(hh_data[hh_data$ACORN_type=="ACORN-U" | hh_data$ACORN_type=="ACORN-" ,])
ids_nan <- unique(hh_data[hh_data$ACORN_type=="ACORN-U" | hh_data$ACORN_type=="ACORN-",]$ID)
length(ids_nan)
hh_data <- hh_data[!(hh_data$ID %in% ids_nan),]
print(-nrow(hh_data)+nrow(hh_data_or))

#CHECKING ZERO VALUES ON DAILY TOTAL, NOT DELETED ENTRIES
nrow(hh_data[hh_data$daily_total==0,])
length(unique(hh_data[hh_data$daily_total==0,]$ID))/length(unique(hh_data$ID))

# SELECT A SUBSET OF DATA SET OF IDS FOR ANALYSIS (TEMPORARY)----
ids <-  unique(hh_data$ID)
rand <- floor(runif(floor(length(ids)/10), min=0, max=length(ids)))
ids_n <- ids[rand]
hh_data <- hh_data[hh_data$ID %in% ids_n,]

# CREATE USEFULL VARIABLES FOR ANALYSIS----

hh_data$year_month <- paste(as.character(hh_data$year),as.character(hh_data$month),sep = '-')
hh_data$year_month <- factor(hh_data$year_month)

#proportion of energy used in hour x versus daily total
hh_data$phh_0 <- hh_data$hh_0/hh_data$daily_total
hh_data$phh_1 <- hh_data$hh_1/hh_data$daily_total
hh_data$phh_2 <- hh_data$hh_2/hh_data$daily_total
hh_data$phh_3 <- hh_data$hh_3/hh_data$daily_total
hh_data$phh_4 <- hh_data$hh_4/hh_data$daily_total
hh_data$phh_5 <- hh_data$hh_5/hh_data$daily_total
hh_data$phh_6 <- hh_data$hh_6/hh_data$daily_total
hh_data$phh_7 <- hh_data$hh_7/hh_data$daily_total
hh_data$phh_8 <- hh_data$hh_8/hh_data$daily_total
hh_data$phh_9 <- hh_data$hh_9/hh_data$daily_total
hh_data$phh_10 <- hh_data$hh_10/hh_data$daily_total
hh_data$phh_11 <- hh_data$hh_11/hh_data$daily_total
hh_data$phh_12 <- hh_data$hh_12/hh_data$daily_total
hh_data$phh_13 <- hh_data$hh_13/hh_data$daily_total
hh_data$phh_14 <- hh_data$hh_14/hh_data$daily_total
hh_data$phh_15 <- hh_data$hh_15/hh_data$daily_total
hh_data$phh_16 <- hh_data$hh_16/hh_data$daily_total
hh_data$phh_17 <- hh_data$hh_17/hh_data$daily_total
hh_data$phh_18 <- hh_data$hh_18/hh_data$daily_total
hh_data$phh_19 <- hh_data$hh_19/hh_data$daily_total
hh_data$phh_20 <- hh_data$hh_20/hh_data$daily_total
hh_data$phh_21 <- hh_data$hh_21/hh_data$daily_total
hh_data$phh_22 <- hh_data$hh_22/hh_data$daily_total
hh_data$phh_23 <- hh_data$hh_23/hh_data$daily_total
hh_data$phh_24 <- hh_data$hh_24/hh_data$daily_total
hh_data$phh_25 <- hh_data$hh_25/hh_data$daily_total
hh_data$phh_26 <- hh_data$hh_26/hh_data$daily_total
hh_data$phh_27 <- hh_data$hh_27/hh_data$daily_total
hh_data$phh_28 <- hh_data$hh_28/hh_data$daily_total
hh_data$phh_29 <- hh_data$hh_29/hh_data$daily_total
hh_data$phh_30 <- hh_data$hh_30/hh_data$daily_total
hh_data$phh_31 <- hh_data$hh_31/hh_data$daily_total
hh_data$phh_32 <- hh_data$hh_32/hh_data$daily_total
hh_data$phh_33 <- hh_data$hh_33/hh_data$daily_total
hh_data$phh_34 <- hh_data$hh_34/hh_data$daily_total
hh_data$phh_35 <- hh_data$hh_35/hh_data$daily_total
hh_data$phh_36 <- hh_data$hh_36/hh_data$daily_total
hh_data$phh_37 <- hh_data$hh_37/hh_data$daily_total
hh_data$phh_38 <- hh_data$hh_38/hh_data$daily_total
hh_data$phh_39 <- hh_data$hh_39/hh_data$daily_total
hh_data$phh_40 <- hh_data$hh_40/hh_data$daily_total
hh_data$phh_41 <- hh_data$hh_41/hh_data$daily_total
hh_data$phh_42 <- hh_data$hh_42/hh_data$daily_total
hh_data$phh_43 <- hh_data$hh_43/hh_data$daily_total
hh_data$phh_44 <- hh_data$hh_44/hh_data$daily_total
hh_data$phh_45 <- hh_data$hh_45/hh_data$daily_total
hh_data$phh_46 <- hh_data$hh_46/hh_data$daily_total
hh_data$phh_47 <- hh_data$hh_47/hh_data$daily_total

#NIGTH from 6am to 6pm
hh_data$night_total <- hh_data$hh_0 + hh_data$hh_1 + hh_data$hh_2 + hh_data$hh_3 + hh_data$hh_4 + hh_data$hh_5 + hh_data$hh_6 + hh_data$hh_7 + hh_data$hh_8 + hh_data$hh_9 + hh_data$hh_10 + hh_data$hh_11 +
  + hh_data$hh_36 + hh_data$hh_37 + hh_data$hh_38 + hh_data$hh_39 + hh_data$hh_40 + hh_data$hh_41 + hh_data$hh_42 + hh_data$hh_43 + hh_data$hh_44 + hh_data$hh_45 + hh_data$hh_46 + hh_data$hh_47

hh_data$day_total <- hh_data$daily_total-hh_data$night_total

hh_data$dn_ratio <- hh_data$day_total/hh_data$night_total
hh_data$nt_ratio <- hh_data$night_total/hh_data$daily_total
hh_data$dt_ratio <- hh_data$day_total/hh_data$daily_total

#Non-PEAK hours from 23:00 to 7:00 https://customerservices.npower.com/app/answers/detail/a_id/179/~/what-are-the-economy-7-peak-and-off-peak-periods%3F
hh_data$off_peak_total <- hh_data$hh_0 + hh_data$hh_1 + hh_data$hh_2 + hh_data$hh_3 + hh_data$hh_4 + hh_data$hh_5 + hh_data$hh_6 + hh_data$hh_7 + hh_data$hh_8 + hh_data$hh_9 + hh_data$hh_10 + hh_data$hh_11 + hh_data$hh_12 + hh_data$hh_13+
  hh_data$hh_46 + hh_data$hh_47
#PEAK hous from 16:00 to 19:00 https://www.theguardian.com/money/2017/jan/07/night-time-use-electrical-appliances-lower-bills-fire-risk
hh_data$peak_total <- hh_data$hh_32 + hh_data$hh_33 + hh_data$hh_34 + hh_data$hh_35 + hh_data$hh_36 + hh_data$hh_37

hh_data$po_ratio <- hh_data$peak_total/hh_data$off_peak_total
hh_data$pt_ratio <- hh_data$peak_total/hh_data$daily_total
hh_data$ot_ratio <- hh_data$off_peak_total/hh_data$daily_total

hh_data[is.infinite(hh_data$dn_ratio),]$dn_ratio <- NaN 
hh_data[is.infinite(hh_data$dt_ratio),]$dt_ratio <- NaN 
hh_data[is.infinite(hh_data$nt_ratio),]$nt_ratio <- NaN 
hh_data[is.infinite(hh_data$pt_ratio),]$pt_ratio <- NaN 
hh_data[is.infinite(hh_data$ot_ratio),]$ot_ratio <- NaN 
hh_data[is.infinite(hh_data$po_ratio),]$po_ratio <- NaN 

#CREATE A NUMBER VARIABLE FOR ACORN_TYPE

hh_data[hh_data$ACORN_type=="Adversity",]$ACORN_TYPE_ID <- 0
hh_data[hh_data$ACORN_type=="Affluent",]$ACORN_TYPE_ID <- 1
hh_data[hh_data$ACORN_type=="Comfortable",]$ACORN_TYPE_ID <- 2

# SUMMARIZE THE DATASET----
hh_data %>%
  group_by(ACORN_type) %>%
  dplyr::summarise(daily_consumption = mean(daily_total), sd_daily_consuption = sd(daily_total), sd_mean = sd(daily_total)/mean(daily_total), n = n())

hh_data %>%
  group_by(ACORN_group) %>%
  dplyr::summarise(daily_consumption = mean(daily_total), sd_daily_consuption = sd(daily_total), sd_mean = sd(daily_total)/mean(daily_total), n = n())

hh_data_ID <- hh_data %>%
  group_by(ID) %>%
  dplyr::summarise(m_daily_total = mean(daily_total, na.rm = TRUE), sd_daily_total = sd(daily_total, na.rm = TRUE),
                   dn_ratio = mean(dn_ratio, na.rm = TRUE),dt_ratio = mean(dt_ratio, na.rm = TRUE), 
                   nt_ratio = mean(nt_ratio, na.rm = TRUE),po_ratio=mean(po_ratio, na.rm = TRUE),
                   pt_ratio = mean(pt_ratio, na.rm = TRUE),ot_ratio = mean(ot_ratio, na.rm = TRUE),
                   phh_0 = mean(phh_0, na.rm = TRUE), phh_1 = mean(phh_1, na.rm = TRUE), phh_2 = mean(phh_2, na.rm = TRUE),
                   phh_3 = mean(phh_3, na.rm = TRUE), phh_4 = mean(phh_4, na.rm = TRUE), phh_5 = mean(phh_5, na.rm = TRUE),
                   phh_6 = mean(phh_6, na.rm = TRUE), phh_7 = mean(phh_7, na.rm = TRUE), phh_8 = mean(phh_8, na.rm = TRUE),
                   phh_9 = mean(phh_10, na.rm = TRUE), phh_10 = mean(phh_10, na.rm = TRUE), phh_11 = mean(phh_11, na.rm = TRUE),
                   phh_12 = mean(phh_12, na.rm = TRUE), phh_13 = mean(phh_13, na.rm = TRUE), phh_14 = mean(phh_14, na.rm = TRUE),
                   phh_15 = mean(phh_15, na.rm = TRUE), phh_16 = mean(phh_16, na.rm = TRUE), phh_17 = mean(phh_17, na.rm = TRUE),
                   phh_18 = mean(phh_18, na.rm = TRUE), phh_19 = mean(phh_19, na.rm = TRUE), phh_20 = mean(phh_20, na.rm = TRUE),
                   phh_21 = mean(phh_21, na.rm = TRUE), phh_22 = mean(phh_22, na.rm = TRUE), phh_23 = mean(phh_23, na.rm = TRUE),
                   phh_24 = mean(phh_24, na.rm = TRUE), phh_25 = mean(phh_25, na.rm = TRUE), phh_26 = mean(phh_26, na.rm = TRUE),
                   phh_27 = mean(phh_27, na.rm = TRUE), phh_28 = mean(phh_28, na.rm = TRUE), phh_29 = mean(phh_29, na.rm = TRUE),
                   phh_30 = mean(phh_30, na.rm = TRUE), phh_31 = mean(phh_31, na.rm = TRUE), phh_32 = mean(phh_32, na.rm = TRUE),
                   phh_33 = mean(phh_33, na.rm = TRUE), phh_34 = mean(phh_34, na.rm = TRUE), phh_35 = mean(phh_35, na.rm = TRUE),
                   phh_36 = mean(phh_36, na.rm = TRUE), phh_37 = mean(phh_37, na.rm = TRUE), phh_38 = mean(phh_38, na.rm = TRUE),
                   phh_39 = mean(phh_39, na.rm = TRUE), phh_40 = mean(phh_40, na.rm = TRUE), phh_41 = mean(phh_41, na.rm = TRUE),
                   phh_42 = mean(phh_42, na.rm = TRUE), phh_43 = mean(phh_43, na.rm = TRUE), phh_44 = mean(phh_44, na.rm = TRUE),
                   phh_45 = mean(phh_45, na.rm = TRUE), phh_46 = mean(phh_46, na.rm = TRUE), phh_47 = mean(phh_47, na.rm = TRUE),
                   hh_0 = mean(hh_0, na.rm = TRUE), hh_1 = mean(hh_1, na.rm = TRUE), hh_2 = mean(hh_2, na.rm = TRUE),
                   hh_3 = mean(hh_3, na.rm = TRUE), hh_4 = mean(hh_4, na.rm = TRUE), hh_5 = mean(hh_5, na.rm = TRUE),
                   hh_6 = mean(hh_6, na.rm = TRUE), hh_7 = mean(hh_7, na.rm = TRUE), hh_8 = mean(hh_8, na.rm = TRUE),
                   hh_9 = mean(hh_10, na.rm = TRUE), hh_10 = mean(hh_10, na.rm = TRUE), hh_11 = mean(hh_11, na.rm = TRUE),
                   hh_12 = mean(hh_12, na.rm = TRUE), hh_13 = mean(hh_13, na.rm = TRUE), hh_14 = mean(hh_14, na.rm = TRUE),
                   hh_15 = mean(hh_15, na.rm = TRUE), hh_16 = mean(hh_16, na.rm = TRUE), hh_17 = mean(hh_17, na.rm = TRUE),
                   hh_18 = mean(hh_18, na.rm = TRUE), hh_19 = mean(hh_19, na.rm = TRUE), hh_20 = mean(hh_20, na.rm = TRUE),
                   hh_21 = mean(hh_21, na.rm = TRUE), hh_22 = mean(hh_22, na.rm = TRUE), hh_23 = mean(hh_23, na.rm = TRUE),
                   hh_24 = mean(hh_24, na.rm = TRUE), hh_25 = mean(hh_25, na.rm = TRUE), hh_26 = mean(hh_26, na.rm = TRUE),
                   hh_27 = mean(hh_27, na.rm = TRUE), hh_28 = mean(hh_28, na.rm = TRUE), hh_29 = mean(hh_29, na.rm = TRUE),
                   hh_30 = mean(hh_30, na.rm = TRUE), hh_31 = mean(hh_31, na.rm = TRUE), hh_32 = mean(hh_32, na.rm = TRUE),
                   hh_33 = mean(hh_33, na.rm = TRUE), hh_34 = mean(hh_34, na.rm = TRUE), hh_35 = mean(hh_35, na.rm = TRUE),
                   hh_36 = mean(hh_36, na.rm = TRUE), hh_37 = mean(hh_37, na.rm = TRUE), hh_38 = mean(hh_38, na.rm = TRUE),
                   hh_39 = mean(hh_39, na.rm = TRUE), hh_40 = mean(hh_40, na.rm = TRUE), hh_41 = mean(hh_41, na.rm = TRUE),
                   hh_42 = mean(hh_42, na.rm = TRUE), hh_43 = mean(hh_43, na.rm = TRUE), hh_44 = mean(hh_44, na.rm = TRUE),
                   hh_45 = mean(hh_45, na.rm = TRUE), hh_46 = mean(hh_46, na.rm = TRUE), hh_47 = mean(hh_47, na.rm = TRUE),
                   n = n())
temp <- hh_data[hh_data$ID==6,]
mean(temp$daily_total)
sd(temp$daily_total)
mean(temp$po_ratio,na.rm = TRUE)

# CREATE AGGREGATE GRAPHS----

hh_data_sum1 <- summarySE(hh_data, measurevar="daily_total", groupvars=c("month","ACORN_type"))
ggplot(hh_data_sum1, aes(x=month, y=daily_total, colour=ACORN_type,group=ACORN_type)) + 
  #geom_errorbar(aes(ymin=daily_total-sd, ymax=daily_total+sd), width=.1, position=pd) +
  geom_line() +
  geom_point() +
  labs(title = "Average daily consumption", colour = "ACORN type",y = "kWh-day")

hh_data_sum2 <- summarySE(hh_data, measurevar="daily_total", groupvars=c("year_month","ACORN_type"))
ggplot(hh_data_sum2, aes(x=year_month, y=daily_total, colour=ACORN_type,group=ACORN_type)) + 
  #geom_errorbar(aes(ymin=daily_total-sd, ymax=daily_total+sd), width=.1, position=pd) +
  geom_line() +
  geom_point() +
  labs(title = "Average daily consumption", colour = "ACORN type",y = "kWh-day", x = 'date')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=hh_data)+
  geom_boxplot(aes(x=year_month,y=daily_total,fill=ACORN_type),outlier.size=-1)+
  coord_cartesian(ylim=c(0,30))+
  labs(title = "Average daily consumption", colour = "ACORN type",y = "kWh-day", x = 'date')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=hh_data)+
  geom_boxplot(aes(x=month,y=daily_total,fill=ACORN_type),outlier.size=-1)+
  coord_cartesian(ylim=c(0,30))+
  labs(title = "Average daily consumption", colour = "ACORN type",y = "kWh-day")

ggplot(data=hh_data)+
  geom_boxplot(aes(x=year,y=daily_total,fill=ACORN_type),outlier.size=-1)+
  coord_cartesian(ylim=c(0,30))+
  labs(title = "Average daily consumption", colour = "ACORN type",y = "kWh-day")

ggplot(data=hh_data)+
  geom_boxplot(aes(x=ACORN_type,y=daily_total),outlier.size=-1)+
  coord_cartesian(ylim=c(0,30))+
  labs(title = "Average daily consumption", colour = "ACORN type",y = "kWh-day")

ggplot(data=hh_data,aes(x=daily_total,color=ACORN_type))+
  geom_density(alpha=.4, fill="white",size=1)+
  coord_cartesian(xlim=c(0,60))+
  labs(title = "Daily consumption histogram", colour = "ACORN type",x = "kWh-day")

ggplot(data=hh_data,aes(x=daily_total,color=ACORN_group))+
  geom_density(alpha=.4, fill="white",size=1)+
  coord_cartesian(xlim=c(0,60))+
  labs(title = "Daily consumption histogram", colour = "ACORN type",x = "kWh-day")

ggplot(data=hh_data)+
  geom_boxplot(aes(x=ACORN_type,y=dn_ratio),outlier.size=-1)+
  coord_cartesian(ylim=c(0,3))+
  labs(title = "Day/nigth consumption ratio", colour = "ACORN type",y = "ratio")

ggplot(data=hh_data)+
  geom_boxplot(aes(x=ACORN_type,y=po_ratio),outlier.size=-1)+
  coord_cartesian(ylim=c(0,3))+
  labs(title = "Peak/Off-peak consumption ratio", colour = "ACORN type",y = "ratio")

ggplot(data=hh_data)+
  geom_boxplot(aes(x=ACORN_type,y=pt_ratio),outlier.size=-1)+
  coord_cartesian(ylim=c(0,1))+
  labs(title = "Peak/total consumption ratio", colour = "ACORN type",y = "ratio")


# STATISTICAL TESTS ----

##Difference between the daily consumption of the ACORN types
#If p-value < 0.05 there is difference between the means. We reject the null hypotesis of drift in mean == 0 

#Adversity vs. Affluent
res1 <- wilcox.test(hh_data[hh_data$ACORN_type=='Adversity',]$daily_total, hh_data[hh_data$ACORN_type=='Affluent',]$daily_total, alternative = "two.sided")
res1 <- res1$p.value

#Adversity vs. Confortable
res2 <- wilcox.test(hh_data[hh_data$ACORN_type=='Adversity',]$daily_total, hh_data[hh_data$ACORN_type=='Comfortable',]$daily_total, alternative = "two.sided")
res2 <- res2$p.value

#Confortable vs. Affluent
res3 <- wilcox.test(hh_data[hh_data$ACORN_type=='Comfortable',]$daily_total, hh_data[hh_data$ACORN_type=='Affluent',]$daily_total, alternative = "two.sided")
res3 <- res3$p.value

mean(hh_data[hh_data$ACORN_type=='Adversity',]$daily_total)
mean(hh_data[hh_data$ACORN_type=='Affluent',]$daily_total)
sd(hh_data[hh_data$ACORN_type=='Adversity',]$daily_total)
sd(hh_data[hh_data$ACORN_type=='Affluent',]$daily_total)

#test
d1 = rnorm(10000, mean = 0, sd = 1)
d2 = rnorm(10000, mean = 0, sd = 1)
wilcox.test(d1,d2, alternative = "two.sided")

