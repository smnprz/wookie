# LOAD PACKAGES AND DATA -----

#load(file = "~/University College London/BENV0091 Energy Data Analysis/Group project/wookie/London smart meter data 1GB/halfhourly_data.Rda")
load("../Data/SML_data/tidy_data/halfhourly_data.Rda")

library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)
library(janitor)
library(matrixStats)

hh_full_data <- hh_data
rm(hh_data)

str(hh_full_data) # Let's look at the features and see what types of variables there are.

# Some categorical variables need to be converted to type factor.

hh_full_data$ID <- as.factor(hh_full_data$ID)
hh_full_data$year <- as.factor(hh_full_data$year)
hh_full_data$month <- as.factor(hh_full_data$month)
hh_full_data$tariff_type <- as.factor(hh_full_data$tariff_type)
hh_full_data$ACORN_group <- as.factor(hh_full_data$ACORN_group)
hh_full_data$ACORN_type <- as.factor(hh_full_data$ACORN_type)
hh_full_data$is_we_hd <- as.factor(hh_full_data$is_we_hd)

# IDENTIFYING MISSING VALUES ----

missing_values <- hh_full_data[which(rowSums(is.na(hh_full_data[,3:50])) > 0),] # Subset of hh_full_data, showing all the rows with more than 0 NA values.

# DISTRIBUTION OF NA VALUES

na_values_distribution <- tabyl(rowSums(is.na(hh_full_data[,3:50]))) # Counts the number of NA values in each row and outputs a frequency table, showing the number of rows that contain 1, 2, ... , 48 NA values.
names(na_values_distribution) <- c('num_of_NAs', 'counts', 'percentage')
na_values_distribution # Frequency distribution of NA values. Interesting to note that all rows that contain NA values, don't have more than 1 NA.
na_values_distribution[2,2]/nrow(hh_full_data) # 0.2 % of the rows of hh_full_data have 1 or more NA values.

# LINEAR INTERPOLATION OF THE NA VALUES

# Check to make sure that all na values appear in column hh_30
tabyl(is.na(missing_values$hh_30)) # Surprisingly, there are 26 rows where this is not true.

# Exploring the 26 rows.
strange_nas <- missing_values[which(is.na(missing_values$hh_30) == F),] 
#View(strange_nas) # The NAs instead appear in other columns.

# We can still apply linear interpolation no problem, but we can't do it manually. Perform row
# wise linear interpolation using na.interpolation() which is part of the imputeTS package.
interpolated_data <- t(apply(missing_values[,3:50], 1, na.interpolation)) # A matrix is outputted which only contains the 48 half hourly columns.

# Convert back to a dataframe.
interpolated_data <- as.data.frame(interpolated_data)

# Add back the columns that were omitted.
interpolated_data <- cbind(missing_values[,1:2], interpolated_data, missing_values[,51:59])

# Now the daily_total column can be recalculated.
interpolated_data$daily_total <- apply(interpolated_data[,3:50], 1, sum)

# Recombining this data with hh_full_data. 
hh_full_data[which(rowSums(is.na(hh_full_data[,3:50])) > 0),] <- interpolated_data

# DISTRIBUTION OF ZERO VALUES

zero_values_distribution <- tabyl(rowSums(hh_full_data[,3:50] == 0), show_na = F) # Counts the number of zero values in each row and outputs a frequency table, showing the number of rows that contain 1, 2, ... , 48 zero values.
names(zero_values_distribution) <- c('num_of_zeros', 'counts', 'percentage')
zero_values_distribution  # Frequency distribution of zero values.
sum(zero_values_distribution[2:49,2])/nrow(hh_full_data) # 4.32 % of the rows of hh_full_data have 1 or more zero values.


# Plot of the distribution of zero values for subsetted rows with at least one zero value.
ggplot(zero_values_distribution[2:49,], aes(x = num_of_zeros, y = counts)) +
  theme(axis.text = element_text(size= 15), text = element_text(size = 20)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(limits = seq(1, 48, 2)) +
  labs(x = "Number of zeros", y = "Counts") +
  scale_y_continuous(breaks = seq(0, 15000, 5000))

# DATA CLEANING -----

# We should decide on how the rows with zero and NA values should be cleaned.

# For the rows that contain NA values, I suggest that we keep the rows, as all rows only contain one NA value.
# For the rows that contain zero values, I arbitrarily suggest we omit all rows that have > 30 zero values. Let's discuss this!

#hh_full_data <- hh_full_data[which(rowSums(hh_full_data[,3:50] == 0) < 30),]

#Select data only from 2013 and with standar tariff (only control group from the experiment)
hh_data_ss <- hh_full_data
hh_data_ss <- hh_data_ss[as.numeric(as.character(hh_data_ss$year)) == 2013 & as.character(hh_data_ss$tariff_type) == "Std",]

#Select ID with at least 80% of the days measured (discussion!)
ID_count <- hh_data_ss %>%
  group_by(ID) %>%
  dplyr::summarize(n=n())
ID_count_2 <- ID_count[ID_count$n>=365*0.8,]
hh_data_ss <- hh_data_ss[hh_data_ss$ID %in% ID_count_2$ID,]

#Drop rows with 48 values == to zero
hh_data_ss <- hh_data_ss[which(rowSums(hh_data_ss[,3:50] == 0) != 48),]

#NA ROWS
missing_values_ss <- hh_data_ss[which(rowSums(is.na(hh_data_ss[,3:50])) > 0),] # Subset of hh_full_data, showing all the rows with more than 0 NA values.
if (nrow(missing_values_ss)>0){
  print("Check NAs")
  #hh_data_ss <- hh_data_ss[!is.na(hh_data_ss$daily_total),]
  #ADD LINEAR INTERPOLATION
}

#REMOVING IDS WITH WRONG LABEL IN ACORN_TYPE
hh_data_ss %>%
  group_by(ACORN_type) %>%
  dplyr::summarise(daily_consumption = mean(daily_total), sd_daily_consuption = sd(daily_total), sd_mean = sd(daily_total)/mean(daily_total), n = n())

ids_nan <- unique(hh_data_ss[hh_data_ss$ACORN_type=="ACORN-U" | hh_data_ss$ACORN_type=="ACORN-",]$ID)
length(ids_nan)
hh_data_ss <- hh_data_ss[!(hh_data_ss$ID %in% ids_nan),]

# FEATURE ENGINEERING ----

hh_data_ss$year_month <- substr(ISOdate(as.numeric(as.character(hh_data_ss$year)),as.numeric(as.character(hh_data_ss$month)),1),1,10)
hh_data_ss$year_month <- factor(hh_data_ss$year_month)

#Daily (hh) Min, Max and STD
hh_cons <- data.matrix(select(hh_data_ss,c(3:50)))
hh_data_ss$hh_max <- rowMaxs(hh_cons)
hh_data_ss$hh_min <- rowMins(hh_cons)
hh_data_ss$hh_p25 <- rowQuantiles(hh_cons,probs = 0.25)
hh_data_ss$hh_p75 <- rowQuantiles(hh_cons,probs = 0.75)
hh_data_ss$hourly_sd <- rowSds(hh_cons,na.rm=TRUE)
hh_data_ss$hourly_sdm_ratio <- hh_data_ss$hourly_sd/(hh_data_ss$daily_total/48)
rm(hh_cons)

#daily profile (max = 1)

norm_hh <- sweep(hh_data_ss[,3:50],hh_data_ss[,"hh_max"],MARGIN=1,"/")
colN <- sprintf("phh_%s",(0:47))
colnames(norm_hh) <- colN
norm_hh$ID <- hh_data_ss$ID
norm_hh$date <- hh_data_ss$date

hh_data_ss <- merge(hh_data_ss,norm_hh,by = c("ID","date"))

#NIGTH USE from 6pm to 6am (discuss!)

hh_data_ss$night_total <- hh_data_ss$hh_0 + hh_data_ss$hh_1 + hh_data_ss$hh_2 + hh_data_ss$hh_3 + hh_data_ss$hh_4 + hh_data_ss$hh_5 + hh_data_ss$hh_6 + hh_data_ss$hh_7 + hh_data_ss$hh_8 + hh_data_ss$hh_9 + hh_data_ss$hh_10 + hh_data_ss$hh_11 +
  + hh_data_ss$hh_36 + hh_data_ss$hh_37 + hh_data_ss$hh_38 + hh_data_ss$hh_39 + hh_data_ss$hh_40 + hh_data_ss$hh_41 + hh_data_ss$hh_42 + hh_data_ss$hh_43 + hh_data_ss$hh_44 + hh_data_ss$hh_45 + hh_data_ss$hh_46 + hh_data_ss$hh_47

hh_data_ss$day_total <- hh_data_ss$daily_total-hh_data_ss$night_total

#DAY-NIGTH RATIO
hh_data_ss$dn_ratio <- hh_data_ss$day_total/hh_data_ss$night_total
hh_data_ss$nt_ratio <- hh_data_ss$night_total/hh_data_ss$daily_total
hh_data_ss$dt_ratio <- hh_data_ss$day_total/hh_data_ss$daily_total

#Non-PEAK hours from 23:00 to 7:00 (discuss!)
#https://customerservices.npower.com/app/answers/detail/a_id/179/~/what-are-the-economy-7-peak-and-off-peak-periods%3F
hh_data_ss$off_peak_total <- hh_data_ss$hh_0 + hh_data_ss$hh_1 + hh_data_ss$hh_2 + hh_data_ss$hh_3 + hh_data_ss$hh_4 + hh_data_ss$hh_5 + hh_data_ss$hh_6 + hh_data_ss$hh_7 + hh_data_ss$hh_8 + hh_data_ss$hh_9 + hh_data_ss$hh_10 + hh_data_ss$hh_11 + hh_data_ss$hh_12 + hh_data_ss$hh_13+
  hh_data_ss$hh_46 + hh_data_ss$hh_47

#PEAK hous from 16:00 to 19:00 (discuss!)
#https://www.theguardian.com/money/2017/jan/07/night-time-use-electrical-appliances-lower-bills-fire-risk
hh_data_ss$peak_total <- hh_data_ss$hh_32 + hh_data_ss$hh_33 + hh_data_ss$hh_34 + hh_data_ss$hh_35 + hh_data_ss$hh_36 + hh_data_ss$hh_37

#PEAK-OFF PEAK RAIO
hh_data_ss$po_ratio <- hh_data_ss$peak_total/hh_data_ss$off_peak_total
hh_data_ss$pt_ratio <- hh_data_ss$peak_total/hh_data_ss$daily_total
hh_data_ss$ot_ratio <- hh_data_ss$off_peak_total/hh_data_ss$daily_total

#ELIIMINATE INFINITES DUE TO CONSUMPTION == 0
hh_data_ss[is.infinite(hh_data_ss$dn_ratio),]$dn_ratio <- NaN 
hh_data_ss[is.infinite(hh_data_ss$po_ratio),]$po_ratio <- NaN
hh_data_ss[is.infinite(hh_data_ss$dt_ratio),]$dt_ratio <- NaN 
hh_data_ss[is.infinite(hh_data_ss$nt_ratio),]$nt_ratio <- NaN 
hh_data_ss[is.infinite(hh_data_ss$pt_ratio),]$pt_ratio <- NaN 
hh_data_ss[is.infinite(hh_data_ss$ot_ratio),]$ot_ratio <- NaN

#Summarize dataset

hh_data_ss %>%
  group_by(ACORN_type) %>%
  dplyr::summarise(daily_consumption = mean(daily_total), sd_daily_consuption = sd(daily_total), sd_mean = sd(daily_total)/mean(daily_total), n_ID = length(unique(ID)), n = n())

# CREATE DATASET AS INPUT

hh_data_ID <- hh_data_ss %>%
  group_by(ID) %>%
  dplyr::summarise(m_daily_total = mean(daily_total, na.rm = TRUE), sd_daily_total = sd(daily_total, na.rm = TRUE),
                   dn_ratio = mean(dn_ratio, na.rm = TRUE),dt_ratio = mean(dt_ratio, na.rm = TRUE), 
                   nt_ratio = mean(nt_ratio, na.rm = TRUE),po_ratio=mean(po_ratio, na.rm = TRUE),
                   pt_ratio = mean(pt_ratio, na.rm = TRUE),ot_ratio = mean(ot_ratio, na.rm = TRUE),
                   hh_max = mean(hh_max, na.rm = TRUE),hh_min = mean(hh_min, na.rm = TRUE),
                   hh_p25 = mean(hh_p25, na.rm = TRUE),hh_p75 = mean(hh_p75, na.rm = TRUE),
                   hh_sd = mean(hourly_sd, na.rm = TRUE),hh_sdm_ratio = mean(hourly_sdm_ratio, na.rm = TRUE),
                   daily_total_max = max(daily_total, na.rm = TRUE), daily_total_min = min(daily_total, na.rm = TRUE),
                   daily_total_p75 = quantile(daily_total,0.75), daily_total_p25 = quantile(daily_total,0.25),
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
                   hh_45 = mean(hh_45, na.rm = TRUE), hh_46 = mean(hh_46, na.rm = TRUE), hh_47 = mean(hh_47, na.rm = TRUE))

hh_data_ID <- hh_data_ID[complete.cases(hh_data_ID),]
hh_data_ID$daily_sdm_ratio <- hh_data_ID$sd_daily_total/hh_data_ID$m_daily_total

# Save type group
ID_type_all<-select(hh_data_ss,c('ID','ACORN_type'))
ID_type_all<-ID_type_all[!duplicated(ID_type_all),]
ID_type <- merge(hh_data_ID, ID_type_all, by="ID",all.x = TRUE)
ID_type$adversity <- 0
ID_type$affluent <- 0
ID_type$comfortable <- 0
ID_type[ID_type$ACORN_type == "Adversity",]$adversity <- 1
ID_type[ID_type$ACORN_type == "Affluent",]$affluent <- 1
ID_type[ID_type$ACORN_type == "Comfortable",]$comfortable <- 1
ID_type<-select(ID_type,c('adversity','affluent','comfortable','ACORN_type','ID'))

ID_type <- ID_type[order(ID_type$ID),]
hh_data_ID <- hh_data_ID[order(hh_data_ID$ID),]

input_hh <- select(hh_data_ID,-c('ID'))
output_hh <- ID_type$adversity

# SAVE RESULTING TIDY AND WIDE EXTENDEND DATASET -----
save(hh_data_ss,ID_type,input_hh,output_hh,file="../Data/SML_data/tidy_data/hh_class_input.Rda")

#The predicted output from each method should be a nx1 binary matrix
#just to check. How many rows does your input have? I have 4095




