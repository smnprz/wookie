# LOAD PACKAGES AND DATA -----

load(file = "E:/University College London/BENV0091 Energy Data Analysis/Group project/Data/London smart meter data 1GB/halfhourly_data.Rda")


library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)
library(janitor)
library(matrixStats)
library(imputeTS)

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
View(strange_nas) # The NAs instead appear in other columns.

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

# For the rows that contain zero values, I arbitrarily suggest we omit all rows that have > 30 zero values. Let's discuss this!

# For now it's agreed that we should at least get rid of all the rows with 48 zeros.
hh_full_data <- hh_full_data[which(rowSums(hh_full_data[,3:50] == 0) < 48),]

# NEED TO CHECK THE CONTROL GROUP

# Select data only from 2013 and with standar tariff (only control group from the experiment)
hh_data <- hh_full_data
hh_data <- hh_data[as.numeric(as.character(hh_data$year)) == 2013 & as.character(hh_data$tariff_type) == "Std",]

# Select ID with at least 80% of the days measured (discussion!)
ID_count <- hh_data %>%
  group_by(ID) %>%
  dplyr::summarize(n=n())
ID_count_2 <- ID_count[ID_count$n>=365*0.8,]

hh_data <- hh_data[hh_data$ID %in% ID_count_2$ID,]

# Keep rows with at least 20 half hourly values different than 0 (discussion!)
hh_data <- hh_data[which(rowSums(hh_data[,3:50] == 0) <= 28),]

# REMOVING IDS WITH WRONG LABEL IN ACORN_TYPE
hh_data %>%
  group_by(ACORN_type) %>%
  dplyr::summarise(daily_consumption = mean(daily_total), sd_daily_consuption = sd(daily_total), sd_mean = sd(daily_total)/mean(daily_total), n = n())

ids_nan <- unique(hh_data[hh_data$ACORN_type=="ACORN-U" | hh_data$ACORN_type=="ACORN-",]$ID)
length(ids_nan)
hh_data <- hh_data[!(hh_data$ID %in% ids_nan),]


# FEATURE ENGINEERING ----

hh_data$year_month <- substr(ISOdate(as.numeric(as.character(hh_data$year)),as.numeric(as.character(hh_data$month)),1),1,10)
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

#NIGHT USE from 6pm to 6am (discuss!)
hh_data$night_total <- hh_data$hh_0 + hh_data$hh_1 + hh_data$hh_2 + hh_data$hh_3 + hh_data$hh_4 + hh_data$hh_5 + hh_data$hh_6 + hh_data$hh_7 + hh_data$hh_8 + hh_data$hh_9 + hh_data$hh_10 + hh_data$hh_11 +
  + hh_data$hh_36 + hh_data$hh_37 + hh_data$hh_38 + hh_data$hh_39 + hh_data$hh_40 + hh_data$hh_41 + hh_data$hh_42 + hh_data$hh_43 + hh_data$hh_44 + hh_data$hh_45 + hh_data$hh_46 + hh_data$hh_47

hh_data$day_total <- hh_data$daily_total-hh_data$night_total

#DAY-NIGTH RATIO
hh_data$dn_ratio <- hh_data$day_total/hh_data$night_total
hh_data$nt_ratio <- hh_data$night_total/hh_data$daily_total
hh_data$dt_ratio <- hh_data$day_total/hh_data$daily_total

#Non-PEAK hours from 23:00 to 7:00 (discuss!)
#https://customerservices.npower.com/app/answers/detail/a_id/179/~/what-are-the-economy-7-peak-and-off-peak-periods%3F
hh_data$off_peak_total <- hh_data$hh_0 + hh_data$hh_1 + hh_data$hh_2 + hh_data$hh_3 + hh_data$hh_4 + hh_data$hh_5 + hh_data$hh_6 + hh_data$hh_7 + hh_data$hh_8 + hh_data$hh_9 + hh_data$hh_10 + hh_data$hh_11 + hh_data$hh_12 + hh_data$hh_13+
  hh_data$hh_46 + hh_data$hh_47

#PEAK hous from 16:00 to 19:00 (discuss!)
#https://www.theguardian.com/money/2017/jan/07/night-time-use-electrical-appliances-lower-bills-fire-risk
hh_data$peak_total <- hh_data$hh_32 + hh_data$hh_33 + hh_data$hh_34 + hh_data$hh_35 + hh_data$hh_36 + hh_data$hh_37

#PEAK-OFF PEAK RAIO
hh_data$po_ratio <- hh_data$peak_total/hh_data$off_peak_total
hh_data$pt_ratio <- hh_data$peak_total/hh_data$daily_total
hh_data$ot_ratio <- hh_data$off_peak_total/hh_data$daily_total

#ELIIMINATE INFINITES DUE TO CONSUMPTION == 0
hh_data[is.infinite(hh_data$dn_ratio),]$dn_ratio <- NaN 
hh_data[is.infinite(hh_data$po_ratio),]$po_ratio <- NaN
hh_data[is.infinite(hh_data$dt_ratio),]$dt_ratio <- NaN 
hh_data[is.infinite(hh_data$nt_ratio),]$nt_ratio <- NaN 
hh_data[is.infinite(hh_data$pt_ratio),]$pt_ratio <- NaN 
hh_data[is.infinite(hh_data$ot_ratio),]$ot_ratio <- NaN

#HOURLY STD
hh_cons <- data.matrix(select(hh_data,c(3:40)))
hh_data$hourly_sd <- rowSds(hh_cons,na.rm=TRUE)
hh_data$hourly_sdm_ratio <- hh_data$hourly_sd/(hh_data$daily_total/48)
rm(hh_cons)

#Summarize dataset

hh_data %>%
  group_by(ACORN_type) %>%
  dplyr::summarise(daily_consumption = mean(daily_total), sd_daily_consuption = sd(daily_total), 
                   sd_mean = sd(daily_total)/mean(daily_total), n_ID = length(unique(ID)), n = n())

# SAVE RESULTING TIDY AND WIDE EXTENDEND DATASET -----
save(hh_data,file="../Data/SML_data/tidy_data/halfhourly_data_tidy.Rda")



