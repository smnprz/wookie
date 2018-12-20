load(file = "E:/University College London/BENV0091 Energy Data Analysis/Group project/Data/London smart meter data 1GB/halfhourly_data.Rda")

library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)
library(janitor)

hh_full_data <- hh_data
rm(hh_data)

str(hh_full_data) # Let's look at the features and see what types of variables there are.

# DATA CLEANING

# Some categorical variables need to be converted to type factor.

hh_full_data$ID <- as.factor(hh_full_data$ID)
hh_full_data$year <- as.factor(hh_full_data$year)
hh_full_data$month <- as.factor(hh_full_data$month)
hh_full_data$tariff_type <- as.factor(hh_full_data$tariff_type)
hh_full_data$ACORN_group <- as.factor(hh_full_data$ACORN_group)
hh_full_data$ACORN_type <- as.factor(hh_full_data$ACORN_type)
hh_full_data$is_we_hd <- as.factor(hh_full_data$is_we_hd)


# MISSING DATA

missing_values <- hh_full_data[which(rowSums(is.na(hh_full_data[,3:50])) > 0),] # Subset of hh_full_data, showing all the rows with more than 0 NA values.


# DISTRIBUTION OF ZERO VALUES

zero_values_distribution <- tabyl(rowSums(hh_full_data[,3:50] == 0), show_na = F) # Counts the number of zero values in each row and outputs a frequency table, showing the number of rows that contain 1, 2, ... , 48 zero values.
names(zero_values_distribution) <- c('num_of_zeros', 'counts', 'percentage')
zero_values_distribution  # Frequency distribution of zero values.
sum(zero_values_distribution[2:49,2])/nrow(hh_full_data) # 4.32 % of the rows of hh_full_data have 1 or more zero values.

# DISTRIBUTION OF NA VALUES

na_values_distribution <- tabyl(rowSums(is.na(hh_full_data[,3:50]))) # Counts the number of NA values in each row and outputs a frequency table, showing the number of rows that contain 1, 2, ... , 48 NA values.
names(na_values_distribution) <- c('num_of_NAs', 'counts', 'percentage')
na_values_distribution # Frequency distribution of NA values. Interesting to note that all rows that contain NA values, don't have more than 1 NA.
na_values_distribution[2,2]/nrow(hh_full_data) # 0.2 % of the rows of hh_full_data have 1 or more zero values.


# Plot of the distribution of zero values for subsetted rows with at least one zero value.
ggplot(zero_values_distribution[2:49,], aes(x = num_of_zeros, y = counts)) +
  theme(axis.text = element_text(size= 15), text = element_text(size = 20)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(limits = seq(1, 48, 2)) +
  labs(x = "Number of zeros", y = "Counts") +
  scale_y_continuous(breaks = seq(0, 15000, 5000))

ggsave("Distribution of zero values across rows subsetted from full data which have one or more zero value.png",
       path = getwd(),
       height = 10,
       width = 15)

# We should decide on how the rows with zero and NA values should be cleaned.

# For the rows that contain NA values, I suggest that we keep the rows, as all rows only contain one NA value.
# For the rows that contain zero values, I arbitrarily suggest we omit all rows that have > 30 zero values. Let's discuss this!

hh_full_data <- hh_full_data[which(rowSums(hh_full_data[,3:50] == 0) < 30),]

# Exploring the ACORN_types.

hh_full_data %>%
  group_by(ACORN_type) %>%
  dplyr::summarise(daily_consumption = mean(daily_total), sd_daily_consumption = sd(daily_total), sd_mean = sd(daily_total)/mean(daily_total), n = n())


hh_acorn_acorn_u_subset <- hh_full_data[hh_full_data$ACORN_type=="ACORN-U" | hh_full_data$ACORN_type=="ACORN-" ,]
hh_acorn_acorn_u_subset %>%
  group_by(year) %>%
  dplyr::summarise(n = n(), mean_daily_consumption = mean(daily_total), 
                   sd_mean = sd(daily_total)/mean(daily_total),
                   percentage = n()/nrow(hh_acorn_acorn_u_subset)) # The acorn- and acorn-U data borken down by year. 91% of the rows exist in the 2012 and 2013 data.

# Are there any ACORN- and ACORN-U rows in the control group? Do you know Simon?


# Looking at IDs. 
nrow(hh_acorn_acorn_u_subset) # 28449 of rows that are labelled type ACORN- and ACORN-U.
acorn_group_nan <- unique(hh_full_data[hh_full_data$ACORN_type=="ACORN-U" | hh_full_data$ACORN_type=="ACORN-",]$ID) # The unique IDs that have ACORN- and ACORN-U labels.
length(acorn_group_nan) # Number of unique IDs.

# NaNs in the ACORN_group for the hh_acorn_acorn_u_subset.
length(which(hh_acorn_acorn_u_subset$ACORN_group == 'NaN'))/nrow(hh_acorn_acorn_u_subset) # 769 indices or 2.7% of the hh_acorn_acorn_u_subset have NaN values in the ACORN_group column.

# NaNs values in the ACORN_group for the full data set.
nan_hh_full_subset <- hh_full_data[hh_full_data$ACORN_group == 'NaN',] # 769 observations. All these rows are also in the hh_acorn_acorn_u_subset.
str(nan_hh_full_subset) # Two tariff types Std and ToU. Not all ToU!

# I think we should remove the rows that have ACORN_group == Nan values, but keep these in a subset. Further into our analysis, maybe be can label these
# observations as one of the 18 ACORN groupings.
hh_acorn_acorn_u_subset <- hh_acorn_acorn_u_subset[!(hh_acorn_acorn_u_subset$ACORN_group == 'NaN'),]

# What are the different acorn groups that are associated with the ACORN- and ACORN-U types?
hh_acorn_acorn_u_subset %>% group_by(ACORN_group) %>% dplyr::summarise(n = n()) # This indicates that all observations in this subset belong to the ACORN U group.
hh_acorn_acorn_u_subset[which(hh_acorn_acorn_u_subset$ACORN_type == 'ACORN-'),] # This confirms that there are no observations with the ACORN- label in this subset.

# Comparing with the user guide, the only difference between the documented groupings and the groupings in this dataset is that there is no 
# group U in the literature. Likewise, there is no group R in this dataset, like there is in the literature. Does group U = group R?

# In any case, this group definitely does not belong in the ACORN_type column and should be subsetted.
hh_full_data <- hh_full_data[!(hh_full_data$ID %in% acorn_group_nan),] # This line removes the ACORN- and ACORN-U row from the full data.

# CHECKING ZERO VALUES ON DAILY TOTAL, NOT DELETED ENTRIES
nrow(hh_full_data[hh_full_data$daily_total==0,])

