
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

# We should decide on how the rows with zero and NA values should be cleaned.

# For the rows that contain NA values, I suggest that we keep the rows, as all rows only contain one NA value.
# For the rows that contain zero values, I arbitrarily suggest we omit all rows that have > 30 zero values. Let's discuss this!

hh_full_data <- hh_full_data[which(rowSums(hh_full_data[,3:50] == 0) < 30),]





# Plotting the energy consumption of all households over the data collection period.

# Date, Total energy consumed

daily_totals <- select(hh_full_data, date, daily_total)

# For each date, I would like to add up all the daily total values.

daily_totals_summed <- daily_totals %>% group_by(date) %>% summarise(sum(daily_total))

names(daily_totals_summed) <- names(daily_totals)

# Text wrapping function.



wrapper <- function(x, ...)
{
  paste(strwrap(x, ...), collapse = "\n")
}



ggplot(daily_totals_summed, aes(x = date, y = daily_total)) +
  theme(axis.text = element_text(size= 8), text = element_text(size = 9), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_line() +
  labs(x = ~Date, y = ~Total ~consumption ~(kWhr)) +
  ggtitle("Total electricity consumption for all households over the entire data collection period") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file = "Total electricity consumption for all households over the entire data collection period.png", 
       height = 6, width = 8)

# Mucking about.

# Day of the year is missing. This will be helpful to have later for plotting consumption vs
# day of the year. 

hh_full_data$y_day <- yday(hh_full_data$date)

# Renaming the 'day' variable to 'm_day' to avoid confusion.

colnames(hh_full_data)[colnames(hh_full_data) == 'day'] <- "m_day"

# Rearranging the columns.

hh_full_data <- hh_full_data[, c(1:54, 60, 55, 56, 57, 58, 59)]

# Over what years has the data been collected?
table(hh_full_data$year)





# Let's break the data up into the years it has been collected across.

hh_2011_data <- hh_full_data[which(hh_full_data$year == '2011'),]
hh_2012_data <- hh_full_data[which(hh_full_data$year == '2012'),]
hh_2013_data <- hh_full_data[which(hh_full_data$year == '2013'),]
hh_2014_data <- hh_full_data[which(hh_full_data$year == '2014'),]

# Aggregate the datasets into a list.

hh_data_by_year <- list(hh_2011_data, hh_2012_data, hh_2013_data, hh_2014_data)

# Do PCA analysis for each dataframe in the hh_data_by_year list.

lapply(x = hh_data_by_year, FUN = prcomp(x[,3:50], scale. = T, rank. = 10))


# There's a lot of half hourly data, 48 columns. For each of those 48 columns, there are
# over 5000 numbers.

# DIMENSIONALITY REDUCTION

# Let's see if we can show that most of the variance in the half hourly periods can be 
# shown by only a few variables (q half hourly  periods).

# I won't need to scale to unit variance, as the half hourly features I'm analysing all have 
# the same units. The prcomp function performs PCA on a given dataset. 

prc_results <- prcomp(hh_2014_data[,3:50], scale. = T, rank. = 10)

# Summary of results

prc_summary_2014 <- prc_results %>% summary()

# What half hourly periods are the principal components with the largest % variance?

prc_perc_variance <- (prc_summary_2014[[1]]^2/sum(prc_summary_2014[[1]]^2))[1:10]

# Convert to percentages

prc_perc_variance <-  prc_perc_variance*100

Princpal_components <- seq(1,10, 1)

prc_var_df <- data.frame(Princpal_components, prc_perc_variance)

# Using the screeplot function.
screeplot(prc_results, type = "bar", main = "PCA of the 2014 data")

# Same graph using ggplot.
ggplot(data = prc_var_df, aes(x = Princpal_components, y = prc_perc_variance)) +
  geom_col(fill = '#238cd8') +
  geom_line(colour = '#da7b93', size = 1) +
  geom_point(size = 3, color = 'black') +
  scale_x_continuous(breaks = Princpal_components) +
  labs(x = ~Principal ~components, y = ~Variance ~('%')) +
  ggtitle("Screeplot for the 2014 half hourly data") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file = "Screeplot for the 2014 half hourly data.png", 
       height = 6, width = 8)

# CLUSTERING.

# Feature engineering.

# Penalised regression (dealing with multicollinearity between features).
