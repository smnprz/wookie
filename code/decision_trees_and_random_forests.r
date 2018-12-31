# DECISION TREES AND RANDOM FORESTS

# LOAD DATA AND LIBRARIES

load("E:/University College London/BENV0091 Energy Data Analysis/Group project/Data/hh_class_input.Rda")

library(tidyverse)
library(rpart)
library(rpart.plot)

# For the classification and regression tree analysis I am going to use the rpart package.

# The rpart() function is used to grow a tree. The arguments to suppply to it are:

# 1.) The formula. Here I specify the response variable I would like to model, along
#     with the features I will use to split on.
# 2.) The data matrix. The response variable has to be included in the data matrix.
# 3.) The method. Here you specify if you would like to build a classification tree
#     or a regression tree.
# 4.) The 'control' parameter. Here you can specify conditions such as the minimum 
#     number of observations a node must contain before it can be split further. Another
#     condition is the "cost complexity factor", labelled as cp (complexity parameter). 
#     This I think can be used to penalise the features that are not statistically significant.

# Firstly, I must append the ACORN group variables back to the input_hh dataframe.

input_hh <- cbind(input_hh, ID_type$ACORN_type)

names(input_hh)[which(names(input_hh) == "ID_type$ACORN_type")] <- "ACORN_type"

input_hh$ACORN_type <- factor(input_hh$ACORN_type)

# GROW A TREE.
# I will use all the features apart from the response variable and the half-hourly
# and normalised half-hourly periods (to avoid loss of interprebility).

tree <- rpart(ACORN_type ~ daily_sdm_ratio + daily_total_p25 + daily_total_p75  + daily_total_min
              + daily_total_max + hh_sdm_ratio + hh_sd + hh_p75 + hh_p25 + hh_min + hh_max
              + nt_ratio + dt_ratio + dn_ratio + sd_daily_total + m_daily_total, 
              data = input_hh, method = "class", control = rpart.control(minsplit = 30, cp = 0.0027))

printcp(tree) # Displays the results.
plotcp(tree) # Visualise cross-validation results.
summary(tree) # detailed summary of splits.

# VISUALISE RESULTS

rpart.plot(tree, type = 5, extra = 1, under = F, fallen.leaves = F, gap = 0.01, space = 0, 
            branch = .3, cex = 0.55, legend.cex = 1.2, legend.x = 0.03, legend.y = 1.01)


# How many households are in each ACORN group?

nrow(input_hh[which(input_hh$ACORN_type == "Adversity"),]) # 1404
nrow(input_hh[which(input_hh$ACORN_type == "Comfortable"),]) # 1124
nrow(input_hh[which(input_hh$ACORN_type == "Affluent"),]) # 1546

