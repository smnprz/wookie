# DECISION TREES AND RANDOM FORESTS

# LOAD DATA AND LIBRARIES

load("E:/University College London/BENV0091 Energy Data Analysis/Group project/Data/hh_class_input.Rda")

library(tidyverse)
library(rpart)
library(rpart.plot)

# For the classification and regression tree (CART) analysis I am going to use the rpart package.

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

full_tree <- rpart(ACORN_type ~., data = input_hh, method = "class", control = rpart.control(minsplit = 30, cp = 0.0053))


printcp(tree) # Displays the results.
plotcp(tree) # Visualise cross-validation results.
summary(tree) # detailed summary of splits.

# VISUALISE RESULTS

rpart.plot(tree, type = 5, extra = 1, under = F, fallen.leaves = F, gap = 0.01, space = 0, 
            branch = .3, cex = 0.55, legend.cex = 1.2, legend.x = 0.03, legend.y = 1.01)

rpart.plot(full_tree, type = 5, extra = 1, under = F, fallen.leaves = F, gap = 0.01, space = 0, 
           branch = .5, cex = 1, legend.cex = 1.2, legend.x = 0.03, legend.y = 1.01)


# How the rpart anaylsis is done on datacamp to obtain the optimum tree structure:

# Build tree
tree_prior <- rpart(ACORN_type ~., data = input_hh, method = "class", control = rpart.control(minsplit = 30, cp = 0.0053))

# Use plotcp() to visualise error-cp relationship.
plotcp(tree_prior)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized.
cp_table <- data.frame(printcp(tree_prior))

#write.csv(x = cp_table, file = "cp_table.csv")

# Create an index for of the row with the minimum xerror
index <- which.min(tree_prior$cptable[ , "xerror"])

# Create tree_min
tree_min <- tree_prior$cptable[index, "CP"]

#  Prune the tree using tree_min
ptree_prior <- prune(tree_prior, cp = tree_min)

# Use prp() to plot the pruned tree
prp(ptree_prior)



# How many households are in each ACORN group?

nrow(input_hh[which(input_hh$ACORN_type == "Adversity"),]) # 1404
nrow(input_hh[which(input_hh$ACORN_type == "Comfortable"),]) # 1124
nrow(input_hh[which(input_hh$ACORN_type == "Affluent"),]) # 1546


##############################################################################################
#
#                                     RANDOM FORESTS
#
##############################################################################################


library(ggplot2)
library(cowplot)
library(randomForest)

set.seed(42)

# Building the random forest.

rf1 <- randomForest(ACORN_type ~., data = input_hh, proximity = T, ntree = 500) # OOB estimate of  error rate: 53.22%. rf1 predicted 46.78% of the datapoints correctly.

# Check to see if there are enough trees in the random forest.

oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf1$err.rate), times=4),
  Type=rep(c("OOB", "Adversity", "Affluent", "Comfortable"), each=nrow(rf1$err.rate)),
  Error=c(rf1$err.rate[,"OOB"],
          rf1$err.rate[,"Adversity"],
          rf1$err.rate[,"Affluent"],
          rf1$err.rate[,"Comfortable"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

rf2 <- randomForest(ACORN_type ~., data = input_hh, proximity = T, ntree = 1000)

# Statquest constructs the error/number of tree dataframe in long format.

oob.error.data2 <- data.frame(
  Trees=rep(1:nrow(rf2$err.rate), times=4),
  Type=rep(c("OOB", "Comfortable", "Adversity", "Affluent"), each=nrow(rf2$err.rate)),
  Error=c(rf2$err.rate[,"OOB"],
          rf2$err.rate[,"Comfortable"],
          rf2$err.rate[,"Adversity"],
          rf2$err.rate[,"Affluent"]))

library(RColorBrewer)

myColours <- brewer.pal(4, "Set1")
names(myColours) <- levels(oob.error.data1$Type)
colScale <- scale_colour_manual(name = "Type", values = myColours)


ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type)) + 
  labs(x = "Number of trees", y = "Misclassification error", colour = "Legend") +
  colScale

# Minimum difference in error rates between rf1 and rf2, together with the plot indicates that
# ~500 trees seems to be optimum. After ntree = 500, the OOB error rate plateaus.

# Determine the optimum number of variables for bagging.

oob.values <- vector(length=10)
for(i in 5:15) {
  temp.model <- randomForest(ACORN_type ~., data = input_hh, proximity = T, ntree = 500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values

# Save the confusion matrix.

rf1_cnf_matrix <- data.frame(rf1$confusion)

write.csv(x = rf1_cnf_matrix, file = "rf1_cnf_matrix.csv")

