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

########################################################################################################
#
#                                       Gaussian Mixture Models
#
########################################################################################################

library(mclust)

# Categorical variables aren't allowed to be present in the input dataframe. So let's get rid of the
# response variable.

input_hh$ACORN_type <- NULL

# The first model is created using Mclust and the default settings.

mod1 <- Mclust(input_hh)

# Show the results by inputting into the summary function.

plot(mod1, what = 'BIC')
summary(mod1$classification) # In this plot, the covariance model with the highest BIC was selected. This model corresponded to one cluster, hence why only one cluster was identified.

# Create a new model but set G = 3.
mod2 <- Mclust(input_hh, G = 3)
summary(mod2, parameters = T) # Gives detailed information on what the parameters for each partition/cluster/mixture components were upon convergence.
plot(mod2, what = 'BIC')

# Building the confusion matrix.
ID_type$ACORN_type <- factor(ID_type$ACORN_type)
Class <- ID_type$ACORN_type
table(Class, mod2$classification)

# Create a new model but this time, specify the covariance models and not the number of mixture components.

mod3 <- Mclust(input_hh, modelNames = c('VVI', 'EVI', 'VEI', 'EEI'))
summary(mod3)
plot(mod3, what = 'BIC',  legendArgs = list(x = 'bottomright'))  # This plot shows that highest BIC was calculated when 9 mixture components were used. In other words, the GMM is having a tough time identifying 3 clusters within the input dataspace.
summary(mod3$BIC) # Shows the best 3 covariance models in terms of BIC.
table(Class, mod3$classification) # Nonsensical

# Create mod4, which has G = 3 and the best 4 covariance models in terms of BIC specified.

mod4 <- Mclust(input_hh, G = 3, modelNames = c('VVI', 'EVI', 'VEI', 'EEI'))
summary(mod4)
plot(mod4, what = 'BIC',  legendArgs = list(x = 'bottomright'))  # This plot shows that highest BIC was calculated when 9 mixture components were used. In other words, the GMM is having a tough time identifying 3 clusters within the input dataspace.
summary(mod4$BIC) # Shows the best 3 covariance models in terms of BIC.
# From comparing the distribution of the data points across the ACORN groups with the
# mixing coefficients, it's assumed that cluster 1 corresponds to affluent, cluster 2
# corresponds to adversity and cluster 3 comfortable. 
mod4$classification[which(mod4$classification == 1)] <- 4
mod4$classification[which(mod4$classification == 2)] <- 1
mod4$classification[which(mod4$classification == 4)] <- 2
table(Class, mod4$classification)
adjustedRandIndex(Class, mod4$classification) # This number represents a measure of agreement between the known classifications of the training data and the identified clusters using mod4. At 0.8%, there is not much agreement at all.

# What were the top 4 features for determining the clusters?
# drmod4 can be used to project the feature space onto a subspace (dimensionality reduction).

# "The estimated directions which span the reduced subspace are defined as a set of linear combinations
# of the original features, ordered by importance as quantified by the associated eigenvalues."

# - Scrucca L. Dimension reduction for model-based clustering. Statistics and Computing. 
# 2010;20(4):471–484. doi: 10.1007/s11222-009-9138-7.

drmod4 <- MclustDR(mod4, lambda = 1)
summary(drmod4) # The top features were m_daily_total, sd_daily_total, dn_ratio and dt_ratio.
# Feature importance is defined in terms of the separation amongst the estimated clusters.
# By setting lambda = 1 within the MclusterDR function, only the information on cluster means
# is used for estimating the directions.

plot(drmod4, what = "scatterplot") # This visualises the estimated clusters in Dir1 and Dir2.
# Let's show all the misclassified points.
miscl <- classError(Class, mod4$classification)$misclassified # error rate of 0.5976927.
points(drmod4$dir[miscl,], pch = 1, cex = 2) # This shows visually how the accuracy of the GMM. All the black circles represent the misclassified data points.



test <- data.frame(drmod4$dir[,1:2], drmod4$class)
str(test$drmod4.class)
plot(test$Dir1, test$Dir2, what = "scatterlpot")

test2 <- test[which(test$drmod4.class == 1),]
nrow(test2) # 1648 observations. This class corresponds to cluster 1.
plot(test2$Dir1, test2$Dir2, what = "scatterlpot", ylim = c(-0.15, 0.1), xlim = c(-0.4, 0.1))

test3 <- test[which(test$drmod4.class == 2),]
nrow(test3) # 1742 observations. This class corresponds to cluster 2.
plot(test3$Dir1, test3$Dir2, what = "scatterlpot", ylim = c(-0.15, 0.1), xlim = c(-0.4, 0.1))

test4 <- test[which(test$drmod4.class == 3),]
nrow(test4) # 648 observations. This class corresponds to cluster 3.
plot(test4$Dir1, test4$Dir2, what = "scatterlpot", ylim = c(-0.15, 0.1), xlim = c(-0.4, 0.1))

# Therefore, green is cluster 3, red is cluster 2 and blue is cluster 1.


# Visualise the relationships between the top four features as indentified by MclustDR. 
plot(mod4, what = 'classification', dimens = c(1:4), lower.panel = NULL)

test5 <- mod4$data[,1:4]

clp4 <- clPairs(test5, mod4$classification, lower.panel = NULL, gap = 0.5,
                symbols = c(16,15,17), colors = adjustcolor(col, alpha.f = 0.5))

# Change the colour labels.
clp4$class <- c("Cluster 1", "Cluster 2", "Cluster 3")
clPairsLegend(x = 0.1, y = 0.3, class = clp4$class, col = col, pch = clp4$pch,
              title = 'Clusters')

# Plotting the four dimensions with the greatest separation.

X <- input_hh[,1:4]
col <- mclust.options('classPlotColors')[1:3]
clp2 <- clPairs(X, Class, lower.panel = NULL, gap = 0.5,
               symbols = c(16,15,17), colors = adjustcolor(col, alpha.f = 0.5))
clPairsLegend(x = 0.1, y = 0.3, class = clp$class, col = col, pch = clp$pch,
              title = 'ACORN groups')

# Saving the confusion matrix as a csv file.
gmm_confusion_matrix <- table(Class, mod4$classification)
write.csv(x = gmm_confusion_matrix, file = "gmm_confusion_matrix.csv")
