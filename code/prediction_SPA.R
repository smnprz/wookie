# Load libraries -----
library(dplyr)
library(broom)
library(caret)
library(modelr)
library(neuralnet)
library(tidyverse)

# Load and standarize data----
load(file = "./data/hh_class_input_short.Rda")
#oad(file = "../Data/SML_data/tidy_data/hh_class_input.Rda")

standardize <- function(x){(x-min(x))/(max(x)-min(x))}

input_hh <- as.data.frame(input_hh)
input_hh_norm<-as.data.frame(apply(input_hh, 2, standardize))
input_hh_scale<-as.data.frame(scale(input_hh))

#Select test and sample data
test_sample <- sample(dim(volta_reduced)[1], dim(volta_reduced)[1] * 0.3)
volta_test <- volta_reduced[test_sample, ]
volta_train <- volta_reduced[-test_sample, ]

#Hieraquical clustering----

#resHC <- input_hh[,16:63]
resHC <- input_hh[,64:111]
resHC$ID <- ID_type$ID
resHC <- resHC[,c(49,1:48)]

input_data <- input_hh_norm
input_data <- input_hh_scale
input_data <- input_hh

d <- dist(input_data,method="euclidean")

#methodHC <- "single"
#methodHC <- "median"
methodHC <- "average"

h1 <- hclust(d,method="average")
clusterCut1 <- cutree(h1, 3)
table(clusterCut1)
table(clusterCut1,ID_type$ACORN_type)

h2 <- hclust(d,method="median")
clusterCut2 <- cutree(h2, 3)
table(clusterCut2)
table(clusterCut2,ID_type$ACORN_type)

h3 <- hclust(d,method="single")
clusterCut3 <- cutree(h3, 3)
table(clusterCut3)
table(clusterCut3,ID_type$ACORN_type)

resHC %<>% mutate(clusterCut1)
resHC1 <- gather(resHC,time,MW,2:49)
resHC1$time <- as.numeric(gsub("hh_","",(resHC1$time)))
#resHC1$time <- as.numeric(gsub("phh_","",(resHC1$time)))

ggplot(resHC1)+
  geom_line(aes(x=(time),y=MW,group=ID),alpha=.2)+facet_wrap(~clusterCut1,ncol=2)

resHC1_sum <- resHC1 %>%
  group_by(clusterCut1,time) %>%
  dplyr::summarise(MW = mean(MW,na.rm = T))

ggplot(resHC1_sum)+
  geom_line(aes(x=(time),y=MW,group=clusterCut1),alpha=.5)



#Logistic regression----

#input_hh_logit <- input_hh_scale
#input_hh_logit <- input_hh_norm
input_hh_logit <- input_hh

#input_hh_logit <- input_hh_logit[,64:111]

form <- paste("output~",paste(colnames(input_hh_logit),collapse="+"),sep="")
for (i in c(1,2,3)){
  if (i==1){
    input_hh_logit$output <- output_hh_adv
  }else if (i==2){
    input_hh_logit$output <- output_hh_aff
  }else{
    input_hh_logit$output <- output_hh_comf
  }
 
  model <- glm(as.formula(form),family=binomial(link='logit'),data=input_hh_logit)
  sel<-tidy(model)
  sel<-sel[sel$p.value<=0.05,]
  
  form2 <- paste("output~",paste(sel[sel$term != "(Intercept)",]$term,collapse="+"),sep="")
  model2 <- glm(as.formula(form2),family=binomial(link='logit'),data=input_hh_logit)
  sel2<-tidy(model2)
  sel2<-sel2[sel2$p.value<=0.05,]
  
  form3 <- paste("output~",paste(sel2[sel2$term != "(Intercept)",]$term,collapse="+"),sep="")
  model3 <- glm(as.formula(form3),family=binomial(link='logit'),data=input_hh_logit)
  sel3<-tidy(model3)
  sel3<-sel3[sel3$p.value<=0.05,]
  
  temp <- add_predictions(input_hh_logit,model3)
  input_hh_logit[,length(input_hh_logit)+1]=temp$pred
  #input_hh_logit$output_pred <- 0
  #input_hh_logit[input_hh_logit$pred>0,]$output_pred <- 1
}

pred_logit<-input_hh_logit[,(length(input_hh_logit)-2):length(input_hh_logit)]
pred_class <- max.col(pred_logit)
actual_class <- max.col(ID_type[,1:3])
mean(pred_class == actual_class)

pred_class <- factor(as.factor(pred_class),c(1,2,3))
actual_class <- factor(as.factor(actual_class),c(1,2,3))

CM<-confusionMatrix(pred_class,actual_class)
CM$table

#Logistic regression CROSS VALIDATION----

#input_hh_logit <- input_hh_scale
#input_hh_logit <- input_hh_norm
input_hh_logit <- input_hh

set.seed(500)
k <- 10
outs <- NULL
proportion <- 1-1/k

#Randomly shuffle the data
all_data <- cbind(input_hh_logit , ID_type)
all_data<-all_data[sample(nrow(all_data)),]
input_hh_logit<-all_data[,1:112]
ID_type_logit<-all_data[,113:117]
folds <- cut(seq(1,nrow(all_data)),breaks=k,labels=FALSE)

for(sce in 1:k)
{
  testIndexes <- which(folds==sce,arr.ind=TRUE)
  test_cv <- input_hh_logit[testIndexes, ]
  train_cv <- input_hh_logit[-testIndexes, ]
  actual <- ID_type_logit[testIndexes,1:3]
  pred <- ID_type_logit[testIndexes,1:3]
  for (i in c(1,2,3)){
    if (i==1){
      train_cv$output <- output_hh_adv[-testIndexes]
    }else if (i==2){
      train_cv$output <- output_hh_aff[-testIndexes]
    }else{
      train_cv$output <- output_hh_comf[-testIndexes]
    }
    
    model_cv <- glm(as.formula(form3),family=binomial(link='logit'),data=train_cv)
    temp <- add_predictions(test_cv,model_cv)
    pred[,i] <- temp$pred
  }
  pred_class <- max.col(pred)
  actual_class <- max.col(actual)
  outs[sce] <- mean(pred_class == actual_class)
}
1-mean(outs)  

#Neural networks V2 ----
input_hh_nn <- input_hh_scale
input_hh_nn <- input_hh_norm

input_hh_nn <- input_hh_nn[,64:111]

input_hh_nn <- input_hh_nn[,c(1:15,112,53,62,63,98,101)]

f <- as.formula(paste("adv + aff + comf ~", paste(names(input_hh_nn), collapse = " + ")))
f

input_hh_nn$adv <- output_hh_adv
input_hh_nn$aff <- output_hh_aff
input_hh_nn$comf <- output_hh_comf

nn <- neuralnet(f,
                data = input_hh_nn,
                hidden = c(1),
                act.fct = "logistic",
                #stepmax = 1e+06,
                linear.output = FALSE,
                lifesign = "minimal")


plot(nn)

pr.nn <- compute(nn, input_hh_nn[, 1:(ncol(input_hh_nn)-3)])
pr.nn_ <- pr.nn$net.result

original_values <- max.col(input_hh_nn[, (ncol(input_hh_nn)-2):ncol(input_hh_nn)])
pr.nn_2 <- max.col(pr.nn_)
mean(pr.nn_2 == original_values)

pred <- factor(as.factor(pr.nn_2),c(1,2,3))
or <- factor(as.factor(original_values),c(1,2,3))

CM<-confusionMatrix(pred,or)
CMnn <- t(CM$table)
CMnn

#FIND OPTIMAL NUMBER OF NEURONS
neu <- 10
outs_neuron <- NULL
for(i in 1:neu)
{
  nn <- neuralnet(f,
                  data = input_hh_nn,
                  hidden = c(i),
                  act.fct = "logistic",
                  stepmax = 1e+06,
                  linear.output = FALSE,
                  lifesign = "minimal")
  
  pr.nn <- compute(nn, input_hh_nn[, 1:(ncol(input_hh_nn)-3)])
  pr.nn_ <- pr.nn$net.result
  
  original_values <- max.col(input_hh_nn[, (ncol(input_hh_nn)-2):ncol(input_hh_nn)])
  pr.nn_2 <- max.col(pr.nn_)
  outs_neuron[i] <- mean(pr.nn_2 == original_values)
}
pl <- 1-outs_neuron
plot(pl,xlab="number of neurons per layer", ylab="apparent error")
x <- (1:length(pl))
lines(predict(lm(pl~x)),col='green')

#FIND THE OPTIMAL LAYERs
lay <- 10
outs_layer <- NULL
for(i in 1:lay)
{
  nn <- neuralnet(f,
                  data = input_hh_nn,
                  hidden = rep(1,i),
                  act.fct = "logistic",
                  stepmax = 1e+06,
                  linear.output = FALSE,
                  lifesign = "minimal")
  
  pr.nn <- compute(nn, input_hh_nn[, 1:(ncol(input_hh_nn)-3)])
  pr.nn_ <- pr.nn$net.result
  
  original_values <- max.col(input_hh_nn[, (ncol(input_hh_nn)-2):ncol(input_hh_nn)])
  pr.nn_2 <- max.col(pr.nn_)
  outs_layer[i] <- mean(pr.nn_2 == original_values)
}
plot(1-outs_layer,xlab="number of layers", ylab="apparent error")

#CROS VALIDATION

set.seed(500)
k <- 10
outs <- NULL
proportion <- 1-1/k

#Randomly shuffle the data
input_hh_nn <- input_hh_nn[sample(nrow(input_hh_nn)),]
folds <- cut(seq(1,nrow(input_hh_nn)),breaks=k,labels=FALSE)

for(i in 1:k)
{
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test_cv <- input_hh_nn[testIndexes, ]
  train_cv <- input_hh_nn[-testIndexes, ]
  
  nn_cv <- neuralnet(f,
                  data = train_cv,
                  hidden = c(1),
                  act.fct = "logistic",
                  stepmax = 1e+06,
                  linear.output = FALSE,
                  lifesign = "minimal")
  
  pr.nn <- compute(nn_cv, test_cv[, 1:(ncol(test_cv)-3)])
  pr.nn_ <- pr.nn$net.result
  
  original_values <- max.col(test_cv[, (ncol(test_cv)-2):ncol(test_cv)])
  pr.nn_2 <- max.col(pr.nn_)
  
  outs[i] <- mean(pr.nn_2 == original_values)
  
  pred <- factor(as.factor(pr.nn_2),c(1,2,3))
  or <- factor(as.factor(original_values),c(1,2,3))
  
  CM<-confusionMatrix(pred,or)
  CM <- CM$table
  CM
  
}

mean(outs)

#K-means ----
input_hh_k <- input_hh

clust <- kmeans(input_hh_k, 3, nstart = 100)
pred <- clust$cluster
table(pred, ID_type$ACORN_type)
