# Load libraries -----
library(dplyr)
library(broom)
library(caret)
library(modelr)
library(neuralnet)

# Load and standarize data----
load(file = "./data/hh_class_input_short.Rda")
#load(file = "../Data/SML_data/tidy_data/hh_class_input.Rda")

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

d <- dist(input_data,method="euclidean")

#methodHC <- "single"
#methodHC <- "median"
methodHC <- "average"


h1 <- hclust(d,method=methodHC)
clusterCut1 <- cutree(h1, 3)
table(clusterCut1)

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

input_hh_logit <- input_hh_scale
#input_hh_logit <- input_hh_norm
#input_hh_logit <- input_hh

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
pred_logit_class<-as.data.frame(max.col(pred_logit))
pred_logit_class_mat<-pred_logit*0
pred_logit_class_mat[pred_logit_class==1,1]<-1
pred_logit_class_mat[pred_logit_class==2,2]<-1
pred_logit_class_mat[pred_logit_class==3,3]<-1

#confusionMatrix(as.factor(output_hh_adv),as.factor(pred_logit_class_mat[,1]))
#confusionMatrix(as.factor(output_hh_aff),as.factor(pred_logit_class_mat[,2]))
#confusionMatrix(as.factor(output_hh_comf),as.factor(pred_logit_class_mat[,3]))

output_class <- pred_logit_class*0
output_class[ID_type$ACORN_type=="Adversity",]<-1
output_class[ID_type$ACORN_type=="Affluent",]<-2
output_class[ID_type$ACORN_type=="Comfortable",]<-3

effec_logit <- output_class*0
effec_logit[output_class==1&pred_logit_class==1,]<-1
effec_logit[output_class==2&pred_logit_class==2,]<-1
effec_logit[output_class==3&pred_logit_class==3,]<-1

sum(effec_logit)/nrow(effec_logit)

pred_actual <- pred_logit_class
pred_actual$actual <- output_class[,1]
colnames(pred_actual) <- c("pred","actual")

pred_actual1 <- as.data.frame(pred_actual[pred_actual$pred==1,]$actual)
colnames(pred_actual1) <- c("actual")

pred_actual1 <- pred_actual1 %>%
  group_by(actual) %>%
  dplyr::summarise(n=n())

pred_actual2 <- as.data.frame(pred_actual[pred_actual$pred==2,]$actual)
colnames(pred_actual2) <- c("actual")

pred_actual2 <- pred_actual2 %>%
  group_by(actual) %>%
  dplyr::summarise(n=n())

pred_actual3 <- as.data.frame(pred_actual[pred_actual$pred==3,]$actual)
colnames(pred_actual3) <- c("actual")

pred_actual3 <- pred_actual3 %>%
  group_by(actual) %>%
  dplyr::summarise(n=n())

pred_actual_confusion <- as.data.frame(pred_actual1$n)
pred_actual_confusion$affluent <- pred_actual2$n
pred_actual_confusion$comfortable <- pred_actual3$n
colnames(pred_actual_confusion)[1] <- c("adversity")
sum(diag(as.matrix(pred_actual_confusion)))/sum(pred_actual_confusion)

#Neural networks----
input_hh_nn <- input_hh_scale

f <- reformulate(names(input_hh_nn),response = "output")

input_hh_nn$output <- output_hh_adv
#input_hh_nn$output <- output_hh_aff
#input_hh_nn$output <- output_hh_comf

f <- reformulate(names(input_hh_nn[,1:2]),response = "output")

nn_1 <- neuralnet(formula = f,
                  data = input_hh_nn,
                  stepmax = 1e+06,
                  hidden = c(2,2,2),
                  act.fct = "logistic")

input_hh_nn$output_pred<-compute(nn_1,input_hh_nn[,-ncol(input_hh_nn)])$net.result
input_hh_nn$output_pred<-round(input_hh_nn$output_pred)
confusion <- table(input_hh_nn$output_pred, input_hh_nn$output)
accuracy222 <- sum(diag(confusion))/sum(confusion)

confusionMatrix(as.factor(input_hh_nn$output),as.factor(input_hh_nn$output_pred))
