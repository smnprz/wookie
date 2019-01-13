library(ggplot2)
library(reshape)
library(plotly)

Sys.setenv("plotly_username"="simonperez200")
Sys.setenv("plotly_api_key"="oydkFgJZ93Ti0jGA0A92")

# HEATMAP ----

hh_or <- input_hh[,64:(64+47)]
hh <- hh_or
hh <- hh/rowMaxs(as.matrix(hh))
hh$ID <- ID_type$ID
hh$ACORN <- ID_type$ACORN_type
hh <- hh[order(hh$ACORN),]
hh$ID_temp <- 1:nrow(hh)
s1 <- sample(hh[hh$ACORN=="Adversity",]$ID_temp,50) 
s2 <- sample(hh[hh$ACORN=="Affluent",]$ID_temp,50)
s3 <- sample(hh[hh$ACORN=="Comfortable",]$ID_temp,50)
s <- c(s1,s2,s3)
hh <- hh[s,]
hh$n_ID <- 1:nrow(hh)
hh$n_ID <- as.factor(hh$n_ID)
hh <- hh[,c(52,1:48)]
#hh <- hh[1:100,]
hh.m <- melt(hh)
hh.m$variable <- as.numeric(gsub("hh_","",(hh.m$variable)))

p <- ggplot(hh.m, aes(variable, n_ID))+
  geom_tile(aes(fill = value),colour = "white")+
  scale_fill_gradient(low = "white",high = "steelblue")+
  #scale_y_discrete(expand=c(0,0),breaks=c(""))+
  labs(x="half_hour",y="household")+
  #scale_x_discrete(name ="hour",breaks=c("hh_0","hh_1","hh_2","hh_3","hh_4","hh_5","hh_6","hh_7","hh_8","hh_9","hh_10","hh_11","hh_12","hh_13","hh_14","hh_15","hh_16","hh_17","hh_18","hh_19","hh_20","hh_21","hh_22","hh_23","hh_24","hh_25","hh_26","hh_27","hh_28","hh_29","hh_30","hh_31","hh_32","hh_33","hh_34","hh_35","hh_36","hh_37","hh_38","hh_39","hh_40","hh_41","hh_42","hh_43","hh_44","hh_45","hh_46","hh_47"),
  #                 labels=c("0","","","","2","","","","4","","","","6","","","","8","","","","10","","","","12","","","","14","","","","16","","","","18","","","","20","","","","22","","",""))#+
  geom_hline(yintercept=50, linetype="dashed", color = "red", size=1)+
  geom_hline(yintercept=100, linetype="dashed", color = "red", size=1)+
  guides(fill=guide_legend(title="consumption (max=1)"))+
  theme(legend.position="bottom")
  
  #scale_x_discrete(expand=c(0,0),breaks=c(""))
  
p

p<-ggplotly(p, tooltip = c("city"))
api_create(p, filename = "heatmap_consumptiom_LCL")


# Consumption per group ----

hh_or <- input_hh[,64:(64+47)]
hh <- hh_or

for (i in c(1:24)){
  print(i)
  hh[,i] <- hh[,i*2-1] + hh[,i*2]
}
hh <- hh[,1:24]

hh_max <- as.data.frame(rowMaxs(as.matrix(hh)))
hh <- hh/rowMaxs(as.matrix(hh))
hh$ID <- ID_type$ID
hh_max$ID <- hh$ID
hh$ACORN <- ID_type$ACORN_type

hh$ID_temp <- 1:nrow(hh)
s1 <- sample(hh[hh$ACORN=="Adversity",]$ID_temp,300) 
s2 <- sample(hh[hh$ACORN=="Affluent",]$ID_temp,300)
s3 <- sample(hh[hh$ACORN=="Comfortable",]$ID_temp,300)
s <- c(s1,s2,s3)
hh <- hh[s,]

#hh <- hh[,c(49,1:48)]
hh <- hh[,c(25,1:24)]
hh.m <- melt(hh)
hh.m$variable <- as.numeric(gsub("hh_","",(hh.m$variable)))
hh.m <- merge(hh.m,ID_type[,c("ID","ACORN_type")],by = "ID",all.x = TRUE)
hh.m <- merge(hh.m,hh_max,by = "ID",all.x = TRUE)
hh.m$consumption <- hh.m$value*hh.m$`rowMaxs(as.matrix(hh))`
hh.m <- hh.m[order(hh.m$variable),]
hh.m$hour <- factor(hh.m$variable)
hh.m$hour <- factor(hh.m$hour,levels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47"))
hh.m$hour <- as.numeric(as.character(hh.m$hour))

p <- plot_ly(hh.m, x = ~hour, y = ~consumption, color = ~ACORN_type, type = "box") %>%
  layout(boxmode = "group",yaxis = list(title="consumption [kWh]",range = c(0, 2)))

p
api_create(p, filename="consumption_group")
