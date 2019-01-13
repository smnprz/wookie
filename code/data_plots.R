#Some graphs

load(file = "./data/hh_class_input_short.Rda")
input_hh <- as.data.frame(input_hh)


input_hh_plot_or <- input_hh
input_hh_plot_or$ACORN <- ID_type$ACORN_type

input_hh_plot <- input_hh
input_hh_plot <- input_hh_plot[,64:111]
input_hh_plot$ACORN <- ID_type$ACORN_type

input_hh_plot <- gather(input_hh_plot,hour,MW,1:48)
input_hh_plot$hour <- as.numeric(gsub("hh_","",(input_hh_plot$hour)))
input_hh_plot$hour <- as.factor(input_hh_plot$hour)

input_hh_plot_1 <- input_hh_plot %>%
  group_by(ACORN,hour) %>%
  dplyr::summarise(MW=mean(MW))
ggplot(input_hh_plot_1)+
  geom_line(aes(x=hour,y=MW,group=ACORN,colour=ACORN))+
  scale_x_discrete(name ="half_hour",breaks=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47"),
                   labels=c("0","","","","4","","","","8","","","","12","","","","16","","","","20","","","","24","","","","28","","","","32","","","","36","","","","40","","","","44","","","47"))+
  labs(y = "consumption [kWh]",x="half hour")+
  theme(legend.position="bottom",legend.title=element_blank())+
  guides(color=guide_legend(nrow=1))
ggsave("analisys_v0/0_hourly_plot.png",height = 3, width = 3*2.5)

ggplot(input_hh_plot)+
  geom_boxplot(aes(x=ACORN,y=MW*48,fill=ACORN),outlier.size=-1)+
  coord_cartesian(ylim=c(0,30))+
  labs(y = "m_daily_total [kWh-day]",x="Acorn group")+
  guides(fill=FALSE)
ggsave("analisys_v0/0_Acorn_boxplot.png",height = 3, width = 3*2.5)

ggplot(input_hh_plot_or)+
  geom_point(aes(x=m_daily_total,y=daily_sdm_ratio,colour=ACORN),alpha=0.5)+
  theme(legend.position="bottom",legend.title=element_blank())+
  guides(color=guide_legend(nrow=1))+
  labs(y = "std/mean ratio",x="m_daily_total [kWh-day]")
ggsave("analisys_v0/0_std_mean_plot.png")

ggplot(input_hh_plot_or)+
  geom_point(aes(x=m_daily_total,y=dn_ratio,colour=ACORN),alpha=0.5)+
  theme(legend.position="bottom",legend.title=element_blank())+
  guides(color=guide_legend(nrow=1))+
  coord_cartesian(ylim=c(0,5),xlim=c(0,50))+
  labs(y = "dn_ratio",x="m_daily_total")
ggsave("analisys_v0/0_dnratio_mean_plot.png",height = 3, width = 3*2)


