library(ggplot2)
library(ggthemes)
library(gridExtra)

p1<-ggplot(data3,aes(pop,veg_h_mean))+geom_boxplot(outlier.shape = NA)+theme_base()+xlab("Population")+
  ylab("Vegetation height (cm)")+ scale_y_continuous(breaks=c(0,10,20,30,40,50,60))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  stat_summary(fun.y=mean, geom="point",size=2)

p2<-ggplot(data3,aes(pop,meanT))+geom_boxplot(outlier.shape = NA)+theme_base()+xlab("Population")+
  ylab("Soil temperature")+ scale_y_continuous(breaks=c(14,15,16,17,18,19,20,21))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  stat_summary(fun.y=mean, geom="point",size=2)

p3<-ggplot(data3,aes(pop,n_redants))+geom_boxplot(outlier.shape = NA)+theme_base()+xlab("Population")+
  ylab(expression(paste("Number of ", italic("Myrmica")," ants")))+
  scale_y_continuous(limits=c(0,35),breaks=c(0,5,10,15,20,25,30,35))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  stat_summary(fun.y=mean, geom="point",size=2)

pdf("./results/figures/fig_env_hetero.pdf", width=10,height=3,family = "Times")
grid.arrange(p1,p2,p3, ncol=3)
dev.off()
