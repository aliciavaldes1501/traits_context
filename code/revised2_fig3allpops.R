ggplot(data3, aes(PC1,as.integer(data3$attack)))+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F,size=1,color="black")+
  geom_point(size = 1,color="black")+theme_base()+ylab("Probability of having eggs")+
  xlab("Traits PC1")+  theme(legend.position="none")+
  theme(plot.background=element_rect(fill="white", colour=NA))

ggplot(data3, aes(veg_h_mean,as.integer(data3$attack)))+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F,size=1,color="black")+
  geom_point(size = 1,color="black")+theme_base()+ylab("Probability of having eggs")+
  xlab("Vegetation height (cm)")+theme(legend.position="none")+
  theme(plot.background=element_rect(fill="white", colour=NA))

ggplot(subset(data3,n_eggs_max>0), aes(PC1,n_eggs_max))+
  geom_smooth(method = "glm.nb", se = F,fullrange=T,size=1,color="black")+
  geom_point(size = 1,color="black")+theme_base()+ylab("Number of eggs")+
  xlab("Traits PC1")+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(strip.text.x = element_text(colour="white"),strip.background = element_rect(fill="white"))+
  theme(legend.position="none")
