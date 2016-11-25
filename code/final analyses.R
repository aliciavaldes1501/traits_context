#####################################################################################
######################## Final analyses to be included in paper #####################
#####################################################################################

library(ggplot2)
library(MASS)
library(MuMIn)
library(visreg)
library(effects)
library(pscl)
library(lme4)
library(ggthemr)
library(xtable)
library(ggthemes)
library(fmsb)
library(gridExtra)

#5 different models
#Response variables: meanT, phen_index_avg, n_redants, n_eggs_max,cbind(n_intact_fruits,n_fl)
#Distributions of response variables

#Temp ~ veg ###################################################
model_temp<-lm(meanT~scale(veg_h_mean)*pop,data=data3,na.action="na.fail")
summary(model_temp) 
models_temp<-dredge(model_temp)
summary(model.avg(models_temp, subset = delta < 2)) 
print.xtable(xtable(xtable(summary(model.avg(models_temp, subset = delta < 2))),
      digits=c(0,3,3,3,2,3)), type="html",file="./results/tables/model_temp.html")
models_temp[1]
summary(lm(meanT~veg_h_mean*pop,data=data3,na.action="na.fail"))
#Interaction is * but still negative relationship in all 3 pops

model_temp_H<-lm(meanT~scale(veg_h_mean),data=subset(data3,pop=="H")) #*
model_temp_R<-lm(meanT~scale(veg_h_mean),data=subset(data3,pop=="R")) #*
model_temp_T<-lm(meanT~scale(veg_h_mean),data=subset(data3,pop=="T")) #* 
summary(model_temp_H)
summary(model_temp_R)
summary(model_temp_T)

print.xtable(xtable(model_temp_H,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_temp_H.html")
print.xtable(xtable(model_temp_R,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_temp_R.html")
print.xtable(xtable(model_temp_T,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_temp_T.html")

pdf("./results/figures/fig1_temp.pdf", family="Times",width=7,height=3)
ggplot(data3, aes(veg_h_mean,meanT)) + facet_grid(.~population)+
  geom_smooth(method = "lm",  se = T,fullrange=T,size=0.5,color="black")+ylab("Soil temperature")+
  geom_point(size = 1)+ theme_base()+ xlab("Vegetation height (cm)")+theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()


#Phen ~ temp + veg ###################################################
#Using phen_index_avg (cause higher R2 than julian_w3)

model_phen<-lm(phen_index_avg~(scale(meanT)+scale(veg_h_mean))*pop,data=data3,na.action="na.fail")
summary(model_phen)
models_phen<-dredge(model_phen)
summary(model.avg(models_phen, subset = delta < 2)) #Only one
models_phen[1]
model_phen_best<-lm(phen_index_avg~scale(meanT)*pop+scale(veg_h_mean),data=data3,na.action="na.fail")
summary(model_phen_best)

print.xtable(xtable(model_phen_best,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_phen_best.html")

model_phen_H<-lm(phen_index_avg~scale(meanT)+scale(veg_h_mean),data=subset(data3,pop=="H"),na.action="na.fail")
model_phen_R<-lm(phen_index_avg~scale(meanT)+scale(veg_h_mean),data=subset(data3,pop=="R"),na.action="na.fail")
model_phen_T<-lm(phen_index_avg~scale(meanT)+scale(veg_h_mean),data=subset(data3,pop=="T"),na.action="na.fail")
  
summary(model_phen_H)
summary(model_phen_R)
summary(model_phen_T)

print.xtable(xtable(model_phen_H,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_phen_H.html")
print.xtable(xtable(model_phen_R,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_phen_R.html")
print.xtable(xtable(model_phen_T,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_phen_T.html")

p7<-ggplot(data3, aes(meanT,phen_index_avg)) + facet_grid(.~population)+
  geom_smooth(method = "lm",  se = T,fullrange=T,size=0.5,color="black")+ylab("Phenology")+
  geom_point(size = 1)+ theme_base()+ xlab("Soil temperature")+theme(plot.background=element_rect(fill="white", colour=NA))
p8<-ggplot(data3, aes(veg_h_mean,phen_index_avg)) + facet_grid(.~population)+
  geom_smooth(method = "lm",  se = T,fullrange=T,size=0.5,color="black")+ylab("Phenology")+
  geom_point(size = 1)+ theme_base()+ xlab("Vegetation height (cm)")+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(strip.text.x = element_text(colour="white"),strip.background = element_rect(fill="white"))
pdf("./results/figures/fig2_phen.pdf", family="Times",width=7,height=6)
multiplot(p7,p8,cols=1)
dev.off()

#Not including n_fl as covariate (= measure of resource state / size) 
#cause not correlated (low values) to phen within populations
with(data3,cor(phen_index_avg,n_fl)) #0.47
with(subset(data3,pop=="H"),cor(phen_index_avg,n_fl)) #-0.25
with(subset(data3,pop=="R"),cor(phen_index_avg,n_fl)) #↓0.30
with(subset(data3,pop=="T"),cor(phen_index_avg,n_fl)) #0.28


#Ants ~ temp + veg ###################################################
#Not using n_redants_mean because it is not an integer (warning with glm.nb)
model_ants<-glm.nb(n_redants~(scale(veg_h_mean)*scale(meanT))*pop,na.action="na.fail",data=data3)
models_ants<-dredge(model_ants)
summary(model.avg(models_ants, subset = delta < 2))
print.xtable(xtable(xtable(summary(model.avg(models_ants, subset = delta < 2))),
     digits=c(0,3,3,3,2,3)), type="html",file="./results/tables/model_ants.html")
models_ants[1]
model_ants_best<-glm.nb(n_redants~scale(meanT)*scale(veg_h_mean)+pop+pop:scale(veg_h_mean),data3)
summary(model_ants_best)
r.squaredLR(model_ants_best)
NagelkerkeR2(model_ants_best)

#Plot interaction for the best model
plot(effect(term="meanT:veg_h_mean", mod=model_ants_best,
     xlevels=list(veg_h_mean=5:60, meanT=seq(14,20,1))),multiline=T,type="response")

int_ants<-data.frame(effect(term="meanT:veg_h_mean", mod=model_ants_best,
           xlevels=list(veg_h_mean=0:60, meanT=seq(14,20,1))))

pdf("./results/figures/fig3_ants.pdf", family="Times",width=4,height=3.5)
ggplot(int_ants, aes(veg_h_mean,fit, group = as.factor(meanT))) +
  geom_line(size=0.3)+theme_base()+xlab("Vegetation height (cm)")+
  ylab(expression(paste("Number of ", italic("Myrmica")," ants")))+
  coord_cartesian(xlim=c(0,72),ylim=c(0,40))+theme(legend.position="none")+
  annotate("text", x = 66.5, y = 28.3, label = "20 ºC")+
  annotate("text", x = 66.5, y = 19.3, label = "19 ºC")+
  annotate("text", x = 66.5, y = 13, label = "18 ºC")+
  annotate("text", x = 66.5, y = 9, label = "17 ºC")+
  annotate("text", x = 66.5, y = 6.3, label = "16 ºC")+
  annotate("text", x = 66.5, y = 4.4, label = "15 ºC")+
  annotate("text", x = 66.5, y = 2.6, label = "14 ºC")+
  theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()
  
pdf("./results/figures/fig3_ants_lines.pdf", family="Times",width=4,height=3.5)
ggplot(int_ants, aes(veg_h_mean,fit, group = as.factor(meanT))) +
  geom_line(size=0.3,aes(veg_h_mean,fit,linetype=as.factor(meanT)))+theme_base()+xlab("Vegetation height (cm)")+
  ylab(expression(paste("Number of ", italic("Myrmica")," ants")))+
  coord_cartesian(xlim=c(0,72),ylim=c(0,40))+theme(legend.position="none")+
  annotate("text", x = 66.5, y = 28.3, label = "20 ºC")+
  annotate("text", x = 66.5, y = 19.3, label = "19 ºC")+
  annotate("text", x = 66.5, y = 13, label = "18 ºC")+
  annotate("text", x = 66.5, y = 9, label = "17 ºC")+
  annotate("text", x = 66.5, y = 6.3, label = "16 ºC")+
  annotate("text", x = 66.5, y = 4.4, label = "15 ºC")+
  annotate("text", x = 66.5, y = 2.6, label = "14 ºC")+
  theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()

#Not including models for ants for each population 
#because interactions with population are not significant


#Eggs ~ phen + over_veg + ants ###################################################

data3$over_veg<-data3$shoot_h-data3$veg_h_mean
#Positive values = shoot over the vegetation
#Negative values = shoot under the vegetation

model_eggs<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(over_veg)+scale(n_redants))*pop,data=data3,dist="negbin",zero.dist="binomial",na.action="na.fail")
models_eggs<-dredge(model_eggs)
summary(model.avg(models_eggs, subset = delta < 2))
#Only one model
models_eggs[1]
model_eggs_best<-hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg),data=data3,dist="negbin",zero.dist="binomial",na.action="na.fail")
summary(model_eggs_best)
#xtable not working with hurdle

#Poisson model had an effect on ants on the count part, but was strongly overdispersed :(
#And negbin pointed as better by AIC comparisons and vuong test

nullmodel_eggs<-hurdle(n_eggs_max~1,data=data3,dist="negbin",zero.dist="binomial",na.action="na.fail")
#. Percent deviance explained, defined as [D(null model) - D(fitted model)] / D(null model)
2*logLik(model_eggs)     # Deviance model_eggs
2*logLik(nullmodel_eggs)  # Deviance null model
(-1459.397-(-1402.661))/-1459.397 # Only 3.8% !?

# model_eggs_H<-hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg)+scale(n_redants),data=subset(data3,pop=="H"),
#                dist="negbin",zero.dist="binomial",na.action="na.fail")
# model_eggs_R<-hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg)+scale(n_redants),data=subset(data3,pop=="R"),
#                dist="negbin",zero.dist="binomial",na.action="na.fail")
# model_eggs_T<-hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg)+scale(n_redants),data=subset(data3,pop=="T"),
#                dist="negbin",zero.dist="binomial",na.action="na.fail")
# summary(model_eggs_H)
# summary(model_eggs_R)
# summary(model_eggs_T)
# models_eggs_H<-dredge(model_eggs_H)
# models_eggs_R<-dredge(model_eggs_R)
# models_eggs_T<-dredge(model_eggs_T)
# summary(model.avg(models_eggs_H, subset = delta < 2))
# summary(model.avg(models_eggs_R, subset = delta < 2))
# summary(model.avg(models_eggs_T, subset = delta < 2))

#Not including models for eggs for each population 
#because interactions with population are not significant

data3$attack<-with(data3,ifelse(n_eggs_max>0,1,0))

p9<-ggplot(data3, aes(over_veg,as.integer(data3$attack)))+facet_grid(.~population)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = T,size=0.5,color="black")+
  geom_point(size = 1)+theme_base()+ylab("Probability of having eggs")+
  xlab("Difference betwen shoot and vegetation height")+
  theme(plot.background=element_rect(fill="white", colour=NA))
p10<-ggplot(subset(data3,n_eggs_max>0), aes(phen_index_avg,n_eggs_max))+facet_grid(.~population)+
  geom_smooth(method = "glm.nb", se = T,size=0.5,color="black")+
  geom_point(size = 1)+theme_base()+ylab("Number of eggs")+
  xlab("Phenology")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(strip.text.x = element_text(colour="white"),strip.background = element_rect(fill="white"))
pdf("./results/figures/fig4_eggs.pdf", family="Times",width=7,height=6)
multiplot(p9,p10,cols=1)
dev.off()


#N_intact_fruits relative to n_fl #############################################

data3$fruit_set<-data3$n_intact_fruits/data3$n_fl
model_fruitset<-glm(cbind(n_intact_fruits,n_fl)~(scale(phen_index_avg)+scale(n_eggs_max)+scale(meanT)+scale(over_veg))*pop,
              family="binomial",data=data3,na.action="na.fail")
summary(model_fruitset)
models_fruitset<-dredge(model_fruitset)
summary(model.avg(models_fruitset,subset=delta<2)) # Phen, eggs and pop *, no interactions
#Could also remove Remmene cause only 14 plants where fruitset>0

print.xtable(xtable(xtable(summary(model.avg(models_fruitset, subset = delta < 2))),
    digits=c(0,3,3,3,2,3)), type="html",file="./results/tables/model_fruitset.html")
models_fruitset[1]
model_fruitset_best<-glm(cbind(n_intact_fruits,n_fl)~scale(phen_index_avg)+scale(n_eggs_max)*pop+scale(over_veg),
            family="binomial",data=data3)
summary(model_fruitset_best)
r.squaredLR(model_fruitset_best)
NagelkerkeR2(model_fruitset_best)

p11<-ggplot(data3, aes(phen_index_avg, fruit_set)) + geom_point(size = 1) + facet_grid(.~population)+
  geom_smooth(method = "glm", method.args=list(family = "binomial"), aes(weight=n_fl),size=0.5,color="black")+
  theme_base()+xlab("Phenology")+ylab(NULL)+
  theme(plot.background=element_rect(fill="white", colour=NA))
p12<-ggplot(data3, aes(n_eggs_max, fruit_set)) + geom_point(size = 1) + facet_grid(.~population)+
  geom_smooth(method = "glm", method.args=list(family = "binomial"), aes(weight=n_fl),size=0.5,color="black")+
  theme_base()+xlab("Number of eggs")+ylab(NULL)+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(strip.text.x = element_text(colour="white"),strip.background = element_rect(fill="white"))
pdf("./results/figures/fig5_fruitset.pdf", family="Times",width=7,height=5.5)
grid.arrange(p11, p12,left = textGrob("Number of intact fruits / Number of flowers", rot = 90,gp=gpar(fontsize=16)))
dev.off()


