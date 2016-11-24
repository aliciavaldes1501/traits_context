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

#5 different models
#Response variables: meanT, phen_index_avg, n_redants, n_eggs_max,cbind(n_intact_fruits,n_fl)
#Distributions of response variables

#Temp ~ veg ###################################################
model_temp<-lm(meanT~veg_h_mean*pop,data=data3,na.action="na.fail")
summary(model_temp) 
models_temp<-dredge(model_temp)
summary(model.avg(models_temp, subset = delta < 2)) 
print.xtable(xtable(xtable(summary(model.avg(models_temp, subset = delta < 2))),
      digits=c(0,3,3,3,2,3)), type="html",file="./results/tables/model_temp.html")
models_temp[1]
summary(lm(meanT~veg_h_mean*pop,data=data3,na.action="na.fail"))
#Interaction is * but still negative relationship in all 3 pops

model_temp_H<-lm(meanT~veg_h_mean,data=subset(data3,pop=="H")) #*
model_temp_R<-lm(meanT~veg_h_mean,data=subset(data3,pop=="R")) #*
model_temp_T<-lm(meanT~veg_h_mean,data=subset(data3,pop=="T")) #* 
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

model_phen<-lm(phen_index_avg~(meanT+veg_h_mean)*pop,data=data3,na.action="na.fail")
summary(model_phen)
models_phen<-dredge(model_phen)
summary(model.avg(models_phen, subset = delta < 2)) #Only one
models_phen[1]
model_phen_best<-lm(phen_index_avg~meanT*pop+veg_h_mean,data=data3,na.action="na.fail")
summary(model_phen_best)

print.xtable(xtable(model_phen_best,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_phen_best.html")

model_phen_H<-lm(phen_index_avg~meanT+veg_h_mean,data=subset(data3,pop=="H"),na.action="na.fail")
model_phen_R<-lm(phen_index_avg~meanT+veg_h_mean,data=subset(data3,pop=="R"),na.action="na.fail")
model_phen_T<-lm(phen_index_avg~meanT+veg_h_mean,data=subset(data3,pop=="T"),na.action="na.fail")
  
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
#cause not correlated (low values) to phen within populatiosn
with(data3,cor(phen_index_avg,n_fl)) #0.47
with(subset(data3,pop=="H"),cor(phen_index_avg,n_fl)) #-0.25
with(subset(data3,pop=="R"),cor(phen_index_avg,n_fl)) #↓0.30
with(subset(data3,pop=="T"),cor(phen_index_avg,n_fl)) #0.28


#Ants ~ temp + veg ###################################################

model_ants<-glm.nb(n_redants~(veg_h_mean*meanT)*pop,na.action="na.fail",data=data3)
models_ants<-dredge(model_ants)
summary(model.avg(models_ants, subset = delta < 2))
print.xtable(xtable(xtable(summary(model.avg(models_ants, subset = delta < 2))),
     digits=c(0,3,3,3,2,3)), type="html",file="./results/tables/model_ants.html")
models_ants[1]
model_ants_best<-glm.nb(n_redants~meanT*veg_h_mean+pop+pop:veg_h_mean,data3)
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





model_ants_H<-glm.nb(n_redants~veg_h_mean+meanT+meanT:veg_h_mean,data=subset(data3,pop=="H"),na.action="na.fail")
model_ants_R<-glm.nb(n_redants~veg_h_mean+meanT+meanT:veg_h_mean,data=subset(data3,pop=="R"),na.action="na.fail")
model_ants_T<-glm.nb(n_redants~veg_h_mean+meanT+meanT:veg_h_mean,data=subset(data3,pop=="T"),na.action="na.fail")
summary(model_ants_H)
summary(model_ants_R)
summary(model_ants_T)

with(subset(data3,pop=="H"),min(veg_h_mean))
with(subset(data3,pop=="H"),max(veg_h_mean))
with(subset(data3,pop=="R"),min(veg_h_mean))
with(subset(data3,pop=="R"),max(veg_h_mean))
with(subset(data3,pop=="T"),min(veg_h_mean))
with(subset(data3,pop=="T"),max(veg_h_mean))
with(subset(data3,pop=="H"),min(meanT))
with(subset(data3,pop=="H"),max(meanT))
with(subset(data3,pop=="R"),min(meanT))
with(subset(data3,pop=="R"),max(meanT))
with(subset(data3,pop=="T"),min(meanT))
with(subset(data3,pop=="T"),max(meanT))

#Not including models for ants for each population 
#because interactions with population are not significant










############################################################################################
# OK till here
############################################################################################

#Eggs
#phen_index_avg

model1<-(glm.nb(n_eggs_max~(phen_index_avg+veg_h_mean+n_redants)*pop,data=data6,na.action="na.fail"))
model2<-(glm.nb(n_eggs_max~(phen_index_avg+n_fl+veg_h_mean+n_redants)*pop,data=data6,na.action="na.fail"))
model3<-(glm.nb(n_eggs_max~(phen_index_avg+n_fl+shoot_h+veg_h_mean+n_redants)*pop,data=data6,na.action="na.fail"))
model4<-(glm.nb(n_eggs_max~(phen_index_avg+shoot_h+veg_h_mean+n_redants)*pop,data=data6,na.action="na.fail"))

models1<-dredge(model1)
models2<-dredge(model2)
models3<-dredge(model3)
models4<-dredge(model4)

summary(model.avg(models1, subset = delta < 2))
summary(model.avg(models2, subset = delta < 2))
summary(model.avg(models3, subset = delta < 2))
summary(model.avg(models4, subset = delta < 2))
models4[1]
summary(glm.nb(n_eggs_max~n_redants+pop+shoot_h+veg_h_mean+n_redants:pop+pop:veg_h_mean,data6))

summary(glm.nb(n_eggs_max~phen_index_avg+veg_h_mean+n_redants,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~phen_index_avg+veg_h_mean+n_redants,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~phen_index_avg+veg_h_mean+n_redants,data=subset(data6,pop=="T"),na.action="na.fail"))

summary(glm.nb(n_eggs_max~phen_index_avg+n_fl+veg_h_mean+n_redants,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~phen_index_avg+n_fl+veg_h_mean+n_redants,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~phen_index_avg+n_fl+veg_h_mean+n_redants,data=subset(data6,pop=="T"),na.action="na.fail"))

summary(glm.nb(n_eggs_max~phen_index_avg+n_fl+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~phen_index_avg+n_fl+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~phen_index_avg+n_fl+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="T"),na.action="na.fail"))

summary(glm.nb(n_eggs_max~phen_index_avg+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~phen_index_avg+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~phen_index_avg+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="T"),na.action="na.fail"))

summary(glm.nb(n_eggs_max~phen_index_avg,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~phen_index_avg,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~phen_index_avg,data=subset(data6,pop=="T"),na.action="na.fail"))


#julian_w3

model1b<-(glm.nb(n_eggs_max~(julian_w3+veg_h_mean+n_redants)*pop,data=data6,na.action="na.fail"))
model2b<-(glm.nb(n_eggs_max~(julian_w3+n_fl+veg_h_mean+n_redants)*pop,data=data6,na.action="na.fail"))
model3b<-(glm.nb(n_eggs_max~(julian_w3+n_fl+shoot_h+veg_h_mean+n_redants)*pop,data=data6,na.action="na.fail"))
model4b<-(glm.nb(n_eggs_max~(julian_w3+shoot_h+veg_h_mean+n_redants)*pop,data=data6,na.action="na.fail"))

models1b<-dredge(model1b)
models2b<-dredge(model2b)
models3b<-dredge(model3b)
models4b<-dredge(model4b)

summary(model.avg(models1b, subset = delta < 2))
summary(model.avg(models2b, subset = delta < 2))
summary(model.avg(models3b, subset = delta < 2))
summary(model.avg(models4b, subset = delta < 2))

summary(glm.nb(n_eggs_max~julian_w3+veg_h_mean+n_redants,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~julian_w3+veg_h_mean+n_redants,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~julian_w3+veg_h_mean+n_redants,data=subset(data6,pop=="T"),na.action="na.fail"))

summary(glm.nb(n_eggs_max~julian_w3+n_fl+veg_h_mean+n_redants,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~julian_w3+n_fl+veg_h_mean+n_redants,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~julian_w3+n_fl+veg_h_mean+n_redants,data=subset(data6,pop=="T"),na.action="na.fail"))

summary(glm.nb(n_eggs_max~julian_w3+n_fl+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~julian_w3+n_fl+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~julian_w3+n_fl+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="T"),na.action="na.fail"))

summary(glm.nb(n_eggs_max~julian_w3+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~julian_w3+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~julian_w3+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="T"),na.action="na.fail"))

summary(glm.nb(n_eggs_max~julian_w3,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~julian_w3,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_eggs_max~julian_w3,data=subset(data6,pop=="T"),na.action="na.fail"))

#Model selection within each population

model1H<-glm.nb(n_eggs_max~julian_w3+n_fl+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="H"),na.action="na.fail")
model1R<-glm.nb(n_eggs_max~julian_w3+n_fl+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="R"),na.action="na.fail")
model1T<-glm.nb(n_eggs_max~julian_w3+n_fl+shoot_h+veg_h_mean+n_redants,data=subset(data6,pop=="R"),na.action="na.fail")

models1H<-dredge(model1H)
models1R<-dredge(model1R)
models1T<-dredge(model1T)

summary(model.avg(models1H, subset = delta < 2))
summary(model.avg(models1R, subset = delta < 2))
summary(model.avg(models1T, subset = delta < 2))


####################################################################################
#N eggs / fl_n

data6$neggsfl<-data6$n_eggs_max/data6$n_fl
with(data6,hist(neggsfl))

model1c<-(glm.nb(neggsfl~(phen_index_avg+veg_h_mean+n_redants)*pop,data=data6,na.action="na.fail"))
model2c<-(glm.nb(neggsfl~(phen_index_avg+over_veg+n_redants)*pop,data=data9,na.action="na.fail"))
# Warnings: non-integer values
models1c<-dredge(model1c)
models2c<-dredge(model2c)

summary(model.avg(models1c, subset = delta < 2))
summary(model.avg(models2c, subset = delta < 2))


######################################################################################
# Attack
data6$attack<-as.factor(with(data6,ifelse(n_eggs_max>0,"1","0")))

model1d<-(glm(attack~(phen_index_avg+veg_h_mean+n_redants)*pop,data=data6,family="binomial",na.action="na.fail"))
model2d<-(glm(attack~(phen_index_avg+n_fl+veg_h_mean+n_redants)*pop,data=data6,family="binomial",na.action="na.fail"))
model3d<-(glm(attack~(phen_index_avg+shoot_h+veg_h_mean+n_redants)*pop,data=data6,family="binomial",na.action="na.fail"))
model4d<-(glm(attack~(phen_index_avg+n_fl+shoot_h+veg_h_mean+n_redants)*pop,data=data6,family="binomial",na.action="na.fail"))

models1d<-dredge(model1d)
models2d<-dredge(model2d)
models3d<-dredge(model3d)
models4d<-dredge(model4d)

summary(model.avg(models1d, subset = delta < 2))
summary(model.avg(models2d, subset = delta < 2))
summary(model.avg(models3d, subset = delta < 2))
summary(model.avg(models4d, subset = delta < 2))


######################################################################################
# Proportion of buds with eggs
buds_eggs<-read.table("./data/clean/buds_eggs.txt",header=T,sep=",",dec=".")
head(buds_eggs)
str(buds_eggs) #309 obs

data7<-merge(data6,buds_eggs,by="id_pl")
str(data7)

model1e<-(glm(cbind(buds_e,buds_ne)~(phen_index_avg+veg_h_mean+n_redants)*pop,data=data7,family="binomial",na.action="na.fail"))
model2e<-(glm(cbind(buds_e,buds_ne)~(phen_index_avg+n_fl+veg_h_mean+n_redants)*pop,data=data7,family="binomial",na.action="na.fail"))
model3e<-(glm(cbind(buds_e,buds_ne)~(phen_index_avg+shoot_h+veg_h_mean+n_redants)*pop,data=data7,family="binomial",na.action="na.fail"))
model4e<-(glm(cbind(buds_e,buds_ne)~(phen_index_avg+n_fl+shoot_h+veg_h_mean+n_redants)*pop,data=data7,family="binomial",na.action="na.fail"))
model5e<-(glm(cbind(buds_e,buds_ne)~(phen_index_avg+over_veg+n_redants)*pop,data=data9,family="binomial",na.action="na.fail"))

models1e<-dredge(model1e)
models2e<-dredge(model2e)
models3e<-dredge(model3e)
models4e<-dredge(model4e)
models5e<-dredge(model5e)

summary(model.avg(models1e, subset = delta < 2))
summary(model.avg(models2e, subset = delta < 2))
summary(model.avg(models3e, subset = delta < 2))
summary(model.avg(models4e, subset = delta < 2))
summary(model.avg(models5e, subset = delta < 2))

data7$prop_buds_e<-data7$buds_e/(data7$buds_e+data7$buds_ne)

ggplot(data7, aes(phen_index_avg, prop_buds_e))+
  geom_smooth(method = "glm",  method.args = list(family = "binomial"), se = T,colour="black")+
  geom_point(size = 1)+facet_grid(.~population)+ 
  theme(legend.position="none")+theme_bw()


summary(glm.nb(n_eggs_max~redants_pres,data=subset(data7,pop=="H"&n_eggs_max>0)))
summary(glm.nb(n_eggs_max~redants_pres,data=subset(data7,pop=="R"&n_eggs_max>0)))
summary(glm.nb(n_eggs_max~redants_pres,data=subset(data8,pop=="T"&n_eggs_max>0)))



# Ants present more than once
days_ant_pres<-read.table("./data/clean/days_ant_pres.txt",header=T,sep=",",dec=".")
head(days_ant_pres)
str(days_ant_pres) 

data8<-merge(data7,days_ant_pres,by="id_pl")
str(data8)
with(data8,plot(n_redants~as.factor(days_ant_pres)))
with(data8,summary(aov(n_redants~as.factor(days_ant_pres))))

#Put n_redantsC=0 and redants_presC=0 when days_ant_pres=1
data8$n_redantsC<-ifelse(data8$days_ant_pres==1,0,data8$n_redants)
data8$redants_pres<-ifelse(data8$days_ant_pres<1,0,1)
data8$redants_presC<-ifelse(data8$days_ant_pres<2,0,1)
data8$redants_presC<-as.factor(data8$redants_presC)
data8$redants_pres<-as.factor(data8$redants_pres)

with(data8,plot(redants_pres,n_eggs_max))
with(data8,plot(redants_presC,n_eggs_max))

summary(glm(cbind(buds_e,buds_ne)~n_fl*n_redantsC,family="binomial",data=subset(data8,pop=="H")))
summary(glm(cbind(buds_e,buds_ne)~n_fl*n_redantsC,family="binomial",data=subset(data8,pop=="R")))
summary(glm(cbind(buds_e,buds_ne)~n_fl*n_redantsC,family="binomial",data=subset(data8,pop=="T")))

summary(glm(cbind(buds_e,buds_ne)~redants_presC,family="binomial",data=subset(data8,n_eggs_max>0)))
            
            
###########################################################################################
# As done in paper

model2<-glm.nb(n_eggs_max~(phen_index_avg+n_fl+shoot_h+veg_h_mean+n_redants)*
        pop,na.action = "na.fail",data=data8)
model2<-glm.nb(n_eggs_max~(phen_index_avg+n_fl+shoot_h+veg_h_mean+n_redants)*
                 pop,na.action = "na.fail",data=subset(data8,n_eggs_max>0))
summary(model2)
models2<-dredge(model2)
summary(model.avg(models2, subset = delta < 2)) #Only n_fl is *

summary(glm.nb(n_eggs_max~phen_index_avg+pop,data=data8)) #OK signif and no int with pop
summary(glm.nb(n_eggs_max~julian_w3+pop,data=data8)) #NS
plot(glm.nb(n_eggs_max~phen_index_avg+pop,data=data8))
summary(glm(n_eggs_max~phen_index_avg+pop,data=data8,family="quasipoisson")) #Overdisp
summary(glm(n_eggs_max~phen_index_avg+pop,data=data8,family="poisson")) #Overdisp


#zeroinfl or hurdle
summary(zeroinfl(n_eggs_max~phen_index_avg+pop|1,data=data8,dist="poisson"))
summary(zeroinfl(n_eggs_max~phen_index_avg+pop|1,data=data8,dist="negbin"))
summary(zeroinfl(n_eggs_max~phen_index_avg+pop,data=data8,dist="poisson"))
summary(zeroinfl(n_eggs_max~phen_index_avg+pop,data=data8,dist="negbin"))

summary(zeroinfl(n_eggs_max~julian_w3+pop,data=data8,dist="poisson"))
summary(zeroinfl(n_eggs_max~julian_w3+pop,data=data8,dist="negbin"))

summary(zeroinfl(n_eggs_max~phen_index_avg+n_redants+pop,data=data8,dist="poisson"))
summary(zeroinfl(n_eggs_max~phen_index_avg+n_redants+pop,data=data8,dist="negbin"))

summary(hurdle(n_eggs_max~phen_index_avg+n_redants+pop,data=data8,dist="poisson",zero.dist="binomial"))
summary(hurdle(n_eggs_max~phen_index_avg+n_redants+pop,data=data8,dist="negbin",zero.dist="binomial"))

summary(hurdle(n_eggs_max~julian_w3+n_redants+pop,data=data8,dist="poisson",zero.dist="binomial"))
summary(hurdle(n_eggs_max~julian_w3+n_redants+pop,data=data8,dist="negbin",zero.dist="binomial"))

modelA<-hurdle(n_eggs_max~phen_index_avg+n_redants+pop,data=data8,dist="poisson")
modelB<-hurdle(n_eggs_max~phen_index_avg+n_redants+pop,data=data8,dist="negbin")

vuong(modelA, modelB) 
# Negative binomial is better
# After removing the zeros, there still seems to be overdispersion in n_eggs_max


# Good stuff starts from here!

modelA<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(n_fl)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants))*pop,data=data8,dist="poisson",zero.dist="binomial",na.action="na.fail")
modelsA<-dredge(modelA)
summary(model.avg(modelsA, subset = delta < 2))
###############################################
modelAbis<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(n_fl)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants))*pop,data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsAbis<-dredge(modelAbis)
summary(model.avg(modelsAbis, subset = delta < 2))

modelB<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(veg_h_mean)+scale(n_redants))*pop,data=data8,dist="poisson",zero.dist="binomial",na.action="na.fail")
modelsB<-dredge(modelB)
summary(model.avg(modelsB, subset = delta < 2))
###############################################
modelBbis<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(veg_h_mean)+scale(n_redants))*pop,data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsBbis<-dredge(modelBbis)
summary(model.avg(modelsBbis, subset = delta < 2))

data8$over_veg<-data8$shoot_h-data8$veg_h_mean

modelC<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(over_veg)+scale(n_redants))*pop,data=data8,dist="poisson",zero.dist="binomial",na.action="na.fail")
modelsC<-dredge(modelC)
summary(model.avg(modelsC, subset = delta < 2))
summary(hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg)+scale(n_redants)+pop+
                 scale(n_redants):pop,data=data8,dist="poisson",zero.dist="binomial",na.action="na.fail"))
# Favourite by now!
modelCbis<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(over_veg)+scale(n_redants))*pop,data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsCbis<-dredge(modelCbis)
summary(model.avg(modelsCbis, subset = delta < 2))
modelsCbis[1]
summary(hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg)
                 ,data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail"))

######################

modelD<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants))*pop,data=data8,dist="poisson",zero.dist="binomial",na.action="na.fail")
modelsD<-dredge(modelD)
summary(model.avg(modelsD, subset = delta < 2)) 
###############################################
modelDbis<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(shoot_h)*scale(veg_h_mean)+scale(n_redants))*pop,data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsDbis<-dredge(modelDbis)
summary(model.avg(modelsDbis, subset = delta < 2))
modelDbisbis<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(shoot_h):scale(veg_h_mean)+scale(n_redants))*pop,data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsDbisbis<-dredge(modelDbisbis)
summary(model.avg(modelsDbisbis, subset = delta < 2))
modelDbisbisbis<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants))*pop,data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsDbisbisbis<-dredge(modelDbisbisbis)
summary(model.avg(modelsDbisbisbis, subset = delta < 2))

modelE<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(n_fl)+scale(over_veg)+scale(n_redants))*pop,
               data=data8,dist="poisson",zero.dist="binomial",na.action="na.fail")
modelsE<-dredge(modelE)
summary(model.avg(modelsE, subset = delta < 2))
modelsE[1]
summary(hurdle(n_eggs_max~scale(phen_index_avg)+scale(n_fl)+scale(over_veg)+scale(n_redants)+pop+
                 scale(n_fl):pop+scale(over_veg):pop+scale(phen_index_avg):pop,data=data8,dist="poisson",zero.dist="binomial",na.action="na.fail"))
###############################################
modelEbis<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(n_fl)+scale(over_veg)+scale(n_redants))*pop,
               data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsEbis<-dredge(modelEbis)
summary(model.avg(modelsEbis, subset = delta < 2))
#All interacts with pop except ants

#FAV models
summary(hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg)+scale(n_redants)+pop+
        scale(n_redants):pop,data=data8,dist="poisson",zero.dist="binomial",na.action="na.fail"))
#BUT overdispersed!
#If changing to negbin, ants not *
summary(hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg)+scale(n_redants)+pop+
        scale(n_redants):pop,data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail"))


#Best model from modelsCbis (using negbin from the start of model selection) <------------------
m1<-hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg),data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail")
mnull<-hurdle(n_eggs_max~1,data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail")
m2<-glm.nb(n_eggs_max~scale(phen_index_avg)+scale(over_veg),data=data8,na.action="na.fail")
m3<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(over_veg)+scale(n_redants))*pop,
           data=data8,dist="negbin",zero.dist="binomial",na.action="na.fail")
m4<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(over_veg)+scale(n_redants))*pop,
           data=data8,dist="poisson",zero.dist="binomial",na.action="na.fail")
summary(m1)
summary(mnull)
summary(m2)
AIC(m1,m2) #hurdle is better
vuong(m1,m2) #hurdle is better
pchisq(2 * (logLik(m1) - logLik(mnull)), df = 2, lower.tail = FALSE) #Model is significant

AIC(m3,m4)
vuong(m3,m4)

#. Percent deviance explained, defined as [D(null model) - D(fitted model)] / D(null model)
2*logLik(m1)     # Deviance m1
2*logLik(mnull)  # Deviance mnull
(-1459.397-(-1419.88))/-1459.397 # Only 2.7% !?

# Make as two models
modelattack<-glm(attack~scale(over_veg)+scale(phen_index_avg)+scale(n_redants)+pop+scale(n_redants):pop,data=data8,family="binomial")
summary(modelattack)
modeleggs<-glm(n_eggs_max~scale(over_veg)+scale(phen_index_avg)+scale(n_redants)+pop+scale(n_redants):pop,data=subset(data8,n_eggs_max>0),family="poisson")
summary(modeleggs) # Strongly overdispersed! - Reason for * effect of ants?
modeleggs<-glm.nb(n_eggs_max~scale(over_veg)+scale(phen_index_avg)+scale(n_redants)+pop+scale(n_redants):pop,data=subset(data8,n_eggs_max>0))
summary(modeleggs) # Definitely better but no effect of ants

#For each pop
summary(hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg),data=subset(data8,pop=="H"),
               dist="negbin",zero.dist="binomial",na.action="na.fail"))
summary(hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg),data=subset(data8,pop=="R"),
               dist="negbin",zero.dist="binomial",na.action="na.fail"))
summary(hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg),data=subset(data8,pop=="T"),
               dist="negbin",zero.dist="binomial",na.action="na.fail"))

#GRAPHS
ggplot(subset(data8,n_eggs_max>0), aes(phen_index_avg,n_eggs_max,colour=pop)) +
  geom_smooth(method = "glm.nb", se = T)+
  geom_point(size = 1)
ggplot(subset(data8,n_eggs_max>0), aes(phen_index_avg,n_eggs_max)) +
  geom_smooth(method = "glm.nb", se = T)+facet_grid(.~pop)+
  geom_point(size = 1)
ggplot(subset(data8,n_eggs_max>0), aes(phen_index_avg,n_eggs_max)) +
  geom_smooth(method = "glm.nb", se = T)+
  geom_point(size = 1)

# ggplot(subset(data8,n_eggs_max>0), aes(n_redants,n_eggs_max,colour=pop)) +
#   geom_smooth(method = "glm.nb", se = T)+
#   geom_point(size = 1)
# ggplot(subset(data8,n_eggs_max>0), aes(n_redants,n_eggs_max)) +
#   geom_smooth(method = "glm.nb", se = T)+facet_grid(.~pop)+
#   geom_point(size = 1)

ggplot(data8, aes(over_veg,ifelse(as.integer(data8$attack)==2,1,0),colour=pop)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = T)+
  geom_point(size = 1)
ggplot(data8, aes(over_veg,ifelse(as.integer(data8$attack)==2,1,0))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = T)+facet_grid(.~pop)+
  geom_point(size = 1)
ggplot(data8, aes(over_veg,ifelse(as.integer(data8$attack)==2,1,0))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = T)+
  geom_point(size = 1)

# EITHER keep this model (negative binomial hurdle, only over_veg and phen_index significant)
# OR Try different things (using mean ants instead of max etc.) with the same 3 expl variables in global model <------------------------

# Using mean of n_redants instead of max (as before)
#n_redants_mean is the mean of the daily max (3 sampling days / plant)
n_redants_mean<-read.table("n_redants_mean.txt",header=T,sep=",",dec=".")
head(n_redants_mean)
str(n_redants_mean) 

data9<-merge(data8,n_redants_mean,by="id_pl")
head(data9)
with(data9,hist(n_redants_mean))

modeleggs<-glm.nb(n_eggs_max~scale(over_veg)+scale(phen_index_avg)+scale(n_redants)+pop+scale(n_redants):pop,data=subset(data9,n_eggs_max>0))
summary(modeleggs)
modeleggs<-glm.nb(n_eggs_max~scale(over_veg)+scale(phen_index_avg)+scale(n_redants_mean)+pop+scale(n_redants_mean):pop,data=subset(data9,n_eggs_max>0))
summary(modeleggs)
#A bit better

modelF<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(over_veg)+scale(n_redants_mean))*pop,
                  data=data9,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsF<-dredge(modelF)
summary(model.avg(modelsF, subset = delta < 2))
#Ants not * but at least they are in the averaged model - better?

data9$n_redants_meanC<-ifelse(data9$days_ant_pres==1,0,data9$n_redants_mean)
data9$n_redants_pres<-as.factor(ifelse(data9$n_redants>0,1,0))
data9$n_redants_presC<-as.factor(ifelse(data9$n_redantsC>0,1,0))

modelG<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(over_veg)+redants_presC)*pop,
               data=data9,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsG<-dredge(modelG)
summary(model.avg(modelsG, subset = delta < 2))
modelsG[1]

# It does not get better with redants_meanC, redants_pres or redants_presC


# Using sum of n_redants instead of max (as before)
#n_redants_sum is the sum of all values for each pl
n_redants_sum<-read.table("n_redants_sum.txt",header=T,sep=",",dec=".")
head(n_redants_sum)
str(n_redants_sum) 

data10<-merge(data9,n_redants_sum,by="id_pl")
head(data10)
with(data10,hist(n_redants_sum))

modeleggs<-glm.nb(n_eggs_max~scale(over_veg)+scale(phen_index_avg)+scale(n_redants)+pop+scale(n_redants):pop,data=subset(data10,n_eggs_max>0))
summary(modeleggs)
modeleggs<-glm.nb(n_eggs_max~scale(over_veg)+scale(phen_index_avg)+scale(n_redants_sum)+pop+scale(n_redants_sum):pop,data=subset(data10,n_eggs_max>0))
summary(modeleggs)
#A bit better

modelH<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(over_veg)+scale(n_redants_sum))*pop,
               data=data10,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsH<-dredge(modelH)
summary(model.avg(modelsH, subset = delta < 2))
#Ants not * but at least they are in the averaged model - better?


data10$n_redants_sumC<-ifelse(data10$days_ant_pres==1,0,data10$n_redants_sum)

modelI<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(over_veg)+scale(n_redants_sumC))*pop,
               data=data10,dist="negbin",zero.dist="binomial",na.action="na.fail")
modelsI<-dredge(modelI)
summary(model.avg(modelsI, subset = delta < 2))
modelsG[1]



####################################################################

# Fruits?
with(data10,hist(n_intact_fruits,breaks=15, main = "Number of intact fruits"))
with(subset(data10,n_intact_fruits>0),hist(n_intact_fruits,breaks=15, main = "Number of intact fruits > 0"))

with(data10,mean(n_intact_fruits))
with(data10,var(n_intact_fruits))
with(subset(data10,n_intact_fruits>0),mean(n_intact_fruits))
with(subset(data10,n_intact_fruits>0),var(n_intact_fruits))

modelfr1<-glm(n_intact_fruits~(scale(phen_index_avg)+scale(meanT)+scale(n_eggs_max))*
          pop,family="poisson",data10,na.action="na.fail")
summary(modelfr1)
modelfr2<-glm.nb(n_intact_fruits~(scale(phen_index_avg)+scale(meanT)+scale(n_eggs_max))*
                pop,data10,na.action="na.fail")
summary(modelfr2)
AIC(modelfr1,modelfr2) #Only slightly lower for nb
vuong(modelfr1,modelfr2) #Indistinguishible

modelfr3<-hurdle(n_intact_fruits~(scale(phen_index_avg)+scale(meanT)+scale(n_eggs_max))*
          pop,dist="poisson",zero.dist="binomial",data10,na.action="na.fail")
modelfr4<-hurdle(n_intact_fruits~(scale(phen_index_avg)+scale(meanT)+scale(n_eggs_max))*
          pop,dist="negbin",zero.dist="binomial",data10,na.action="na.fail")
summary(modelfr3)
summary(modelfr4)
AIC(modelfr1,modelfr2,modelfr3,modelfr4) #Still nb
vuong(modelfr1,modelfr3) 
vuong(modelfr2,modelfr4) 

modelsfr1<-dredge(modelfr1)
summary(model.avg(modelsfr1,subset=delta<2))

modelsfr2<-dredge(modelfr2)
summary(model.avg(modelsfr2,subset=delta<2))

modelsfr3<-dredge(modelfr3)
summary(model.avg(modelsfr3,subset=delta<2))

modelsfr4<-dredge(modelfr4)
summary(model.avg(modelsfr4,subset=delta<2))

#Keep poisson model
summary(model.avg(modelsfr1,subset=delta<2))
#Best model to check for overdsip. --> No overdisp.
summary(glm(n_intact_fruits~pop+scale(meanT)+scale(n_eggs_max)+scale(phen_index_avg)+
              +pop:scale(meanT)+pop:scale(phen_index_avg),family="poisson",data10))

#GRAPHS
ggplot(data10, aes(phen_index_avg,n_intact_fruits,colour=pop)) +
  geom_smooth(method = "glm", method.args=list(family="poisson"),se = T)+
  geom_point(size = 1)
ggplot(data10, aes(phen_index_avg,n_intact_fruits)) +
  geom_smooth(method = "glm", method.args=list(family="poisson"), se = T)+facet_grid(.~pop)+
  geom_point(size = 1)
ggplot(data10, aes(phen_index_avg,n_intact_fruits)) +
  geom_smooth(method = "glm", method.args=list(family="poisson"), se = T)+
  geom_point(size = 1)

ggplot(data10, aes(n_eggs_max,n_intact_fruits,colour=pop)) +
  geom_smooth(method = "glm", method.args=list(family="poisson"),se = T)+
  geom_point(size = 1)
ggplot(data10, aes(n_eggs_max,n_intact_fruits)) +
  geom_smooth(method = "glm", method.args=list(family="poisson"), se = T)+facet_grid(.~pop)+
  geom_point(size = 1)
ggplot(data10, aes(n_eggs_max,n_intact_fruits)) +
  geom_smooth(method = "glm", method.args=list(family="poisson"), se = T)+
  geom_point(size = 1)

# Seeds?

head(data2compA)
str(data2compA)
names(data2compA)
data2compA[c(2,55)]

dataseeds<-merge(data10,data2compA[c(2,55)],by="id_pl")
head(dataseeds)
names(dataseeds)
str(dataseeds)
with(dataseeds,table(pop)) #88,80,70

with(dataseeds,hist(seed_n_per_shoot))
with(subset(dataseeds,seed_n_per_shoot>0),hist(seed_n_per_shoot))

modelseeds1<-glm(round(seed_n_per_shoot)~(scale(phen_index_avg)+scale(meanT)+scale(n_eggs_max))*
                pop,family="poisson",dataseeds,na.action="na.fail")
summary(modelseeds1) # Extreme overdispersion
modelseeds2<-glm.nb(round(seed_n_per_shoot)~(scale(phen_index_avg)+scale(meanT)+scale(n_eggs_max))*
                   pop,dataseeds,na.action="na.fail")
summary(modelseeds2) # Extreme overdispersion

# Need zero-inflated models!!! (here zero inflation: it is zero either because no fruits produced
# or because fruit with no seeds)
modelseeds3<-zeroinfl(round(seed_n_per_shoot)~(scale(phen_index_avg)+scale(meanT)+scale(n_eggs_max))*
                   pop,dist="poisson",dataseeds,na.action="na.fail")
summary(modelseeds3)
modelseeds4<-zeroinfl(round(seed_n_per_shoot)~(scale(phen_index_avg)+scale(meanT)+scale(n_eggs_max))*
                        pop,dist="negbin",dataseeds,na.action="na.fail")
summary(modelseeds4)
AIC(modelseeds3,modelseeds4) # Go for negbin

modelsseeds4<-dredge(modelseeds4)
summary(model.avg(modelsseeds4,subset=delta<2))
modelsseeds4[1]
summary(zeroinfl(round(seed_n_per_shoot)~scale(phen_index_avg)+scale(n_eggs_max)+
                   pop+pop:scale(phen_index_avg),dist="negbin",dataseeds,na.action="na.fail"))

# Strange results where effect of eggs on seeds is positive
# Probably not right to look at seeds without looking at fruits --> NOT USE

# Fruit set
data10$fruit_set<-data10$n_intact_fruits/data10$n_fl
with(data10,boxplot(fruit_set))
with(data10,min(fruit_set))
with(data10,mean(fruit_set))
with(data10,max(fruit_set))
with(data10,hist(fruit_set))
with(data10,plot(fruit_set))
with(data10,plot(pop,fruit_set))

model_fs<-glm(cbind(n_intact_fruits,n_fl)~(scale(phen_index_avg)+scale(n_eggs_max)+scale(meanT)+scale(over_veg))*pop,
              family="binomial",data10,na.action="na.fail")
summary(model_fs)
models_fs<-dredge(model_fs)
summary(model.avg(models_fs,subset=delta<2)) # Phen, eggs and pop *, no interactions

#GRAPHS
ggplot(data10, aes(n_eggs_max, fruit_set,colour=pop)) + geom_point(size = 1) + 
  geom_smooth(method = "glm", method.args=list(family = "binomial"), aes(weight=n_fl)) 
ggplot(data10, aes(n_eggs_max, fruit_set)) + geom_point(size = 1) + facet_grid(.~pop)+
  geom_smooth(method = "glm", method.args=list(family = "binomial"), aes(weight=n_fl)) 
ggplot(data10, aes(n_eggs_max, fruit_set)) + geom_point(size = 1) + 
  geom_smooth(method = "glm", method.args=list(family = "binomial"), aes(weight=n_fl)) 

ggplot(data10, aes(phen_index_avg, fruit_set,colour=pop)) + geom_point(size = 1) + 
  geom_smooth(method = "glm", method.args=list(family = "binomial"), aes(weight=n_fl)) 
ggplot(data10, aes(phen_index_avg, fruit_set)) + geom_point(size = 1) + facet_grid(.~pop)+
  geom_smooth(method = "glm", method.args=list(family = "binomial"), aes(weight=n_fl)) 
ggplot(data10, aes(phen_index_avg, fruit_set)) + geom_point(size = 1) + 
  geom_smooth(method = "glm", method.args=list(family = "binomial"), aes(weight=n_fl)) 

