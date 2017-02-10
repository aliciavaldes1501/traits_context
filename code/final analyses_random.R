# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # #  Final analyses to be included in paper # # # # # # # # #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

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
library(car)
library(grid)
library(cowplot)

#5 different models
#Response variables: meanT, phen_index1, n_redants, n_eggs_max,cbind(n_intact_fruits,n_fl)

#### Model 1: Eggs ####

#Two models
model_eggs1<-glmer(attack~scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants)+
                     (1+scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants)|pop),
                      data=data3,family="binomial",na.action="na.fail")
summary(model_eggs1)
models_eggs1<-dredge(model_eggs1)
summary(model.avg(models_eggs1, subset = delta < 2))

model_eggs2<-glmer.nb(n_eggs_max~scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants)+
                      (1+scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants)|pop),
                    data=subset(data3,n_eggs_max>0),na.action="na.fail")
summary(model_eggs2)
models_eggs2<-dredge(model_eggs2)
summary(model.avg(models_eggs2, subset = delta < 2))

#### Model 2: Phen ####
model_phen<-lmer(log(phen_index1)~scale(meanT)+(scale(meanT)|pop),data=data3,na.action="na.fail")
summary(model_phen)
plot(model_phen)

models_phen<-dredge(model_phen)
summary(model.avg(models_phen, subset = delta < 2))
models_phen[1]

#### Model 3: Height ####
model_height<-lmer(shoot_h~scale(meanT)+(scale(shoot_h)|pop),data=data3,na.action="na.fail")

summary(model_height)
plot(model_height)

models_height<-dredge(model_height)
summary(model.avg(models_height, subset = delta < 2))
models_height[1]

#### Model 4: Ants ####
#Not using n_redants_mean because it is not an integer (warning with glm.nb)
model_ants<-glmer.nb(n_redants~scale(veg_h_mean)*scale(meanT)+(scale(veg_h_mean)*scale(meanT)|pop),na.action="na.fail",data=data3)
models_ants<-dredge(model_ants)
summary(model.avg(models_ants, subset = delta < 2))


#### Model 5: Fruit set ####

##Without Remmene (only 14 plants where fruitset>0)
model_fruitset<-glmer(cbind(n_intact_fruits,n_fl)~scale(phen_index1)+scale(shoot_h)+
                 scale(veg_h_mean)+scale(meanT)+scale(n_eggs_max)+
                (1+scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(meanT)+scale(n_eggs_max)|pop),
                family="binomial",data=subset(data3,!pop=="R"),na.action="na.fail")
summary(model_fruitset)
models_fruitset<-dredge(model_fruitset)
summary(model.avg(models_fruitset,subset=delta<2)) 

#### Model 6: Temp ####
model_temp<-lmer(meanT~scale(veg_h_mean)+(scale(veg_h_mean)|pop),data=data3,na.action="na.fail")
summary(model_temp) 
models_temp<-dredge(model_temp)
summary(model.avg(models_temp, subset = delta < 2)) 
print.xtable(xtable(xtable(summary(model.avg(models_temp, subset = delta < 2))),
                    digits=c(0,3,3,3,2,3)), type="html",file="./results/tables/model_temp.html")
models_temp[1]
summary(lm(meanT~veg_h_mean*pop,data=data3,na.action="na.fail"))

model_temp_H<-lm(meanT~scale(veg_h_mean),data=subset(data3,pop=="H")) #*
model_temp_R<-lm(meanT~scale(veg_h_mean),data=subset(data3,pop=="R")) #*
model_temp_T<-lm(meanT~scale(veg_h_mean),data=subset(data3,pop=="T")) #* 
summary(model_temp_H)
summary(model_temp_R)
summary(model_temp_T)

print.xtable(xtable(model_temp_H,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_temp_H.html")
print.xtable(xtable(model_temp_R,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_temp_R.html")
print.xtable(xtable(model_temp_T,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_temp_T.html")
#Models for each pop not needed cause no * interaction in averaged model!

pdf("./results/figures/fig_appendix_temp.pdf", family="Times",width=7,height=3)
ggplot(data3, aes(veg_h_mean,meanT)) + facet_grid(.~population)+
  geom_smooth(method = "lm",  se = T,fullrange=T,size=0.5,color="black")+ylab("Soil temperature")+
  geom_point(size = 0.5)+ theme_base()+ xlab("Vegetation height (cm)")+theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()

pdf("./results/figures/fig_appendix_temp_together.pdf", family="Times",width=3,height=3)
ggplot(data3, aes(veg_h_mean,meanT)) +
  geom_smooth(method = "lm",  se = T,fullrange=T,size=0.5,color="black")+ylab("Soil temperature")+
  geom_point(size = 0.5)+ theme_base()+ xlab("Vegetation height (cm)")+theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()




