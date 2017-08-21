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

model_eggs<-hurdle(n_eggs_max~(scale(phen_index1)+scale(shoot_h)+
                   scale(veg_h_mean)+scale(n_redants))*pop,
                   data=data3,dist="negbin",zero.dist="binomial",na.action="na.fail")
model_eggs_wT<-hurdle(n_eggs_max~(scale(phen_index1)+scale(shoot_h)+
                   scale(veg_h_mean)+scale(n_redants)+scale(meanT))*pop,
                   data=data3,dist="negbin",zero.dist="binomial",na.action="na.fail")
models_eggs<-dredge(model_eggs)
models_eggs_wT<-dredge(model_eggs_wT)

summary(model.avg(models_eggs, subset = delta < 2)) #Table1
summary(model.avg(models_eggs_wT, subset = delta < 2)) 

summary(hurdle(n_eggs_max~meanT*pop,
        data=data3,dist="negbin",zero.dist="binomial",na.action="na.fail"))
#xtable not working with hurdle

#Poisson model had an effect on ants on the count part, but was strongly overdispersed :(
#And negbin pointed as better by AIC comparisons and vuong test

#Two models
model_eggs1<-glm(attack~(scale(phen_index1)+scale(shoot_h)+
                  scale(veg_h_mean)+scale(n_redants)+scale(meanT))*pop,
                 data=data3,family="binomial",na.action="na.fail")
summary(model_eggs1)
models_eggs1<-dredge(model_eggs1)
summary(model.avg(models_eggs1, subset = delta < 2))

model_eggs2<-glm.nb(n_eggs_max~(scale(phen_index1)+scale(shoot_h)+
                 scale(veg_h_mean)+scale(n_redants))*pop,
                 data=subset(data3,n_eggs_max>0),na.action="na.fail")
summary(model_eggs2)
models_eggs2<-dredge(model_eggs2)
summary(model.avg(models_eggs2, subset = delta < 2))

nullmodel_eggs<-hurdle(n_eggs_max~1,data=data3,dist="negbin",zero.dist="binomial",na.action="na.fail")
#. Percent deviance explained, defined as [D(null model) - D(fitted model)] / D(null model)
2*logLik(model_eggs)     # Deviance model_eggs
2*logLik(nullmodel_eggs)  # Deviance null model
(-1459.397-(-1334.749))/-1459.397 # 8.5% !?

# model_eggs_H<-hurdle(n_eggs_max~scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants),data=subset(data3,pop=="H"),
#                dist="negbin",zero.dist="binomial",na.action="na.fail")
# model_eggs_R<-hurdle(n_eggs_max~scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants),data=subset(data3,pop=="R"),
#                dist="negbin",zero.dist="binomial",na.action="na.fail")
# model_eggs_T<-hurdle(n_eggs_max~scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(n_redants),data=subset(data3,pop=="T"),
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

#Not including models for eggs for each population  BY NOW
#because interactions with population are not significant

data3$attack<-with(data3,ifelse(n_eggs_max>0,1,0))

ggplot(data3, aes(meanT,as.integer(data3$attack)))+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), aes(linetype = population),se = T,size=0.5,color="black")+
  geom_point(size = 1)+theme_base()+ylab("Probability of having eggs")+
  xlab("Temp")+theme(plot.background=element_rect(fill="white", colour=NA))

p1<-ggplot(data3, aes(phen_index1,as.integer(data3$attack)))+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = T,size=0.5,color="blue",fill = "lightblue")+
  geom_point(size = 1,color="blue")+theme_base()+ylab("Probability of having eggs")+
  xlab("Plant phenology")+
  theme(plot.background=element_rect(fill="white", colour=NA))
p2<-ggplot(data3, aes(shoot_h,as.integer(data3$attack)))+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = T,size=0.5,color="blue",fill = "lightblue")+
  geom_point(size = 1,color="blue")+theme_base()+ylab(NULL)+theme(axis.text.y = element_blank())+
  xlab("Shoot height (cm)")+
  theme(plot.background=element_rect(fill="white", colour=NA))
p3<-ggplot(data3, aes(veg_h_mean,as.integer(data3$attack)))+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = T,size=0.5,color="blue",fill = "lightblue")+
  geom_point(size = 1,color="blue")+theme_base()+ylab(NULL)+theme(axis.text.y = element_blank())+
  xlab("Vegetation height (cm)")+
  theme(plot.background=element_rect(fill="white", colour=NA))
p1
p2
p3
pdf("./results/figures/fig1A_eggs.pdf", width=10,height=3.5)
grid.arrange(p1, p2, p3, widths=c(0.60,0.45,0.45), ncol=3)
dev.off()
pdf("./results/figures/fig1A_eggs_color.pdf", width=10,height=3.5)
grid.arrange(p1, p2, p3, widths=c(0.60,0.45,0.45), ncol=3)
dev.off()

ggplot(subset(data3,n_eggs_max>0), aes(meanT,n_eggs_max))+
  geom_smooth(method = "glm.nb", se = T,aes(linetype = population),size=0.5,color="black")+
  geom_point(size = 1)+theme_base()+ylab("Number of eggs")+
  xlab("Temp")+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(strip.text.x = element_text(colour="white"),strip.background = element_rect(fill="white"))

ggplot(data3, aes(meanT,n_redants))+
  geom_smooth(method = "glm.nb", se = T,aes(linetype = population),size=0.5,color="black")+
  geom_point(size = 1)+theme_base()+ylab("Number of ants")+
  xlab("Temp")+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(strip.text.x = element_text(colour="white"),strip.background = element_rect(fill="white"))


p4<-ggplot(subset(data3,n_eggs_max>0), aes(phen_index1,n_eggs_max))+
  geom_smooth(method = "glm.nb", se = T,size=0.5,color="blue",fill = "lightblue")+
  geom_point(size = 1,color="blue")+theme_base()+ylab("Number of eggs")+
  xlab("Plant phenology")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(strip.text.x = element_text(colour="white"),strip.background = element_rect(fill="white"))
p5<-ggplot(subset(data3,n_eggs_max>0), aes(shoot_h,n_eggs_max))+
  geom_smooth(method = "glm.nb", se = T,size=0.5,color="blue",fill = "lightblue")+
  geom_point(size = 1,color="blue")+theme_base()+ylab(NULL)+theme(axis.text.y = element_blank())+
  xlab("Shoot height (cm)")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(strip.text.x = element_text(colour="white"),strip.background = element_rect(fill="white"))
p6<-ggplot(subset(data3,n_eggs_max>0), aes(shoot_h,n_eggs_max))+
  geom_point(color="white")+theme_base()+ylab(NULL)+theme(axis.text.y = element_blank())+
  xlab(NULL)+theme(axis.text.x = element_blank())+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(strip.text.x = element_text(colour="white"),strip.background = element_rect(fill="white"))+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank())+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
p4
p5
p6
pdf("./results/figures/fig1B_eggs.pdf", width=10,height=3.5)
grid.arrange(p4,p5,p6, widths=c(0.57,0.45,0.45), ncol=3)
dev.off()
pdf("./results/figures/fig1B_eggs_color.pdf", width=10,height=3.5)
grid.arrange(p4,p5,p6, widths=c(0.57,0.45,0.45), ncol=3)
dev.off()
#Make combined figure A-B in Photoshop

#### Model 2: Phen ####
model_phen<-lm(log(phen_index1)~scale(meanT)*pop,data=data3,na.action="na.fail")
summary(model_phen)
plot(model_phen)

models_phen<-dredge(model_phen)
summary(model.avg(models_phen, subset = delta < 2))
models_phen[1]
model_phen_best<-lm(log(phen_index1)~scale(meanT)+pop,data=data3,na.action="na.fail")
summary(model_phen_best) #R2=0.40
print.xtable(xtable(xtable(summary(model_phen_best)),
      digits=c(0,3,3,2,3)), type="html",file="./results/tables/model_phen.html")

#### Model 3: Height ####
model_height<-lm(shoot_h~scale(meanT)*pop,data=data3,na.action="na.fail")

summary(model_height)
plot(model_height)

models_height<-dredge(model_height)
summary(model.avg(models_height, subset = delta < 2))
models_height[1]
model_height_best<-lm(shoot_h~scale(meanT)*pop,data=data3,na.action="na.fail")
summary(model_height_best)#R2=0.20
print.xtable(xtable(xtable(summary(model.avg(models_height, subset = delta < 2))),
      digits=c(0,3,3,3,2,3)), type="html",file="./results/tables/model_height.html")

model_height_H<-lm(shoot_h~scale(meanT),data=subset(data3,pop=="H"),na.action="na.fail")
model_height_R<-lm(shoot_h~scale(meanT),data=subset(data3,pop=="R"),na.action="na.fail")
model_height_T<-lm(shoot_h~scale(meanT),data=subset(data3,pop=="T"),na.action="na.fail")

summary(model_height_H)
summary(model_height_R)
summary(model_height_T)

print.xtable(xtable(model_height_H,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_height_H.html")
print.xtable(xtable(model_height_R,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_height_R.html")
print.xtable(xtable(model_height_T,digits=c(0,3,3,2,3)),type="html",file="./results/tables/model_height_T.html")

#### Model 4: Ants ####
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

# pdf("./results/figures/fig3_ants.pdf", family="Times",width=4,height=3.5)
# ggplot(int_ants, aes(veg_h_mean,fit, group = as.factor(meanT))) +
#   geom_line(size=0.3)+theme_base()+xlab("Vegetation height (cm)")+
#   ylab(expression(paste("Number of ", italic("Myrmica")," ants")))+
#   coord_cartesian(xlim=c(0,72),ylim=c(0,40))+theme(legend.position="none")+
#   annotate("text", x = 66.5, y = 28.3, label = "20 ºC")+
#   annotate("text", x = 66.5, y = 19.3, label = "19 ºC")+
#   annotate("text", x = 66.5, y = 13, label = "18 ºC")+
#   annotate("text", x = 66.5, y = 9, label = "17 ºC")+
#   annotate("text", x = 66.5, y = 6.3, label = "16 ºC")+
#   annotate("text", x = 66.5, y = 4.4, label = "15 ºC")+
#   annotate("text", x = 66.5, y = 2.6, label = "14 ºC")+
#   theme(plot.background=element_rect(fill="white", colour=NA))
# dev.off()

p7<-ggplot(data3, aes(meanT,phen_index1))+
  geom_smooth(method = "lm",  se = T,fullrange=T,size=0.4,color="blue",fill="lightblue")+ylab(NULL)+
  geom_point(size = 0.5,color="blue")+ theme_base()+ xlab(NULL)+ylab("Plant phenology")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_x_continuous(limits = c(14,19))
p8<-ggplot(data3, aes(meanT,shoot_h))+
  geom_smooth(method = "lm",  se = T,fullrange=T,size=0.4,
  aes(color = population,fill=population))+ylab(NULL)+
  geom_point(size = 0.5,aes(color=population))+ theme_base()+ xlab("Soil temperature")+ylab("Shoot height (cm)")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_color_manual(values = c("#E58700", "#00BA38", "#E76BF3"))+
  scale_fill_manual(values = c("#E58700", "#00BA38", "#E76BF3"))+
  theme(legend.position="none")+scale_x_continuous(limits = c(14,19))

p9<-ggplot(int_ants, aes(veg_h_mean,fit, group = as.factor(meanT)))+
  geom_line(size=0.6,aes(veg_h_mean,fit,color=as.factor(meanT)))+theme_base()+xlab("Vegetation height (cm)")+
  ylab(expression(paste("Number of ", italic("Myrmica")," ants")))+
  coord_cartesian(xlim=c(0,72),ylim=c(0,40))+theme(legend.position="none")+
  annotate("text", x = 66.5, y = 28.3, label = "20 ºC")+
  annotate("text", x = 66.5, y = 19.3, label = "19 ºC")+
  annotate("text", x = 66.5, y = 13, label = "18 ºC")+
  annotate("text", x = 66.5, y = 9, label = "17 ºC")+
  annotate("text", x = 66.5, y = 6.3, label = "16 ºC")+
  annotate("text", x = 66.5, y = 4.4, label = "15 ºC")+
  annotate("text", x = 66.5, y = 2.6, label = "14 ºC")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_color_manual(values=c("#FFFF66","#FFFF00","#FFCC33","#FF9900","#FF6600","#FF0000","#CC3333"))
p9
pdf("./results/figures/fig2_phen_height.pdf", width=3,height=5)
grid.arrange(p7,p8, ncol=1)
dev.off()
pdf("./results/figures/fig2_phen_height_color.pdf", width=3,height=5)
grid.arrange(p7,p8, ncol=1)
dev.off()

pdf("./results/figures/figS2_ants.pdf", width=4,height=4)
p9
dev.off()
pdf("./results/figures/figS2_ants_color.pdf", width=4,height=4)
p9
dev.off()

#### Model 5: Fruit set ####

#Fruits
data3$fruit_set<-data3$n_intact_fruits/data3$n_fl
# model_fruitset<-glm(cbind(n_intact_fruits,n_fl)~(scale(phen_index1)+scale(shoot_h)+
#       scale(veg_h_mean)+scale(meanT)+scale(n_eggs_max))*pop,family="binomial",data=data3,na.action="na.fail")
# summary(model_fruitset)
# models_fruitset<-dredge(model_fruitset)
# summary(model.avg(models_fruitset,subset=delta<2)) 
# 
# model_fruitset_H<-glm(cbind(n_intact_fruits,n_fl)~scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(meanT)+scale(n_eggs_max),
#                       family="binomial",data=subset(data3,pop=="H"),na.action="na.fail")
# model_fruitset_R<-glm(cbind(n_intact_fruits,n_fl)~scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(meanT)+scale(n_eggs_max),
#                       family="binomial",data=subset(data3,pop=="R"),na.action="na.fail")
# model_fruitset_T<-glm(cbind(n_intact_fruits,n_fl)~scale(phen_index1)+scale(shoot_h)+scale(veg_h_mean)+scale(meanT)+scale(n_eggs_max),
#                       family="binomial",data=subset(data3,pop=="T"),na.action="na.fail")
# summary(model_fruitset_H)
# summary(model_fruitset_R)
# summary(model_fruitset_T)

##Without Remmene (only 14 plants where fruitset>0)
model_fruitset<-glm(cbind(n_intact_fruits,n_fl)~(scale(phen_index1)+scale(shoot_h)+
    scale(veg_h_mean)+scale(meanT)+scale(n_eggs_max))*pop,family="binomial",
    data=subset(data3,!pop=="R"),na.action="na.fail")
model_fruitset_noT<-glm(cbind(n_intact_fruits,n_fl)~(scale(phen_index1)+scale(shoot_h)+
    scale(veg_h_mean)+scale(n_eggs_max))*pop,family="binomial",
    data=subset(data3,!pop=="R"),na.action="na.fail")
model_fruitset_noT_noP<-glm(cbind(n_intact_fruits,n_fl)~(scale(phen_index1)+scale(shoot_h)+
    scale(veg_h_mean))*pop,family="binomial",
    data=subset(data3,!pop=="R"),na.action="na.fail")
summary(model_fruitset)
models_fruitset<-dredge(model_fruitset)
models_fruitset_noT<-dredge(model_fruitset_noT)
models_fruitset_noT_noP<-dredge(model_fruitset_noT_noP)
summary(model.avg(models_fruitset,subset=delta<2)) 
summary(model.avg(models_fruitset_noT,subset=delta<2)) 
summary(model.avg(models_fruitset_noT_noP,subset=delta<2)) 

summary(glm(cbind(n_intact_fruits,n_fl)~(scale(meanT))*pop,family="binomial",
    data=subset(data3,!pop=="R"),na.action="na.fail"))

print.xtable(xtable(xtable(summary(model.avg(models_fruitset, subset = delta < 2))),
                    digits=c(0,3,3,3,2,3)), type="html",file="./results/tables/model_fruitset.html")
models_fruitset[1]
model_fruitset_best<-glm(cbind(n_intact_fruits,n_fl)~scale(phen_index1)+scale(meanT)+
             scale(shoot_h)+scale(n_eggs_max)+pop+pop:scale(meanT)+pop:scale(n_eggs_max),
                         family="binomial",data=data3)
summary(model_fruitset_best)
r.squaredLR(model_fruitset_best)
NagelkerkeR2(model_fruitset_best)

pdf("./results/figures/fig3_fruitset.pdf", width=3,height=3)
pdf("./results/figures/fig3_fruitset_color.pdf", width=3,height=3)
ggplot(data3, aes(n_eggs_max, fruit_set)) + geom_point(size = 0.5,color="blue")+
  geom_smooth(method = "glm", method.args=list(family = "binomial"), aes(weight=n_fl),size=0.5,,color="blue",fill="lightblue")+
  theme_base()+xlab("Number of eggs")+ylab("Fruit set")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(strip.text.x = element_text(colour="white"),strip.background = element_rect(fill="white"))
dev.off()


#### Model 6: Temp ####
model_temp<-lm(meanT~scale(veg_h_mean)*pop,data=data3,na.action="na.fail")
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

pdf("./results/figures/fig_appendix_temp.pdf", width=7,height=3)
ggplot(data3, aes(veg_h_mean,meanT)) + facet_grid(.~population)+
  geom_smooth(method = "lm",  se = T,fullrange=T,size=0.5,color="black")+ylab("Soil temperature")+
  geom_point(size = 0.5)+ theme_base()+ xlab("Vegetation height (cm)")+theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()

pdf("./results/figures/fig_appendix_temp_together.pdf", width=3,height=3)
pdf("./results/figures/fig_appendix_temp_together_color.pdf", width=3,height=3)
ggplot(data3, aes(veg_h_mean,meanT)) +
  geom_smooth(method = "lm",  se = T,fullrange=T,size=0.5,color="blue",fill="lightblue")+ylab("Soil temperature")+
  geom_point(size = 0.5,color="blue")+ theme_base()+ xlab("Vegetation height (cm)")+theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()




