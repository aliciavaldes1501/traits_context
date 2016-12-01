#####################################################################################
####################### Final analyses repeated adding plants #######################
########################### from "Uncut" plots in Högsjön ###########################
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

#Adding plants from "Uncut" plots to the datafile
head(data1)
data_vegplots_2015<-read.table("./data/clean/datafile_vegplots_2015.txt",header=T,sep="\t",dec=",")
names(data_vegplots_2015)
#leaving only needed variables
data_vegplots_2015<-subset(data_vegplots_2015,treat=="Uncut")
data_vegplots_2015<-data_vegplots_2015[c(3,5,8,18:19,24,29:31)]
data_vegplots_2015$population<-as.factor("Högsjön")

head(data_vegplots_2015)
str(data_vegplots_2015)
str(data1)

data4<-rbind(data1,data_vegplots_2015)
str(data4)
head(data4)
tail(data4)

#Update phenology with phen_revised_cut_vegplots_2015.txt
phen_revised<-read.table("./data/clean/data_phen_revised.txt",header=T,sep="\t",dec=".")
phen_revised_cut_vegplots_2015<-read.table("./data/clean/data_phen_revised_cut_vegplots_2015.txt",header=T,sep="\t",dec=".")

data4$phen_index<-NULL
data4$most_adv<-NULL
data4$n_fl_corrected<-NULL

data4A<-merge(data4,phen_revised,by="id_pl") 
head(data4A)
str(data4A)
with(data4A,table(population))#102,97,103
data4A$julian1<-NULL
data4A$julian2<-NULL

data4B<-merge(data4,phen_revised_cut_vegplots_2015,by="id_pl") 
head(data4B)
str(data4B)
with(data4B,table(population))#50,0,0

data5<-rbind(data4A,data4B)
head(data5)
str(data5)
with(data5,table(population))#152,97,103

#Remove two outliers from Remmene where veg_h was >90 cm
data5<-subset(data5,veg_h_mean<90)
str(data5) #350 obs
with(data5,table(population))#152,95,103

#There were 26 plants where phenology did not change from t1 to t2
#Assign them a state at t2 based on what we would expect if development had continued
#Use the relationship: phen_index2 = phen_index1 + n_fl + phen_index1 * n_fl
#for all plants that developed to assign those 23 a value,
#and based on this, calculate measure 1 (phen_avg) or 2 (julian3).

data5$phen_index_diff<-data5$phen_index2-data5$phen_index1
model1rep<-lm(phen_index2~phen_index1*n_fl,data=subset(data5,phen_index_diff>0))
summary(model1rep) #R2=0.44
nobs(model1rep) #323 plants
plot(model1rep) #meh

with(subset(data5,phen_index_diff>0),plot(phen_index2~phen_index1))
with(subset(data5,phen_index_diff>0),plot(phen_index2~n_fl))

newdatarep<-subset(data5,phen_index_diff==0)[c(1,15,26)]
newdatarep$phen_index2_pred<-predict(model1rep,newdatarep)
newdatarep

data5<-merge(data5,newdatarep[c(1,4)],by="id_pl",all.x=T)
data5$phen_index2c<-ifelse(data5$phen_index_diff>0,data5$phen_index2,data5$phen_index2_pred)
subset(data5,is.na(phen_index2c))
data5 <- data5[-which(data5$id_pl == "H10_15_008"), ]
with(data5,hist(phen_index2))
with(data5,hist(phen_index2c))

#New measure of phenology: average two dates
data5$phen_index_avg<-(data5$phen_index1+data5$phen_index2c)/2
with(data5,hist(phen_index_avg))

#Redo some of the final analyses #################################################

#Eggs ~ phen + over_veg + ants ###################################################

data5$over_veg<-data5$shoot_h-data5$veg_h_mean
#Positive values = shoot over the vegetation
#Negative values = shoot under the vegetation

model_eggs_rep<-hurdle(n_eggs_max~(scale(phen_index_avg)+scale(over_veg)+scale(n_redants))*population,data=data5,dist="negbin",zero.dist="binomial",na.action="na.fail")
models_eggs_rep<-dredge(model_eggs_rep)
summary(model.avg(models_eggs_rep, subset = delta < 2))
#Only one model
models_eggs_rep[1]
model_eggs_rep_best<-hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg)+scale(n_redants),data=data5,dist="negbin",zero.dist="binomial",na.action="na.fail")
summary(model_eggs_rep_best)
#xtable not working with hurdle
#Same result more or less (ants not significant)

#Poisson model had an effect on ants on the count part, but was strongly overdispersed :(
#And negbin pointed as better by AIC comparisons and vuong test

nullmodel_eggs_rep<-hurdle(n_eggs_max~1,data=data5,dist="negbin",zero.dist="binomial",na.action="na.fail")
#. Percent deviance explained, defined as [D(null model) - D(fitted model)] / D(null model)
2*logLik(model_eggs_rep)     # Deviance model_eggs
2*logLik(nullmodel_eggs_rep)  # Deviance null model
(-1712.712-(-1648.488))/-1712.712 # Only 3.7% !? 

#Refit model for pop H
model_eggs_H_rep<-hurdle(n_eggs_max~scale(phen_index_avg)+scale(over_veg)+scale(n_redants),data=subset(data5,population=="Högsjön"),
                     dist="negbin",zero.dist="binomial",na.action="na.fail")
summary(model_eggs_H_rep)
models_eggs_H_rep<-dredge(model_eggs_H_rep)
summary(model.avg(models_eggs_H_rep, subset = delta < 2))
models_eggs_H_rep[1]
model_eggs_H_rep_best<-hurdle(n_eggs_max~scale(over_veg)+scale(n_redants),data=data5,dist="negbin",zero.dist="binomial",na.action="na.fail")
summary(model_eggs_H_rep_best)

#Keep analyses without data from "Uncut" plots
