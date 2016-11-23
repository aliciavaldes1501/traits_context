#####################################################################################
######################## Code to modifiy datafile to include ########################
################# revised iButton data and new phenology measures ###################
#####################################################################################

library(ggplot2)
library(MASS)
library(MuMIn)
library(visreg)
library(effects)
library(pscl)
library(lme4)
library(ggthemr)

#Set the colour scheme and layout for ggplot (stays for whole session)
ggthemr(palette="greyscale", layout = "clean")

#Rebuild data file with new temperature data (only based on day temperatures)
#and leaving only needed variables
names(data1)
data1<-data1[c(1:2,4,7,17:18,23,28:30)]
head(data1)
str(data1)

data3<-merge(data1,loggers_day_agg,by="id_pl")

#Rebuild data file with new phenology data 

with(data3,table(pop))#104,96,103
str(data3)#303 obs
names(data3)
data3$phen_index<-NULL
data3$most_adv<-NULL
data3$n_fl_corrected<-NULL
names(data3)

phen_revised<-read.table("./data/clean/data_phen_revised.txt",header=T,sep="\t",dec=".")

head(phen_revised)
str(phen_revised) #302 obs

data3<-merge(data3,phen_revised,by="id_pl") 
head(data3)
str(data3) #297 obs
with(data3,table(pop))#99,95,103

#Remove two outliers from Remmene where veg_h was >90 cm
data3<-subset(data3,veg_h_mean<90)
str(data3) #295 obs
with(data3,table(pop))#99,93,103

write.table(data3,file="./data/clean/datafile3.txt",sep="\t",dec=".",col.names=T) 

#Histograms of phenology and flower number
with(data3,hist(phen_index1))
with(data3,hist(phen_index2))
with(data3,hist(most_adv1))
with(data3,hist(most_adv2))
with(data3,hist(n_fl))

#There were 23 plants where phenology did not change from t1 to t2
#Assign them a state at t2 based on what we would expect if development had continued
#Use the relationship: phen_index2 = phen_index1 + n_fl + phen_index1 * n_fl
#for all plants that developed to assign those 23 a value,
#and based on this, calculate measure 1 (phen_avg) or 2 (julian3).

data3$phen_index_diff<-data3$phen_index2-data3$phen_index1
model1<-lm(phen_index2~phen_index1*n_fl,data=subset(data3,phen_index_diff>0))
summary(model1) #R2=0.44
nobs(model1) #295-23=272 plants
plot(model1) #meh

with(subset(data3,phen_index_diff>0),plot(phen_index2~phen_index1))
with(subset(data3,phen_index_diff>0),plot(phen_index2~n_fl))

newdata<-subset(data3,phen_index_diff==0)[c(1,21,32)]
newdata$phen_index2_pred<-predict(model1,newdata)
newdata

data3<-merge(data3,newdata[c(1,4)],by="id_pl",all.x=T)
data3$phen_index2c<-ifelse(data3$phen_index_diff>0,data3$phen_index2,data3$phen_index2_pred)
with(data3,hist(phen_index2))
with(data3,hist(phen_index2c))

#New measure of phenology: average two dates
data3$phen_index_avg<-(data3$phen_index1+data3$phen_index2c)/2
with(data3,hist(phen_index_avg))

#New measure of phenology: extrapolated
#Calculated in JMP
phenology<-read.table("./data/clean/phenology.txt",header=T,sep=",",dec=".",na.strings=".")
head(phenology)
str(phenology)

data3<-merge(data3,phenology[c(1,9)],by="id_pl")
head(data3)
str(data3)

#Histograms of the different measures

#Phenology at time 1, 2, and average
p1<-ggplot(data3,aes(x=phen_index1))+
  geom_histogram(aes(y=..density..),binwidth=.5,colour="black", fill="white")+ 
  geom_density(alpha=.2, fill="#FF6666")+ggtitle("Mean on 29-30 July")+theme(plot.title=element_text(size=10))
p2<-ggplot(data3,aes(x=phen_index2c))+
  geom_histogram(aes(y=..density..),binwidth=.5,colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ggtitle("Mean on 27-29 August, corr")+theme(plot.title=element_text(size=10))
p3<-ggplot(data3,aes(x=phen_index_avg))+
  geom_histogram(aes(y=..density..),binwidth=.5,colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ggtitle("Mean, average 2 dates")+theme(plot.title=element_text(size=10))
p4<-ggplot(data3,aes(x=julian_w3))+
  geom_histogram(aes(y=..density..),binwidth=10,colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ggtitle("Julian day when phen=3")+theme(plot.title=element_text(size=10))
pdf("./results/figures/comparison_distr_phen_measures.pdf", family="Times")
multiplot(p1, p2, p3, p4, cols=2)
dev.off()