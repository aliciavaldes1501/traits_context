library(ggplot2)
library(MASS)
library(MuMIn)
library(visreg)
library(effects)
library(pscl)
library(lme4)

with(data3,table(pop))#104,94,103
str(data3)#301 obs
names(data3)
data3$phen_index<-NULL
data3$most_adv<-NULL
data3$n_fl_corrected<-NULL
data3$t_phen_index<-NULL
data3$t_most_adv<-NULL
data3$t_n_fl_corrected<-NULL
data3$predicted<-NULL
names(data3)

phen_revised<-read.table("./data/clean/data_phen_revised.txt",header=T,sep="\t",dec=".")

head(phen_revised)
str(phen_revised) #302 obs
#with(phen_revised,table(pop))#102,97,103

data5<-merge(data3,phen_revised,by="id_pl") #MODIFY FROM HERE NAMES / LOCATIONS OF DATAFILES
head(data5)
str(data5) #295 obs
with(data5,table(pop))#99,93,103

write.table(data5,file="data5.txt",sep="\t",dec=".",col.names=T)

with(data5,hist(phen_index1))
with(data5,hist(phen_index2))
with(data5,hist(most_adv1))
with(data5,hist(most_adv2))
with(data5,hist(n_fl))

#There were 23 plants where phenology did not change from t1 to t2
#Assign them a state at t2 based on what we would expect if development had continued
#Use the relationship: phen_index2 = phen_index1 + n_fl + phen_index1 * n_fl
#for all plants that developed to assign those 23 a value,
#and based on this, calculate measure 1 (phen_avg), 2 (julian3) or 3 (julian2).

data5$phen_index_diff<-data5$phen_index2-data5$phen_index1
model1<-lm(phen_index2~phen_index1*n_fl,data=subset(data5,phen_index_diff>0))
summary(model1) #R2=0.44
nobs(model1) #295-23=272 plants
plot(model1) #meh

with(subset(data5,phen_index_diff>0),plot(phen_index2~phen_index1))
with(subset(data5,phen_index_diff>0),plot(phen_index2~n_fl))

newdata<-subset(data5,phen_index_diff==0)[c(1,21,32)]
newdata$phen_index2_pred<-predict(model1,newdata)
newdata

data5<-merge(data5,newdata[c(1,4)],by="id_pl",all.x=T)
data5$phen_index2c<-ifelse(data5$phen_index_diff>0,data5$phen_index2,data5$phen_index2_pred)
with(data5,hist(phen_index2))
with(data5,hist(phen_index2c))

####################################################################

#New measure of phenology: average two dates
data5$phen_index_avg<-(data5$phen_index1+data5$phen_index2c)/2
with(data5,hist(phen_index_avg))
with(data4,hist(phen_index_avg)) #Compare

par(mfrow=c(1,3))
with(data5,hist(phen_index1,main="Mean on 29-30 July"))
with(data5,hist(phen_index2c,main="Mean on 27-29 August, corr"))
with(data5,hist(phen_index_avg,main="Mean, average 2 dates"))

p1<-ggplot(data5, aes(x=phen_index1, fill=pop)) + geom_density(alpha=.3)+theme(legend.position="none")+ggtitle("Mean on 29-30 July")
p2<-ggplot(data5, aes(x=phen_index2c, fill=pop)) + geom_density(alpha=.3)+theme(legend.position="none")+ggtitle("Mean on 27-29 August, corr")
p3<-ggplot(data5, aes(x=phen_index_avg, fill=pop)) + geom_density(alpha=.3)+theme(legend.position="none")+ggtitle("Mean, average 2 dates")
multiplot(p1, p2, p3, cols=3)

p1<-ggplot(data5, aes(x=phen_index1)) + geom_density(alpha=.3)+theme(legend.position="none")+ggtitle("Mean on 29-30 July")
p2<-ggplot(data5, aes(x=phen_index2c)) + geom_density(alpha=.3)+theme(legend.position="none")+ggtitle("Mean on 27-29 August, corr")
p3<-ggplot(data5, aes(x=phen_index_avg)) + geom_density(alpha=.3)+theme(legend.position="none")+ggtitle("Mean, average 2 dates")
multiplot(p1, p2, p3, cols=3)

#New measure of phenology: extrapolated
phenology<-read.table("phenology.txt",header=T,sep=",",dec=".",na.strings=".")
head(phenology)
str(phenology)

p1<-ggplot(phenology,aes(x=phen_avg))+
  geom_histogram(aes(y=..density..),binwidth=.5,colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ggtitle("Average 2 dates")+theme(plot.title=element_text(size=10))
p2<-ggplot(phenology,aes(x=julian_w3))+
  geom_histogram(aes(y=..density..),binwidth=10,colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ggtitle("Julian day when phen=3")+theme(plot.title=element_text(size=10))
p3<-ggplot(phenology,aes(x=julian_w2))+
  geom_histogram(aes(y=..density..),binwidth=10,colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ggtitle("Julian day when phen=2")+theme(plot.title=element_text(size=10))
multiplot(p1, p2, p3, cols=3)

#####################################################################################################
data6<-merge(data5,phenology[c(1,9:10)],by="id_pl")
head(data6)
str(data6)
#####################################################################################################

#Relation among temperature and vegetation height ###################################################
summary(lm(meanT~veg_h_mean*pop,data=data6)) 
#Now interaction is * (due to few pls removed!) but still negative relationship in all 3 pops
summary(lm(t_meanT~t_veg_h_mean+t_veg_h_mean:pop,data=data6)) #data std by pop, negative effect, no int
#If using data std by pop, should not include the main effect of pop, right?

p1<-ggplot(data6, aes(veg_h_mean,meanT, colour = population)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+
  theme(legend.position="none")
p2<-ggplot(data6, aes(t_veg_h_mean,t_meanT, colour = population)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+theme(legend.position="none")
multiplot(p1,p2,cols=2)

summary(lm(meanT~veg_h_mean,data=subset(data6,pop=="H"))) #*
summary(lm(meanT~veg_h_mean,data=subset(data6,pop=="R"))) #*
summary(lm(meanT~veg_h_mean,data=subset(data6,pop=="T"))) #* 


#Relation among phenology and temperature (and Veg h?) 

par(mfrow=c(1,1))
plot(lm(phen_index_avg~meanT+veg_h_mean+pop,data=data6))#Good fit
plot(lm(julian_w3~meanT+veg_h_mean+pop,data=data6))#Not so good fit
plot(lm(julian_w2~meanT+veg_h_mean+pop,data=data6))#Not so good fit
plot(glm(julian_w3~meanT+veg_h_mean+pop,family="poisson",data=data6))#Not so good fit
plot(glm(julian_w2~meanT+veg_h_mean+pop,family="poisson",data=data6))#Not so good fit

#Both temp and veg
#phen_index_avg
model1<-lm(phen_index_avg~(meanT+veg_h_mean)*pop,data=data6,na.action="na.fail")
summary(model1)
models1<-dredge(model1)
summary(model.avg(models1, subset = delta < 2)) #Only one
summary(lm(phen_index_avg~meanT*pop+veg_h_mean,data=data6,na.action="na.fail"))

summary(lm(phen_index_avg~meanT+veg_h_mean,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(lm(phen_index_avg~meanT+veg_h_mean,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(lm(phen_index_avg~meanT+veg_h_mean,data=subset(data6,pop=="T"),na.action="na.fail"))

#Including n_fl as covariate = measure of resource state / size

summary(lm(phen_index_avg~n_fl*pop,data6))

summary(lm(phen_index_avg~n_fl,data=subset(data6,pop=="H")))
summary(lm(phen_index_avg~n_fl,data=subset(data6,pop=="R")))
summary(lm(phen_index_avg~n_fl,data=subset(data6,pop=="T")))

model1<-lm(phen_index_avg~(meanT+veg_h_mean)*pop+n_fl,data=data6,na.action="na.fail")
summary(model1)
models1<-dredge(model1)
summary(model.avg(models1, subset = delta < 2)) #Only one
summary(lm(phen_index_avg~meanT*pop+veg_h_mean+n_fl,data=data6,na.action="na.fail"))

summary(lm(phen_index_avg~n_fl+meanT+veg_h_mean,data=subset(data6,pop=="H")))
summary(lm(phen_index_avg~n_fl+meanT+veg_h_mean,data=subset(data6,pop=="R")))
summary(lm(phen_index_avg~n_fl+meanT+veg_h_mean,data=subset(data6,pop=="T")))

summary(lm(phen_index_avg~meanT+veg_h_mean,data=subset(data6,pop=="H")))
summary(lm(phen_index_avg~meanT+veg_h_mean,data=subset(data6,pop=="R")))
summary(lm(phen_index_avg~meanT+veg_h_mean,data=subset(data6,pop=="T")))

p1<-ggplot(data6,aes(meanT,phen_index_avg,colour=pop))+
  geom_smooth(method="lm",se=T,fullrange=T)+
  geom_point(size=1)+theme(legend.position="none")
p2<-ggplot(data6,aes(veg_h_mean,phen_index_avg,colour=pop))+
  geom_smooth(method="lm",se=T,fullrange=T)+
  geom_point(size=1)+theme(legend.position="none")
multiplot(p1,p2,cols=2)

#julian_w3
model1<-lm(julian_w3~(meanT+veg_h_mean)*pop,data=data6,na.action="na.fail")
summary(model1)
models1<-dredge(model1)
summary(model.avg(models1, subset = delta < 2)) 

summary(lm(julian_w3~meanT+veg_h_mean,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(lm(julian_w3~meanT+veg_h_mean,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(lm(julian_w3~meanT+veg_h_mean,data=subset(data6,pop=="T"),na.action="na.fail"))

#Including n_fl as covariate = measure of resource state / size

summary(lm(julian_w3~n_fl*pop,data6))

summary(lm(julian_w3~n_fl,data=subset(data6,pop=="H")))
summary(lm(julian_w3~n_fl,data=subset(data6,pop=="R")))
summary(lm(julian_w3~n_fl,data=subset(data6,pop=="T")))

model1<-lm(julian_w3~(meanT+veg_h_mean)*pop+n_fl,data=data6,na.action="na.fail")
summary(model1)
models1<-dredge(model1)
summary(model.avg(models1, subset = delta < 2)) 

summary(lm(julian_w3~n_fl+meanT+veg_h_mean,data=subset(data6,pop=="H")))
summary(lm(julian_w3~n_fl+meanT+veg_h_mean,data=subset(data6,pop=="R")))
summary(lm(julian_w3~n_fl+meanT+veg_h_mean,data=subset(data6,pop=="T")))


p1<-ggplot(data6,aes(meanT,julian_w3,colour=pop))+
  geom_smooth(method="lm",se=T,fullrange=T)+
  geom_point(size=1)+theme(legend.position="none")
p2<-ggplot(data6,aes(veg_h_mean,julian_w3,colour=pop))+
  geom_smooth(method="lm",se=T,fullrange=T)+
  geom_point(size=1)+theme(legend.position="none")
multiplot(p1,p2,cols=2)

############################################################################################
#Relation among number of flowers and temperature

with(data6,hist(n_fl))
with(data6,hist(log(n_fl)))

summary(glm.nb(n_fl~meanT+pop,data6)) # No interaction

summary(glm.nb(n_fl~meanT,data=subset(data6,pop=="H")))
summary(glm.nb(n_fl~meanT,data=subset(data6,pop=="R")))
summary(glm.nb(n_fl~meanT,data=subset(data6,pop=="T")))

summary(glm(n_fl~meanT+pop,data6,family="poisson")) #Overdisp

ggplot(data6,aes(meanT,n_fl,colour=pop))+
  geom_smooth(method="glm.nb",se=T,fullrange=F)+
  geom_point(size=1)+theme(legend.position="none")
ggplot(data6,aes(veg_h_mean,n_fl,colour=pop))+
  geom_smooth(method="glm.nb",se=T,fullrange=F)+
  geom_point(size=1)+theme(legend.position="none")


############################################################################################

#Ants - related to temperature and / or vegetation height?
with(data6,hist(n_redants))
with(data6,hist(log(n_redants)))
with(data6,hist(log(n_redants+1))) 
with(data6,hist(sqrt(n_redants)))

summary(glm.nb(n_redants~veg_h_mean+population,na.action="na.fail",data=data6))
summary(glm.nb(n_redants~meanT+population,na.action="na.fail",data=data6))#Stronger coef

model1<-glm.nb(n_redants~(veg_h_mean*meanT)*pop,na.action="na.fail",data=data6)
models1<-dredge(model1)
summary(model.avg(models1, subset = delta < 2))

p1<-ggplot(data6, aes(veg_h_mean,n_redants,colour=pop)) +
  geom_smooth(method = MASS::glm.nb,  se = T,fullrange=F)+
  geom_point(size = 1)+ 
  theme(legend.position="none")
p2<-ggplot(data6, aes(meanT,n_redants,colour=pop)) +
  geom_smooth(method = MASS::glm.nb,  se = T,fullrange=F)+
  geom_point(size = 1)+
  theme(legend.position="none")
multiplot(p1,p2,cols=2)

model1H<-glm.nb(n_redants~veg_h_mean*meanT,data=subset(data6,pop=="H"),na.action="na.fail")
model1R<-glm.nb(n_redants~veg_h_mean*meanT,data=subset(data6,pop=="R"),na.action="na.fail")
model1T<-glm.nb(n_redants~veg_h_mean*meanT,data=subset(data6,pop=="T"),na.action="na.fail")

models1H<-dredge(model1H)
models1R<-dredge(model1R)
models1T<-dredge(model1T)

summary(model.avg(models1H, subset = delta < 2))
models1H[1]
summary(glm.nb(n_redants~veg_h_mean*meanT,data=subset(data6,pop=="H"),na.action="na.fail"))
summary(model.avg(models1R, subset = delta < 2))
summary(model.avg(models1T, subset = delta < 2))

summary(glm.nb(n_redants~veg_h_mean*meanT,data=subset(data6,pop=="R"),na.action="na.fail"))
summary(glm.nb(n_redants~veg_h_mean*meanT,data=subset(data6,pop=="T"),na.action="na.fail"))


#Effect of veg h at mean temperature in pop H
with(subset(data6,pop=="H"),mean(meanT)) #16.655
with(subset(data6,pop=="H"),mean(veg_h_mean)) #35

visreg(model1H)
visreg(model1H,"veg_h_mean",by="meanT") #10th, 50th, and 90th quantiles
visreg(model1H,"meanT",by="veg_h_mean")

visreg(model1H,"veg_h_mean",by="meanT",breaks=5)
visreg(model1H,"veg_h_mean",by="meanT",breaks=c(14.09787,16.655,19.6766))
visreg(model1H,"veg_h_mean",by="meanT",breaks=c(14.09787,16.655,19.6766),
       scale="response")

visreg(model1H,"veg_h_mean",by="meanT",type="conditional") 
#Value of the variable on the x-axis and  change in response on the y-axis, 
#holding all other variables (temp) constant at 10th, 50th, and 90th quantiles
visreg(model1H,"veg_h_mean",by="meanT",type="contrast") 
#Effect on the expected value of the response (ants) by moving the x variable away 
#from a reference point on the x-axis (mean)
visreg(model1H,"veg_h_mean",type="contrast") 

plot(effect(term="veg_h_mean:meanT", mod=model1H),multiline=T)


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

