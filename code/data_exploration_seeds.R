#####################################################################################
########################    Data exploration seed counting   ########################
#####################################################################################

library(lme4)
library(sciplot)
library(RVAideMemoire)
library(lmerTest)

#Reading data from plants where we have both up and p fruits (from the same plant)
#N indicates the number of fruits (if more than 1, values are averages of this number of fruits)

dataseeds1<-read.table("./data/clean/seeds_up_p_means.txt",header=T,sep="\t",dec=",")
head(dataseeds1)
tail(dataseeds1)
str(dataseeds1)

hist(dataseeds1$n_dev)
hist(log(dataseeds1$n_dev)+1)
hist(dataseeds1$perc_dev)
hist(dataseeds1$mean_w_dev_mcg)

#Does number of developed seeds differ between up and p fruits?
#n_dev rounded to 0 decimals because poisson family in glmer "prefers" integers
mod1<-glmer(round(n_dev)~p_up+(1|pl_id),family="poisson",data=dataseeds1)
summary(mod1)
overdisp.glmer(mod1) #There is overdispersion

#Add observation level random effect
dataseeds1$id<-1:60
mod1<-glmer(round(n_dev)~p_up+(1|pl_id)+(1|id),family="poisson",data=dataseeds1)
summary(mod1)

overdisp.glmer(mod1) #There is not overdispersion! - Model OK

#Does proportion of developed seeds differ between up and p fruits?
#Prepare perc_dev for using binomial family in glmer, rounded to 0 decimals too
dataseeds1$prop_dev<-cbind(round(dataseeds1$n_dev),round(dataseeds1$n_total))
mod2<-glmer(prop_dev~p_up+(1|pl_id),family="binomial",data=dataseeds1)
summary(mod2)
overdisp.glmer(mod2) #There is overdispersion

mod2<-glmer(prop_dev~p_up+(1|pl_id)+(1|id),family="binomial",data=dataseeds1)
summary(mod2)

overdisp.glmer(mod2) #There is not overdispersion! - Model OK

#Does mean seed weight (for developed seeds) differ between up and p fruits?
#Approx. Gaussian distribution - lmer
mod3<-lmer(mean_w_dev_mcg~p_up+(1|pl_id),data=dataseeds1)
summary(mod3)

pdf("./results/figures/seeds_p_up.pdf", family="Times",width=7,height=3)
par(mfrow=c(1,3),cex=0.8,cex.lab=1.1)
with(dataseeds1,lineplot.CI(p_up,n_dev,cex=2,
     xlab="",ylab="N developed seeds",type="p",bty="l",legend=F))
with(dataseeds1,lineplot.CI(p_up,perc_dev,cex=2,
     xlab="",ylab="% developed seeds",type="p",bty="l",legend=F))
with(dataseeds1,lineplot.CI(p_up,mean_w_dev_mcg,cex=2,
     xlab="",ylab="Weight developed seeds (µg)",type="p",bty="l",legend=F))
dev.off()


par(mfrow=c(1,1),cex.lab=1)

with(subset(dataseeds1,!is.na(n_dev)),interaction.plot(ripe_when_collect,p_up,n_dev,type="b", 
     ylim=c(100,600),col=c(1:3), lwd=2, pch=c(15,16,17)))
with(subset(dataseeds1,!is.na(perc_dev)),interaction.plot(ripe_when_collect,p_up,perc_dev,type="b", 
     col=c(1:3), lwd=2, pch=c(15,16,17)))
with(subset(dataseeds1,!is.na(mean_w_dev_mcg)),interaction.plot(ripe_when_collect,p_up,mean_w_dev_mcg,type="b", 
    col=c(1:3), lwd=2, pch=c(15,16,17)))

###########################################################################################
#Reading data from all plants where we have fruits

dataseeds2<-read.table("./data/clean/seeds_means.txt",header=T,sep="\t",dec=",")
head(dataseeds2)
tail(dataseeds2)
str(dataseeds2)

dataseeds2$prop_dev<-cbind(round(dataseeds2$n_dev),round(dataseeds2$n_total))

hist(dataseeds2$n_dev)
hist(dataseeds2$perc_dev)
hist(dataseeds2$mean_w_dev_mcg)

#Differences among populations
mod3<-glmer(round(n_dev)~population2+(1|pl_id),family="poisson",data=dataseeds2)
summary(mod3)
overdisp.glmer(mod3)
#Add observation level random effect
dataseeds2$id<-1:179
mod3<-glmer(round(n_dev)~population2+(1|pl_id)+(1|id),family="poisson",data=dataseeds2)
summary(mod3)
overdisp.glmer(mod3)

mod4<-glmer(prop_dev~population2+(1|pl_id),family="binomial",data=dataseeds2)
summary(mod4)
overdisp.glmer(mod4)
mod4<-glmer(prop_dev~population2+(1|pl_id)+(1|id),family="binomial",data=dataseeds2)
summary(mod4)
overdisp.glmer(mod4)

mod5<-lmer(mean_w_dev_mcg~population2+(1|pl_id),data=dataseeds2)
summary(mod5)

pdf("./results/figures/seeds_pops.pdf", family="Times",width=9,height=3)
par(mfrow=c(1,3),cex=0.8,cex.lab=1.1)
with(dataseeds2,lineplot.CI(population2,n_dev,group=population2,pch=c(20,20,20),
    xlab="Population",ylab="Number of developed seeds",type="p",cex=2,bty="l",legend=F))
with(dataseeds2,lineplot.CI(population2,perc_dev,group=population2,pch=c(20,20,20),
    xlab="Population",ylab="Percentage of developed seeds",type="p",cex=2,bty="l",legend=F))
with(dataseeds2,lineplot.CI(population2,mean_w_dev_mcg,group=population2,pch=c(20,20,20),
    xlab="Population",ylab="Weight developed seeds (µg)",type="p",cex=2,bty="l",legend=F))
dev.off()

#Differences among ripe_unripe
mod6<-glmer(round(n_dev)~ripe_when_collect+(1|pl_id)+(1|id),family="poisson",data=dataseeds2)
summary(mod6)
overdisp.glmer(mod6)

mod7<-glmer(prop_dev~ripe_when_collect+(1|pl_id)+(1|id),family="binomial",data=dataseeds2)
summary(mod7)
overdisp.glmer(mod7)

mod8<-lmer(mean_w_dev_mcg~ripe_when_collect+(1|pl_id),data=dataseeds2)
summary(mod8)

with(dataseeds2,lineplot.CI(ripe_when_collect,n_dev,group=population2,
                            xlab="",ylab="Number of developed seeds",type="p",cex=2,bty="l",legend=T))
with(dataseeds2,lineplot.CI(ripe_when_collect,perc_dev,group=population2,
                            xlab="",ylab="Percentage of developed seeds",type="p",cex=2,bty="l",legend=F))
with(dataseeds2,lineplot.CI(ripe_when_collect,mean_w_dev_mcg,group=population2,
                            xlab="",ylab="Weight developed seeds (µg)",type="p",cex=2,bty="l",legend=F))

#Differences among ripe_unripe AND population
mod9<-glmer(round(n_dev)~ripe_when_collect*population2+(1|pl_id)+(1|id),family="poisson",data=dataseeds2)
summary(mod9)
Anova(mod9,type="II")
mod10<-glmer(prop_dev~ripe_when_collect*population2+(1|pl_id)+(1|id),family="binomial",data=dataseeds2)
summary(mod10)
Anova(mod10,type="II")
mod11<-lmer(mean_w_dev_mcg~ripe_when_collect*population2+(1|pl_id),data=dataseeds2)
summary(mod11)
Anova(mod11,type="II")


with(subset(dataseeds2,!is.na(n_dev)),interaction.plot(ripe_when_collect,population2,n_dev,type="b", 
    ylim=c(100,600),col=c(1:3), lwd=2, pch=c(15,16,17)))
with(subset(dataseeds2,!is.na(perc_dev)),interaction.plot(ripe_when_collect,population2,perc_dev,type="b", 
    col=c(1:3), lwd=2, pch=c(15,16,17)))
with(subset(dataseeds2,!is.na(mean_w_dev_mcg)),interaction.plot(ripe_when_collect,population2,mean_w_dev_mcg,type="b", 
    col=c(1:3), lwd=2, pch=c(15,16,17)))

#Differences among ripe_unripe AND p_up
mod12<-glmer(round(n_dev)~ripe_when_collect*p_up+(1|pl_id)+(1|id),family="poisson",data=dataseeds2)
summary(mod12)
Anova(mod12,type="II")
mod13<-glmer(prop_dev~ripe_when_collect*p_up+(1|pl_id)+(1|id),family="binomial",data=dataseeds2)
summary(mod13)
Anova(mod13,type="II")
mod14<-lmer(mean_w_dev_mcg~ripe_when_collect*p_up+(1|pl_id),data=dataseeds2)
summary(mod14)
Anova(mod14,type="II")

with(subset(dataseeds2,!is.na(n_dev)),interaction.plot(ripe_when_collect,p_up,n_dev,type="b", 
     ylim=c(100,600),col=c(1:3), lwd=2, pch=c(15,16,17)))
with(subset(dataseeds2,!is.na(perc_dev)),interaction.plot(ripe_when_collect,p_up,perc_dev,type="b", 
     col=c(1:3), lwd=2, pch=c(15,16,17)))
with(subset(dataseeds2,!is.na(mean_w_dev_mcg)),interaction.plot(ripe_when_collect,p_up,mean_w_dev_mcg,type="b", 
     col=c(1:3), lwd=2, pch=c(15,16,17)))






