#####################################################################################
################### Code for analysing data on marked plants 2015 ###################
#####################################################################################

#Reading data from ~100 marked plants per population (Peñas included)
data1wP<-read.table("datafile1_wP.txt",header=T,sep="\t",dec=",")
head(data1wP)
tail(data1wP)
str(data1wP)

#Construct new variable
#Is plant higher than vegetation?
data1wP$higher_veg_max<-as.factor(with(data1wP,ifelse(diff_veg_h_max_shoot_h<0,"1","0")))
data1wP$higher_veg_mean<-as.factor(with(data1wP,ifelse(diff_veg_h_mean_shoot_h<0,"1","0")))

#Construct new variable: attack
data1wP$attack<-as.factor(with(data1wP,ifelse(n_eggs_max>0,"1","0")))

#Construct new variable: redants_pres
data1wP$redants_pres<-as.factor(with(data1wP,ifelse(n_redants>0,"1","0")))

#Construct new variable: prop_pred
data1wP$prop_pred<-with(data1wP,n_predated/n_fl_corrected)

#Construct new variable: observation-level random effect

data1wP$id<-1:419



summary(data1wP)
names(data1wP)

with(data1wP,plot(attack,most_adv_p))
with(data1wP,summary(lm(fruit_set~attack)))
with(data1wP,plot(attack,n_fruits_total_max))
with(data1wP,summary(lm(n_fruits_total_max~attack)))

###########################################################################
with(data1wP,plot(n_eggs_max,n_predated))
abline(with(data1wP,lm(n_predated~n_eggs_max)))
summary(with(data1wP,lm(n_predated~n_eggs_max)))

with(data1wP,plot(n_eggs_max,prop_pred))
abline(with(data1wP,lm(prop_pred~n_eggs_max)))
summary(with(data1wP,lm(prop_pred~n_eggs_max)))
###########################################################################

#Boxplots
for (i in 3:ncol(data1wP)){boxplot(as.numeric(data1wP[,i]),main=paste(colnames(data1wP)[i]))}

#Histograms
for (i in 3:ncol(data1wP)){hist(as.numeric(data1wP[,i]),main=paste(colnames(data1wP)[i]))}

############################################################################

#Differences among populations
#Histograms
par(mfrow=c(2,2))
for (i in 3:9){plot(data1wP$population,data1wP[,i],main=paste(colnames(data1wP)[i]))}
for (i in 16:ncol(data1wP)){plot(data1wP$population,data1wP[,i],main=paste(colnames(data1wP)[i]))}
for (i in 3:9){print(summary(aov(as.numeric(data1wP[,i])~data1wP$population)))}
for (i in 16:ncol(data1wP)){print(summary(aov(as.numeric(data1wP[,i])~data1wP$population)))}

for (i in 3:9){print(TukeyHSD(aov(as.numeric(data1wP[,i])~population)))}
for (i in 16:ncol(data1wP)){print(TukeyHSD(aov(as.numeric(data1wP[,i])~population)))}



#What determines interaction (intensity)?
#Distribution fitting
library(vcd)
library(MASS)
x<-seq(0,100,0.01) 

hist(data1wP$n_eggs_max, prob=TRUE)
curve(dnorm(x, mean=mean(data1wP$n_eggs_max), sd=sd(data1wP$n_eggs_max)), add=TRUE)
fitdistr(data1wP$n_eggs_max,"poisson")
summary(goodfit(data1wP$n_eggs_max,type="poisson"))

#Correlations between possible explanatory variables

cor(data1wP[c(3:4,7:9,17:18,25,27,31,39)],use="pairwise.complete.obs")

#Standardize X variables
z <- scale(data1wP[,c("bud_h","shoot_h","veg_h_mean","diff_veg_h_max_shoot_h","diff_veg_h_mean_shoot_h",
                    "phen_index","most_adv","n_fl_corrected","n_redants",
                    "dist_closest_redants","avg_d_mean","avg_d_sd","avg_d_min","avg_d_max")])
data1wP$z.bud_h <- z[,1]
data1wP$z.shoot_h <- z[,2]
data1wP$z.veg_h_mean <- z[,3]
data1wP$z.diff_veg_h_max_shoot_h <- z[,4]
data1wP$z.diff_veg_h_mean_shoot_h <- z[,5]
data1wP$z.phen_index <- z[,6]
data1wP$z.most_adv <- z[,7]
data1wP$z.n_fl_corrected <- z[,8]
data1wP$z.n_redants <- z[,9]
data1wP$z.dist_closest_redants <- z[,10]
data1wP$z.avg_d_mean <- z[,11]
data1wP$z.avg_d_sd <- z[,12]
data1wP$z.avg_d_min <- z[,13]
data1wP$z.avg_d_max <- z[,14]

head(data1wP)
summary(data1wP)

attach(data1wP)

#GLMMs
#attack
model2<-glmer(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected+z.n_redants
              +(1|population),family="binomial",na.action = "na.fail")
summary(model2)
r.squaredLR(model2,null.RE=T) 

model2_P<-glm(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected,
              data=subset(data1wP,population=="Peñas"),family="binomial",na.action = "na.fail")
summary(model2_P)
r.squaredLR(model2_P) 

multiplot(model2,model2_H,model2_R,model2_T,model2_P,outerCI=0,horizontal=T)+
  geom_hline(linetype="dotted", yintercept=c(1.5,2.5,3.5,4.5,5.5,6.5))+ theme_bw()

#eggs
model2eg_<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                   z.n_fl_corrected
                 +(1|population)+(1|id),family="poisson",na.action = "na.fail",REML=T,
                 control=glmerControl(optimizer="bobyqa"))
summary(model2eg_)

model2eg_P<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                   z.n_fl_corrected
                 +(1|id),data=subset(data1wP,population=="Peñas"),
                 family="poisson",na.action = "na.fail",REML=T,
                 control=glmerControl(optimizer="bobyqa"))
summary(model2eg_P)

#prop_pred





