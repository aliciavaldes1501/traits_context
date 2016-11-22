#####################################################################################
################### Code for analysing data on marked plants 2015 ###################
#####################################################################################
#Reading data from ~100 marked plants per population (Peñas not included)
data1<-read.table("datafile1.txt",header=T,sep="\t",dec=",")
head(data1)
tail(data1)
str(data1)

#Construct new variable
#Is plant higher than vegetation?
data1$higher_veg_max<-as.factor(with(data1,ifelse(diff_veg_h_max_shoot_h<0,"1","0")))
data1$higher_veg_mean<-as.factor(with(data1,ifelse(diff_veg_h_mean_shoot_h<0,"1","0")))

#Construct new variable: attack
data1$attack<-as.factor(with(data1,ifelse(n_eggs_max>0,"1","0")))

#Construct new variable: redants_pres
data1$redants_pres<-as.factor(with(data1,ifelse(n_redants>0,"1","0")))

#Construct new variable: prop_pred
data1$prop_pred<-with(data1,n_predated/n_fl_corrected)

#Construct new variable: observation-level random effect

data1$id<-1:309

  

summary(data1)
names(data1)

with(data1,plot(attack,most_adv))
with(data1,summary(lm(most_adv~attack)))
with(data1,summary(lm(fruit_set~attack)))
with(data1,plot(attack,n_fruits_total))
with(data1,summary(lm(n_fruits_total~attack)))

#Correlations
library(PerformanceAnalytics)

head(data1[c(4,7,18,21:27,31)])
data1corr<-data1[c(4,7,18,21:27,31)]
chart.Correlation(data1corr)

head(data1[c(3:9,17:29,31:34)])
data1corr<-data1[c(3:9,17:29,31:34)]
cor(data1corr,use="pairwise.complete.obs" )
write.table(cor(data1corr,use="pairwise.complete.obs" ),"clipboard",sep="\t")

head(data1[c(3,17:18,21:26,31,34)])
data1corr<-data1[c(3,17:18,21:26,31,34)]
chart.Correlation(data1corr,pch=20)


#Temperature and phenology

with(data1,plot(avg_d_mean,phen_index,pch=20))
abline(with(data1,lm(phen_index ~ avg_d_mean)))
summary(with(data1,lm(phen_index ~ avg_d_mean)))
text(x = 12.8, y = 3.9, labels = "p<0.001")
with(data1,summary(glm(phen_index~avg_d_mean,family="gaussian")))

summary(with(subset(data1,population=="Högsjön"),lm(phen_index~avg_d_mean)))
with(subset(data1,population=="Högsjön"),plot(avg_d_mean,phen_index))
abline(with(subset(data1,population=="Högsjön"),lm(phen_index~avg_d_mean)))

summary(with(subset(data1,population=="Remmene"),lm(phen_index~avg_d_mean)))

summary(with(subset(data1,days_datalog==33),lm(phen_index~avg_d_mean)))
with(subset(data1,days_datalog==33),plot(avg_d_mean,phen_index))
abline(with(subset(data1,days_datalog==33),lm(phen_index~avg_d_mean)))


with(data1,plot(avg_d_max,phen_index))
abline(with(data1,lm(phen_index~avg_d_max)))
summary(with(data1,lm(phen_index~avg_d_max)))

summary(with(subset(data1,population=="Högsjön"),lm(phen_index~avg_d_max)))
with(subset(data1,population=="Högsjön"),plot(avg_d_max,phen_index))
abline(with(subset(data1,population=="Högsjön"),lm(phen_index~avg_d_max)))

summary(with(subset(data1,population=="Remmene"),lm(phen_index~avg_d_max)))

summary(with(subset(data1,days_datalog==33),lm(phen_index~avg_d_max)))


with(data1,plot(avg_d_mean,as.factor(most_adv),pch=20))
with(data1,abline(lm(most_adv~avg_d_mean)))
with(data1,summary(lm(most_adv~avg_d_mean)))
text(x = 12.8, y = 4.9, labels = "p<0.001")
with(data1,summary(glm(most_adv~avg_d_mean,family="poisson")))
with(data1,plot(glm(most_adv~avg_d_mean,family="poisson")))


summary(with(subset(data1,population=="Högsjön"),lm(most_adv~avg_d_mean)))
with(subset(data1,population=="Högsjön"),plot(avg_d_mean,most_adv))
abline(with(subset(data1,population=="Högsjön"),lm(most_adv~avg_d_mean)))

summary(with(subset(data1,population=="Remmene"),lm(most_adv~avg_d_mean)))

summary(with(subset(data1,days_datalog==33),lm(most_adv~avg_d_mean)))
with(subset(data1,days_datalog==33),plot(avg_d_mean,most_adv))
abline(with(subset(data1,days_datalog==33),lm(most_adv~avg_d_mean)))

xyplot(most_adv~avg_d_mean, 
       groups = population,
       par.settings=simpleTheme(pch=20,cex=1.3),
       panel = panel.superpose,
       panel.groups=function(x, y, col, col.symbol, ...) {
         panel.xyplot(x, y, col=col.symbol, ...)
         panel.lmline(x, y, col.line=col.symbol)
       },
       grid = F,
       auto.key = list(title='Population', 
                       text=c("H  p=0.050", "R  p=0.635", "T  p=0.049"),
                       corner = c(0,1), cex=1.0)
)

xyplot(phen_index~avg_d_mean | population, 
       panel=function(x, y){ 
         panel.xyplot(x, y) 
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       } 
)

xyplot(
  most_adv~avg_d_mean,
  groups = population,
  panel = panel.superpose, 
  panel.groups=function(x, y, col, col.symbol, ...) {
    panel.xyplot(x, y, col=col.symbol, ...)
    panel.lmline(x, y, col.line=col.symbol)
  },
  grid = F,
  auto.key = list(title='Population', space='right')
)

xyplot(
  phen_index~avg_d_mean,
  groups = population,
  par.settings=simpleTheme(pch=20,cex=1.3),
    panel = panel.superpose,
  panel.groups=function(x, y, col, col.symbol, ...) {
    panel.xyplot(x, y, col=col.symbol, ...)
    panel.lmline(x, y, col.line=col.symbol)
  },
  grid = F,
  auto.key = list(title='Population', 
  text=c("H  p<0.001", "R  p=0.732", "T  p=0.020"),
  corner = c(0,1), cex=1.0)
)

#Temperature and number of flowers

with(data1,plot(avg_d_mean,n_fl_corrected,pch=20))
abline(with(data1,lm(n_fl_corrected ~ avg_d_mean)))
summary(with(data1,lm(n_fl_corrected ~ avg_d_mean)))
text(x = 13, y = 17, labels = "p<0.001")

xyplot(
  n_fl_corrected~avg_d_mean,
  groups = population,
  par.settings=simpleTheme(pch=20,cex=1.3),
  panel = panel.superpose,
  panel.groups=function(x, y, col, col.symbol, ...) {
    panel.xyplot(x, y, col=col.symbol, ...)
    panel.lmline(x, y, col.line=col.symbol)
  },
  grid = F,
  auto.key = list(title='Population', 
                  text=c("H  p<0.001", "R  p=0.732", "T  p=0.020"),
                  corner = c(0,1), cex=1.0)
)

###########################################################################
with(data1,plot(n_eggs_max,n_predated))
abline(with(data1,lm(n_predated~n_eggs_max)))
summary(with(data1,lm(n_predated~n_eggs_max)))

with(data1,plot(n_eggs_max,prop_pred))
abline(with(data1,lm(prop_pred~n_eggs_max)))
summary(with(data1,lm(prop_pred~n_eggs_max)))
###########################################################################

#Boxplots
for (i in 3:ncol(data1)){boxplot(as.numeric(data1[,i]),main=paste(colnames(data1)[i]))}

#Histograms
for (i in 3:ncol(data1)){hist(as.numeric(data1[,i]),main=paste(colnames(data1)[i]))}

############################################################################

#Differences among populations
#Histograms
par(mfrow=c(1,3),
    oma = c(5,4,1,1) + 0.1,
    mar = c(3,3,2,2) + 0.1)
for (i in c(3:9,16:18,21:27,31:39)){plot(data1$population,data1[,i],main=paste(colnames(data1)[i]))}

for (i in 3:9){print(summary(aov(as.numeric(data1[,i])~data1$population)))}
for (i in 16:ncol(data1)){print(summary(aov(as.numeric(data1[,i])~data1$population)))}

for (i in 3:9){print(TukeyHSD(aov(as.numeric(data1[,i])~population)))}
for (i in 16:ncol(data1)){print(TukeyHSD(aov(as.numeric(data1[,i])~population)))}



#What determines interaction (intensity)?
#Distribution fitting
library(vcd)
library(MASS)
x<-seq(0,100,0.01) 

hist(data1$n_eggs_max, prob=TRUE)
curve(dnorm(x, mean=mean(data1$n_eggs_max), sd=sd(data1$n_eggs_max)), add=TRUE)
fitdistr(data1$n_eggs_max,"poisson")
summary(goodfit(data1$n_eggs_max,type="poisson"))

#Correlations between possible explanatory variables

cor(data1[c(3:4,7:9,17:18,25,27,31,39)],use="pairwise.complete.obs")


###############################################################################
#What determines ants?
plot(avg_d_mean,n_redants,pch=20)
abline(lm(n_redants~avg_d_mean))
summary(lm(n_redants~avg_d_mean))
text(x = 12.8, y = 58, labels = "p<0.001")

summary(with(subset(data1,population=="Högsjön"),lm(n_redants~avg_d_mean)))
summary(with(subset(data1,population=="Remmene"),lm(n_redants~avg_d_mean)))
summary(with(subset(data1,days_datalog==33),lm(n_redants~avg_d_mean)))

xyplot(n_redants~avg_d_mean, 
       groups=population,
       par.settings=simpleTheme(pch=20,cex=1.3),
       panel = panel.superpose,
       panel.groups=function(x, y, col, col.symbol, ...) {
         panel.xyplot(x, y, col=col.symbol, ...)
         panel.lmline(x, y, col.line=col.symbol)
       },
       grid = F,
       auto.key = list(title='Population', 
                       text=c("H  p=0.391", "R  p=0.0315", "T  p=0.0417"),
                       corner = c(0,1), cex=1.0)
)




#What determines temperature?
plot(veg_h_mean,avg_d_mean,pch=20)
abline(lm(avg_d_mean~veg_h_mean))
summary(lm(avg_d_mean~veg_h_mean))
text(x = 100, y = 17.5, labels = "p<0.001")

summary(with(subset(data1,population=="Högsjön"),lm(avg_d_mean~veg_h_mean)))
summary(with(subset(data1,population=="Remmene"),lm(avg_d_mean~veg_h_mean)))
summary(with(subset(data1,days_datalog==33),lm(avg_d_mean~veg_h_mean)))

xyplot(avg_d_mean~veg_h_mean, 
       groups=population,
       par.settings=simpleTheme(pch=20,cex=1.3),
       panel = panel.superpose,
       panel.groups=function(x, y, col, col.symbol, ...) {
         panel.xyplot(x, y, col=col.symbol, ...)
         panel.lmline(x, y, col.line=col.symbol)
       },
       grid = F,
       auto.key = list(title='Population', 
                       text=c("H  p=0.001", "R  p=0.030", "T  p=0.007"),
                       corner = c(1,1), cex=1.0)
)
###############################################################################
plot(veg_h_mean,diff_veg_h_mean_shoot_h)
summary(lm(diff_veg_h_mean_shoot_h~veg_h_mean))
abline(lm(diff_veg_h_mean_shoot_h~veg_h_mean))

plot(veg_h_mean,shoot_h)
plot(shoot_h,diff_veg_h_mean_shoot_h)








