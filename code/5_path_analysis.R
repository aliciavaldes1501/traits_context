#Try path models for interaction (intensity)
library(lavaan)
library(semPlot)

#attack
data1comp$attack<-as.ordered(data1comp$attack)
data1comp$n_eggs_max<-as.ordered(data1comp$n_eggs_max)

path1<- ''
fit1<-sem(path1,data=data1comp,std.ov=F,orthogonal=F)
summary(fit1, standardized=T, fit.measures=TRUE, rsquare=TRUE,modindices=T) # Get summary
semPaths(fit1, what="stand", layout="circle", intercepts=FALSE) # Graph with standardized estimates

fit1<-sem(path1,data=subset(data1comp,population=="Högsjön"),std.lv=F,std.ov=F)
summary(fit1, standardized=T) # Get summary
fit1<-sem(path1,data=subset(data1comp,population=="Remmene"),std.lv=F,std.ov=F)
summary(fit1, standardized=T) # Get summary
fit1<-sem(path1,data=subset(data1comp,population!="Högsjön"&population!="Remmene"),std.lv=F,std.ov=F)
summary(fit1, standardized=T) # Get summary

#eggs

path2<- ' # regressions
z.n_eggs_max~z.avg_d_mean+z.n_redants+z.most_adv+z.n_fl_corrected+z.veg_h_mean
z.n_redants~z.avg_d_mean+z.veg_h_mean
z.most_adv~z.avg_d_mean
z.n_fl_corrected~z.avg_d_mean
z.avg_d_mean~z.veg_h_mean
# variances and covariances 
z.most_adv ~~ z.n_fl_corrected 
'

path2<- ' # regressions
n_eggs_max~avg_d_min+n_redants+most_adv+veg_h_mean
n_redants~avg_d_min
most_adv~avg_d_min
avg_d_min~veg_h_mean
# variances and covariances 
most_adv ~~  avg_d_min
'
fit2<-sem(path2,data=data1comp) #But not standardized --> see here!
#Declare eggs as ordered or not? Try with Amos?
summary(fit2, standardized=T, fit.measures=TRUE, rsquare=TRUE,modindices=T) # Get summary
semPaths(fit2, what="stand", layout="circle", intercepts=FALSE) # Graph with standardized estimates

fit2<-sem(path2,data=subset(data1comp,population=="Högsjön"),std.lv=F,std.ov=F)
summary(fit2, standardized=T) # Get summary
fit2<-sem(path2,data=subset(data1comp,population=="Remmene"),std.lv=F,std.ov=F)
summary(fit2, standardized=T) # Get summary
fit2<-sem(path2,data=subset(data1comp,population!="Högsjön"&population!="Remmene"),std.lv=F,std.ov=F)
summary(fit2, standardized=T) # Get summary

#prop_pred

data1$prop_pred1<-with(data1,n_predated/n_fl_corrected)
data1comp$prop_pred1<-with(data1comp,n_predated/n_fl_corrected)

z <- scale(data1[,c("prop_pred1")])
data1$z.prop_pred1 <- z[,1]

z <- scale(data1comp[,c("prop_pred1")])
data1comp$z.prop_pred1 <- z[,1]

path3<- ' # regressions
z.prop_pred1~z.avg_d_mean+z.n_redants+z.most_adv+z.n_fl_corrected+z.veg_h_mean
z.n_redants~z.avg_d_mean+z.veg_h_mean
z.most_adv~z.avg_d_mean
z.n_fl_corrected~z.avg_d_mean
z.avg_d_mean~z.veg_h_mean
# variances and covariances 
z.most_adv ~~ z.n_fl_corrected 
'
fit3<-sem(path3,data=data1comp,std.lv=F,std.ov=F)
summary(fit3, standardized=T) # Get summary
semPaths(fit3, what="stand", layout="circle", intercepts=FALSE) # Graph with standardized estimates

fit3<-sem(path3,data=subset(data1comp,population=="Högsjön"),std.lv=F,std.ov=F)
summary(fit3, standardized=T) # Get summary
fit3<-sem(path3,data=subset(data1comp,population=="Remmene"),std.lv=F,std.ov=F)
summary(fit3, standardized=T) # Get summary
fit3<-sem(path3,data=subset(data1comp,population!="Högsjön"&population!="Remmene"),std.lv=F,std.ov=F)
summary(fit3, standardized=T) # Get summary



