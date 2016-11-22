#Path models for fitness

#Standardize also fruits

z <- scale(data1[,c("n_intact_fruits_max","fruit_set")])
data1$z.n_intact_fruits_max <- z[,1]
data1$z.fruit_set1 <- z[,2]

z <- scale(data1comp[,c("n_intact_fruits_max","fruit_set")])
data1comp$z.n_intact_fruits_max <- z[,1]
data1comp$z.fruit_set <- z[,2]

#With attack
path1f<- ' # regressions
z.n_intact_fruits_max~attack+z.avg_d_mean+z.most_adv+z.n_fl_corrected
attack~z.avg_d_mean+z.n_redants+z.most_adv+z.n_fl_corrected+z.veg_h_mean
z.n_redants~z.avg_d_mean+z.veg_h_mean
z.most_adv~z.avg_d_mean
z.n_fl_corrected~z.avg_d_mean
z.avg_d_mean~z.veg_h_mean
# variances and covariances 
z.most_adv ~~ z.n_fl_corrected 
'
fit1f<-sem(path1f,data=data1comp,std.lv=F,std.ov=F)
summary(fit1f, standardized=T) # Get summary
semPaths(fit1f, what="stand", layout="circle", intercepts=FALSE) # Graph with standardized estimates

fit1f<-sem(path1f,data=subset(data1comp,population=="Högsjön"),std.lv=F,std.ov=F)
summary(fit1f, standardized=T) # Get summary
fit1f<-sem(path1f,data=subset(data1comp,population=="Remmene"),std.lv=F,std.ov=F)
summary(fit1f, standardized=T) # Get summary
fit1f<-sem(path1f,data=subset(data1comp,population!="Högsjön"&population!="Remmene"),std.lv=F,std.ov=F)
summary(fit1f, standardized=T) # Get summary

#With eggs
path2f<- ' # regressions
z.n_intact_fruits_max~z.n_eggs_max+z.avg_d_mean+z.most_adv+z.n_fl_corrected
z.n_eggs_max~z.avg_d_mean+z.n_redants+z.most_adv+z.n_fl_corrected+z.veg_h_mean
z.n_redants~z.avg_d_mean+z.veg_h_mean
z.most_adv~z.avg_d_mean
z.n_fl_corrected~z.avg_d_mean
z.avg_d_mean~z.veg_h_mean
# variances and covariances 
z.most_adv ~~ z.n_fl_corrected 
'
fit2f<-sem(path2f,data=data1comp,std.lv=F,std.ov=F)
summary(fit2f, standardized=T) # Get summary
semPaths(fit2f, what="stand", layout="circle", intercepts=FALSE) # Graph with standardized estimates

fit2f<-sem(path2f,data=subset(data1comp,population=="Högsjön"),std.lv=F,std.ov=F)
summary(fit2f, standardized=T) # Get summary
fit2f<-sem(path2f,data=subset(data1comp,population=="Remmene"),std.lv=F,std.ov=F)
summary(fit2f, standardized=T) # Get summary
fit2f<-sem(path2f,data=subset(data1comp,population!="Högsjön"&population!="Remmene"),std.lv=F,std.ov=F)
summary(fit2f, standardized=T) # Get summary

#With prop_pred
path3f<- ' # regressions
z.n_intact_fruits_max~z.prop_pred1+z.avg_d_mean+z.most_adv+z.n_fl_corrected
z.prop_pred1~z.avg_d_mean+z.n_redants+z.most_adv+z.n_fl_corrected+z.veg_h_mean
z.n_redants~z.avg_d_mean+z.veg_h_mean
z.most_adv~z.avg_d_mean
z.n_fl_corrected~z.avg_d_mean
z.avg_d_mean~z.veg_h_mean
# variances and covariances 
z.most_adv ~~ z.n_fl_corrected 
'
fit3f<-sem(path3f,data=data1comp,std.lv=F,std.ov=F)
summary(fit3f, standardized=T) # Get summary
semPaths(fit3f, what="stand", layout="circle", intercepts=FALSE) # Graph with standardized estimates

fit3f<-sem(path3f,data=subset(data1comp,population=="Högsjön"),std.lv=F,std.ov=F)
summary(fit3f, standardized=T) # Get summary
fit3f<-sem(path3f,data=subset(data1comp,population=="Remmene"),std.lv=F,std.ov=F)
summary(fit3f, standardized=T) # Get summary
fit3f<-sem(path3f,data=subset(data1comp,population!="Högsjön"&population!="Remmene"),std.lv=F,std.ov=F)
summary(fit3f, standardized=T) # Get summary





