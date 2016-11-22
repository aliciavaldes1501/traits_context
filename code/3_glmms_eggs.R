#Response=n_eggs_max

model1eg<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.phen_index+
                  z.n_fl_corrected+z.n_redants+z.avg_d_mean
                +(1|population),family="poisson",na.action = "na.fail")
summary(model1eg)
r.squaredLR(model1eg,null.RE=T) 

model2eg<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                  z.n_fl_corrected+z.n_redants+z.avg_d_mean
                +(1|population),family="poisson",na.action = "na.fail")
summary(model2eg)
r.squaredLR(model2eg,null.RE=T) 

model3eg<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.bud_h+
                  z.n_fl_corrected+z.n_redants+z.avg_d_mean
                +(1|population),family="poisson",na.action = "na.fail")
summary(model3eg)

model4eg<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                  z.n_fl_corrected+z.n_redants+z.avg_d_max
                +(1|population),family="poisson",na.action = "na.fail")
summary(model4eg)

AIC(model1eg,model2eg,model3eg,model4eg)

#Keep model2eg

summary(model2eg,correlation=F)

#Model diagnostics
plot(model2eg)
plot(fitted(model2eg), residuals(model2eg),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(model2eg), residuals(model2eg)))

library(lattice)
xyplot(residuals(model2eg) ~ fitted(model2eg) | data1$population, main = "model2eg – full model by plot",
       panel=function(x, y){ 
         panel.xyplot(x, y) 
         panel.loess(x, y, span = 0.75) 
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       } 
)

qqnorm(resid(model2eg))

#Goodness of fit
###1 - pchisq(summary(model.pois.2)$deviance,
###           summary(model.pois.2)$df.residual
###)

1 - pchisq(2017.4,295) #0 --> The Poisson model does not fit the data

model2eg.nb = glmer.nb(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                         z.n_fl_corrected+z.n_redants+z.avg_d_mean
                       +(1|population),data=data1comp,na.action="na.fail")
summary(model2eg.nb)

1 - pchisq(1386.6,294) #0 --> The negative binomial model does not fit the data


library(coefplot)
coefplot(model2eg)

#Model selection
models2eg<-dredge(model2eg)
subset(models2eg, delta <4) 
importance(models2eg) 

model2eg_avg<-model.avg(models2eg, subset=delta <4,revised.var = TRUE)
#Error in model.avg.model.selection(models2eg, subset = delta < 4, revised.var = TRUE) : 
# 'object' consists of only one model

r.squaredLR(model2eg_,null.RE=T) 

#Overdispersion in model2eg?

#Tried zero-inflated (not shown)
#But better to correct overdispersion with observation-level random effect

summary(model2eg)
library(RVAideMemoire)
overdisp.glmer(model2eg)

#Model with interactions of all variables with population
model2eg_a<-glmer(n_eggs_max~(z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                   z.n_fl_corrected+z.n_redants+z.avg_d_mean)*population
                 +(1|id),family="poisson",na.action = "na.fail",
                 control=glmerControl(optimizer="bobyqa"))
summary(model2eg_a)
#Model with significant interactions-->COULD KEEP,lower AIC
model2eg_b<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                 z.n_fl_corrected+z.n_redants+z.avg_d_mean+population+
                   population:z.diff_veg_h_mean_shoot_h+population:z.n_redants+population:z.avg_d_mean
                 +(1|id),family="poisson",na.action = "na.fail",
                 control=glmerControl(optimizer="bobyqa"))
summary(model2eg_b)
#Model without interactions and population as a random effect-->COULD KEEP
model2eg_c<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                   z.n_fl_corrected+z.n_redants+z.avg_d_mean
                 +(1|population)+(1|id),family="poisson",na.action = "na.fail")
summary(model2eg_c)
overdisp.glmer(model2eg_c)
r.squaredLR(model2eg_b,null.RE=T) 
r.squaredLR(model2eg_c,null.RE=T) 

AIC(model2eg_a,model2eg_b,model2eg_c)
AIC(model2eg_b,model2eg_c)

#Keep model2eg_b

#Model diagnostics
plot(model2eg_, add.smooth = T, which = 1)
hist(resid(model2eg_), xlab = "Residuals", main = "")
plot(z.avg_d_mean, resid(model2eg_), xlab = "Mean temperature",
     ylab = "Residuals")
plot(population, resid(model2eg_), xlab = "Population",
     ylab = "Residuals")
qqnorm(resid(model2eg_))
qqline(resid(model2eg_))

###Fit to each pop
model2eg_H<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected+z.n_redants+z.avg_d_mean+(1|id),
              data=subset(data1comp,population=="Högsjön"),family="poisson",na.action = "na.fail")
model2eg_R<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected+z.n_redants+z.avg_d_mean+(1|id),
              data=subset(data1comp,population=="Remmene"),family="poisson",na.action = "na.fail")
model2eg_T<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected+z.n_redants+z.avg_d_mean+(1|id),
              data=subset(data1comp,population!="Högsjön"&population!="Remmene"),family="poisson",
              na.action = "na.fail",control=glmerControl(optimizer="bobyqa"))
overdisp.glmer(model2eg_H)
overdisp.glmer(model2eg_R)
overdisp.glmer(model2eg_T)

summary(model2eg_)
summary(model2eg_H)
summary(model2eg_R)
summary(model2eg_T)

multiplot(model2eg_,model2eg_H,model2eg_R,model2eg_T,outerCI=0,horizontal=T)+
  geom_hline(linetype="dotted", yintercept=c(1.5,2.5,3.5,4.5,5.5,6.5))+ theme_bw()




#Some plots --> ugly, probably not using!

data1$color<-ifelse(data1$population=="Högsjön","red",ifelse(data1$population=="Remmene","green","blue"))
data1comp$color<-ifelse(data1comp$population=="Högsjön","red",ifelse(data1comp$population=="Remmene","green","blue"))

plot(n_eggs_max ~ z.avg_d_mean,data=data1comp,col=as.character(data1comp$color),pch=16,cex=0.8)
model<-glmer(n_eggs_max ~ z.avg_d_mean+(1|population)+(1|id), data = data1comp,family="poisson")
abline(summary(model)$coefficients[1],summary(model)$coefficients[2],
       col="gray50",lwd=2,lty="dashed")
#Slope (b) and intercept (a) from mixed effect models effect models

plot(n_eggs_max ~ z.most_adv,data=data1comp,col=as.character(data1comp$color),pch=16,cex=0.8)
model<-glmer(n_eggs_max ~ z.most_adv+(1|population)+(1|id), data = data1comp,family="poisson")
abline(summary(model)$coefficients[1],summary(model)$coefficients[2],
       col="gray50",lwd=2,lty="dashed")

plot(n_eggs_max ~ z.n_fl_corrected,data=data1comp,col=as.character(data1comp$color),pch=16,cex=0.8)
model<-glmer(n_eggs_max ~ z.n_fl_corrected+(1|population)+(1|id), data = data1comp,family="poisson")
abline(summary(model)$coefficients[1],summary(model)$coefficients[2],
       col="gray50",lwd=2,lty="dashed")

plot(n_eggs_max ~ z.n_redants,data=data1comp,col=as.character(data1comp$color),pch=16,cex=0.8)
model<-glmer(n_eggs_max ~ z.n_redants+(1|population)+(1|id), data = data1comp,family="poisson")
abline(summary(model)$coefficients[1],summary(model)$coefficients[2],
       col="gray50",lwd=2,lty="dashed")


#Variation partitioning --> using model with population=random
model2eg_c

#tr=traits
#ec=environmental context
#cc=community context

model2eg_c_tr_ec_cc<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.avg_d_mean+(1|population)+(1|id),family="poisson",na.action = "na.fail",
                           control=glmerControl(optimizer="bobyqa"))
model2eg_c_tr<-glmer(n_eggs_max~z.shoot_h+z.most_adv+z.n_fl_corrected+(1|population)+(1|id),family="poisson",na.action = "na.fail",
                     control=glmerControl(optimizer="bobyqa"))
model2eg_c_ec<-glmer(n_eggs_max~z.diff_veg_h_mean_shoot_h+z.avg_d_mean+(1|population)+(1|id),family="poisson",na.action = "na.fail",
                     control=glmerControl(optimizer="bobyqa"))
model2eg_c_cc<-glmer(n_eggs_max~z.n_redants+(1|population)+(1|id),family="poisson",na.action = "na.fail",
                     control=glmerControl(optimizer="bobyqa"))
model2eg_c_tr_ec<-glmer(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.avg_d_mean+(1|population)+(1|id),family="poisson",na.action = "na.fail",
                        control=glmerControl(optimizer="bobyqa"))
model2eg_c_tr_cc<-glmer(n_eggs_max~z.shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+(1|population)+(1|id),family="poisson",na.action = "na.fail",
                        control=glmerControl(optimizer="bobyqa"))
model2eg_c_ec_cc<-glmer(n_eggs_max~z.diff_veg_h_mean_shoot_h+z.n_redants+z.avg_d_mean+(1|population)+(1|id),family="poisson",na.action = "na.fail",
                        control=glmerControl(optimizer="bobyqa"))

R_tr_ec_cc<-r.squaredLR(model2eg_c_tr_ec_cc, null.RE = TRUE)
R_tr<-r.squaredLR(model2eg_c_tr, null.RE = TRUE)
R_ec<-r.squaredLR(model2eg_c_ec, null.RE = TRUE)
R_cc<-r.squaredLR(model2eg_c_cc, null.RE = TRUE)
R_tr_ec<-r.squaredLR(model2eg_c_tr_ec, null.RE = TRUE)
R_tr_cc<-r.squaredLR(model2eg_c_tr_cc, null.RE = TRUE)
R_ec_cc<-r.squaredLR(model2eg_c_ec_cc, null.RE = TRUE)

tr<-R_tr_ec_cc-R_ec_cc
ec<-R_tr_ec_cc-R_tr_cc
cc<-R_tr_ec_cc-R_tr_ec
tr_ec<-R_tr_ec_cc-R_cc-tr-ec
tr_cc<-R_tr_ec_cc-R_ec-tr-cc
ec_cc<-R_tr_ec_cc-R_tr-ec-cc
tr_ec_cc<-R_tr_ec_cc-tr-ec-cc-tr_ec-tr_cc-ec_cc

(tr/R_tr_ec_cc)*100
(ec/R_tr_ec_cc)*100
(cc/R_tr_ec_cc)*100
(tr_ec/R_tr_ec_cc)*100
(tr_cc/R_tr_ec_cc)*100
(ec_cc/R_tr_ec_cc)*100
(tr_ec_cc/R_tr_ec_cc)*100

tr+ec+cc+tr_ec+tr_cc+ec_cc+tr_ec_cc
