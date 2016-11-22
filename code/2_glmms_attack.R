#Construct models for interaction (intensity)
#Response=attack
data1comp<-data1[complete.cases(data1[37:54]),]

attach(data1comp)
library(lme4)
library(MuMIn)
model1<-glmer(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.phen_index+
                z.n_fl_corrected+z.n_redants+z.avg_d_mean
              +(1|population),family="binomial")
summary(model1)
r.squaredLR(model1,null.RE=T) 

#Model with interactions of all variables with population
model2a<-glm(attack~(z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
              z.n_fl_corrected+z.n_redants+z.avg_d_mean)*
              population,family="binomial",na.action = "na.fail")
summary(model2a)
#Model with the interaction which is close to significance
model2b<-glm(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
              z.n_fl_corrected+z.n_redants+z.avg_d_mean+
              population+population:z.diff_veg_h_mean_shoot_h,family="binomial",na.action = "na.fail")
summary(model2b)
#Model without interactions and population as a random effect-->KEEP
model2c<-glmer(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
              z.n_fl_corrected+z.n_redants+z.avg_d_mean
              +(1|population),family="binomial",na.action = "na.fail")
summary(model2c)
r.squaredLR(model2c,null.RE=T) 
AIC(model2a,model2b,model2c)

model3<-glmer(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.bud_h+
                z.n_fl_corrected+z.n_redants+z.dist_closest_redants+z.avg_d_mean
              +(1|population),family="binomial")
summary(model3)

model4<-glmer(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected+z.n_redants+z.avg_d_max
              +(1|population),family="binomial")
summary(model4)

AIC(model1,model2,model3,model4)

model5<-glmer(attack~z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected+z.avg_d_mean
              +(1|population),family="binomial",na.action = "na.fail")
summary(model5)
r.squaredLR(model5,null.RE=T) 
AIC(model1,model2,model3,model4,model5)

model2_bis<-glmer(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+redants_pres+z.avg_d_mean
                  +(1|population),family="binomial",na.action = "na.fail")

summary(model2_bis)
AIC(model2,model2_bis)

#Keep model2
r.squaredLR(model2,null.RE=T) 

#Model diagnostics
plot(model2c)
plot(fitted(model2c), residuals(model2c),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(model2c), residuals(model2c)))

library(lattice)
xyplot(residuals(model2c) ~ fitted(model2c) | data1$population, main = "model2c – full model by plot",
       panel=function(x, y){ 
         panel.xyplot(x, y) 
         panel.loess(x, y, span = 0.75) 
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       } 
)

plot(model2c, add.smooth = T, which = 1)
hist(resid(model2c), xlab = "Residuals", main = "")
plot(z.avg_d_mean, resid(model2c), xlab = "Mean temperature",
     ylab = "Residuals")
plot(population, resid(model2c), xlab = "Population",
     ylab = "Residuals")
qqnorm(resid(model2c))
qqline(resid(model2c))

#Model selection
models2c<-dredge(model2c)
subset(models2c, delta <4) 
importance(models2c) 

model2cavg<-model.avg(models2c, subset=delta <4,revised.var = TRUE)
summary(model2cavg)

###Fit to each pop
model2_H<-glm(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected+z.n_redants+z.avg_d_mean,
              data=subset(data1comp,population=="Högsjön"),family="binomial",na.action = "na.fail")
model2_R<-glm(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected+z.n_redants+z.avg_d_mean,
              data=subset(data1comp,population=="Remmene"),family="binomial",na.action = "na.fail")
model2_T<-glm(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected+z.n_redants+z.avg_d_mean,
              data=subset(data1comp,population!="Högsjön"&population!="Remmene"),family="binomial",na.action = "na.fail")
summary(model2)
summary(model2_H)
summary(model2_R)
summary(model2_T)

multiplot(model2,model2_H,model2_R,model2_T,outerCI=0,horizontal=T)+
  geom_hline(linetype="dotted", yintercept=c(1.5,2.5,3.5,4.5,5.5,6.5))+ theme_bw()

#Variation partitioning
model2c

#tr=traits
#ec=environmental context
#cc=community context

model2c_tr_ec_cc<-glmer(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.avg_d_mean+(1|population),family="binomial")
model2c_tr<-glmer(attack~z.shoot_h+z.most_adv+z.n_fl_corrected+(1|population),family="binomial")
model2c_ec<-glmer(attack~z.diff_veg_h_mean_shoot_h+z.avg_d_mean+(1|population),family="binomial")
model2c_cc<-glmer(attack~z.n_redants+(1|population),family="binomial")
model2c_tr_ec<-glmer(attack~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.avg_d_mean+(1|population),family="binomial")
model2c_tr_cc<-glmer(attack~z.shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+(1|population),family="binomial")
model2c_ec_cc<-glmer(attack~z.diff_veg_h_mean_shoot_h+z.n_redants+z.avg_d_mean+(1|population),family="binomial")

R_tr_ec_cc<-r.squaredLR(model2c_tr_ec_cc, null.RE = TRUE)
R_tr<-r.squaredLR(model2c_tr, null.RE = TRUE)
R_ec<-r.squaredLR(model2c_ec, null.RE = TRUE)
R_cc<-r.squaredLR(model2c_cc, null.RE = TRUE)
R_tr_ec<-r.squaredLR(model2c_tr_ec, null.RE = TRUE)
R_tr_cc<-r.squaredLR(model2c_tr_cc, null.RE = TRUE)
R_ec_cc<-r.squaredLR(model2c_ec_cc, null.RE = TRUE)

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

