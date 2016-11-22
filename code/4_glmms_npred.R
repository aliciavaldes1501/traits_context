#Response=n_predated

model1pred<-glmer(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.phen_index+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean
                  +(1|population),family="poisson",na.action = "na.fail")
summary(model1pred)
r.squaredLR(model1pred,null.RE=T) 

#Model with interactions of all variables with population
model2pred_a<-glm(n_predated~(z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean)*population
                  ,family="poisson",na.action = "na.fail")
summary(model2pred_a)
#Model with significant interactions-->Lowest AIC
model2pred_b<-glm(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean+population+population:z.avg_d_mean
                  ,family="poisson",na.action = "na.fail")
summary(model2pred_b)
r.squaredLR(model2pred_b) 

#Model without interactions and population as a random effect
model2pred_c<-glmer(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean
                  +(1|population),family="poisson",na.action = "na.fail")
summary(model2pred_c)
r.squaredLR(model2pred_c,null.RE=T) 
AIC(model2pred_a,model2pred_b,model2pred_c)
AIC(model2pred_b,model2pred_c)

models2pred_a<-dredge(model2pred_a)
subset(models2pred_a, delta <4) 
importance(models2pred_a) 

model2pred_a_avg<-model.avg(models2pred_a, subset=delta <4,revised.var = TRUE)
summary(model2pred_a_avg)


model3pred<-glmer(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.bud_h+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean
                  +(1|population),family="poisson",na.action = "na.fail")
summary(model3pred)

model4pred<-glmer(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_max
                  +(1|population),family="poisson",na.action = "na.fail")
summary(model4pred)

AIC(model1pred,model2pred,model3pred,model4pred)

#Keep model2pred

#Overdispersion in model2pred?
overdisp.glmer(model2pred)

#Adding an observation-level random effect
model2pred_<-glmer(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                     z.n_fl_corrected+z.n_redants+z.avg_d_mean
                   +(1|population)+(1|id),family="poisson",na.action = "na.fail")

summary(model2pred_)
r.squaredLR(model2pred_,null.RE=T) 
overdisp.glmer(model2pred_)

AIC(model2pred,model2pred_)
#Again,keep model2pred

#Model diagnostics
plot(model2pred)
plot(fitted(model2pred), residuals(model2pred),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(model2pred), residuals(model2pred)))

library(lattice)
xyplot(residuals(model2pred) ~ fitted(model2pred) | data1$population, main = "model2pred – full model by plot",
       panel=function(x, y){ 
         panel.xyplot(x, y) 
         panel.loess(x, y, span = 0.75) 
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       } 
)

plot(model2pred, add.smooth = T, which = 1)
hist(resid(model2pred), xlab = "Residuals", main = "")
plot(z.avg_d_mean, resid(model2pred), xlab = "Mean temperature",
     ylab = "Residuals")
plot(population, resid(model2pred), xlab = "Population",
     ylab = "Residuals")
qqnorm(resid(model2pred))
qqline(resid(model2pred))


###Fit to each pop
model2pred_H<-glm(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean,
                  data=subset(data1comp,population=="Högsjön"),family="poisson",na.action = "na.fail")
model2pred_R<-glm(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean,
                  data=subset(data1comp,population=="Remmene"),family="poisson",na.action = "na.fail")
model2pred_T<-glm(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                      z.n_fl_corrected+z.n_redants+z.avg_d_mean,
                    data=subset(data1comp,population!="Högsjön"&population!="Remmene"),family="poisson",
                    na.action = "na.fail")
model2pred_T<-glmer(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean+(1|id),
                  data=subset(data1comp,population!="Högsjön"&population!="Remmene"),family="poisson",
                  na.action = "na.fail",control=glmerControl(optimizer="bobyqa"))
overdisp.glmer(model2pred_H)
overdisp.glmer(model2pred_R)
overdisp.glmer(model2pred_T)

summary(model2pred)
summary(model2pred_H) #91.398/97<1 --> no overdispersion
summary(model2pred_R) #53.675/89<1 --> no overdisperion
summary(model2pred_T) #144.85/96=1.5 --> slight overdispersion

multiplot(model2pred,model2pred_H,model2pred_R,model2pred_T,outerCI=0,horizontal=T)+
  geom_hline(linetype="dotted", yintercept=c(1.5,2.5,3.5,4.5,5.5,6.5))+ theme_bw()

#Variation partitioning --> using model with population=random
model2pred_c
#tr=traits
#ec=environmental context
#cc=community context

model2pred_c_tr_ec_cc<-glmer(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.avg_d_mean+(1|population),family="poisson",na.action = "na.fail",
                           control=glmerControl(optimizer="bobyqa"))
model2pred_c_tr<-glmer(n_predated~z.shoot_h+z.most_adv+z.n_fl_corrected+(1|population),family="poisson",na.action = "na.fail",
                     control=glmerControl(optimizer="bobyqa"))
model2pred_c_ec<-glmer(n_predated~z.diff_veg_h_mean_shoot_h+z.avg_d_mean+(1|population),family="poisson",na.action = "na.fail",
                     control=glmerControl(optimizer="bobyqa"))
model2pred_c_cc<-glmer(n_predated~z.n_redants+(1|population),family="poisson",na.action = "na.fail",
                     control=glmerControl(optimizer="bobyqa"))
model2pred_c_tr_ec<-glmer(n_predated~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.avg_d_mean+(1|population),family="poisson",na.action = "na.fail",
                        control=glmerControl(optimizer="bobyqa"))
model2pred_c_tr_cc<-glmer(n_predated~z.shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+(1|population),family="poisson",na.action = "na.fail",
                        control=glmerControl(optimizer="bobyqa"))
model2pred_c_ec_cc<-glmer(n_predated~z.diff_veg_h_mean_shoot_h+z.n_redants+z.avg_d_mean+(1|population),family="poisson",na.action = "na.fail",
                        control=glmerControl(optimizer="bobyqa"))

R_tr_ec_cc<-r.squaredLR(model2pred_c_tr_ec_cc, null.RE = TRUE)
R_tr<-r.squaredLR(model2pred_c_tr, null.RE = TRUE)
R_ec<-r.squaredLR(model2pred_c_ec, null.RE = TRUE)
R_cc<-r.squaredLR(model2pred_c_cc, null.RE = TRUE)
R_tr_ec<-r.squaredLR(model2pred_c_tr_ec, null.RE = TRUE)
R_tr_cc<-r.squaredLR(model2pred_c_tr_cc, null.RE = TRUE)
R_ec_cc<-r.squaredLR(model2pred_c_ec_cc, null.RE = TRUE)

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

######################################################################################
#Same with prop_pred (binomial) --> more correct because n_predated depends
#on n_fl
data1$prop_pred<-cbind(data1$n_predated,data1$n_fl_corrected)
data1comp$prop_pred<-cbind(data1comp$n_predated,data1comp$n_fl_corrected)

#Model with interactions of all variables with population
model2pred_prop_a<-glmer(prop_pred~(z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                           z.n_fl_corrected+z.n_redants+z.avg_d_mean)*population
                         +(1|id),family="binomial",na.action = "na.fail")
summary(model2pred_prop_a)
#Model with (close to) significant interaction-->lowest AIC
model2pred_prop_b<-glmer(prop_pred~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                           z.n_fl_corrected+z.n_redants+z.avg_d_mean+population+population:z.avg_d_mean
                         +(1|id),family="binomial",na.action = "na.fail")
summary(model2pred_prop_b)
r.squaredLR(model2pred_prop_b,null.RE=T) 
#Model without interactions and population as a random effect
model2pred_prop_c<-glmer(prop_pred~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean
                  +(1|population)+(1|id),family="binomial",na.action = "na.fail")
summary(model2pred_prop_c)
r.squaredLR(model2pred_prop_c,null.RE=T) 

AIC(model2pred_prop_b,model2pred_prop_c)
AIC(model2pred_prop_a,model2pred_prop_b,model2pred_prop_c)

###Fit to each pop
model2pred_prop_H<-glm(prop_pred~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean,
                  data=subset(data1comp,population=="Högsjön"),family="binomial",na.action = "na.fail")
model2pred_prop_R<-glm(prop_pred~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean,
                  data=subset(data1comp,population=="Remmene"),family="binomial",na.action = "na.fail")
model2pred_prop_T<-glm(prop_pred~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean,
                  data=subset(data1comp,population!="Högsjön"&population!="Remmene"),family="binomial",
                  na.action = "na.fail")

summary(model2pred_prop_H)
summary(model2pred_prop_R)
summary(model2pred_prop_T)

#Variation partitioning --> using model with population=random
model2pred_prop_c
#tr=traits
#ec=environmental context
#cc=community context
model2pred_prop_c_tr_ec_cc<-glmer(prop_pred~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.avg_d_mean+(1|population)+(1|id),family="binomial",na.action = "na.fail")
model2pred_prop_c_tr<-glmer(prop_pred~z.shoot_h+z.most_adv+z.n_fl_corrected+(1|population)+(1|id),family="binomial",na.action = "na.fail")
model2pred_prop_c_ec<-glmer(prop_pred~z.diff_veg_h_mean_shoot_h+z.avg_d_mean+(1|population)+(1|id),family="binomial",na.action = "na.fail")
model2pred_prop_c_cc<-glmer(prop_pred~z.n_redants+(1|population)+(1|id),family="binomial",na.action = "na.fail")
model2pred_prop_c_tr_ec<-glmer(prop_pred~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.avg_d_mean+(1|population)+(1|id),family="binomial",na.action = "na.fail")
model2pred_prop_c_tr_cc<-glmer(prop_pred~z.shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+(1|population)+(1|id),family="binomial",na.action = "na.fail")
model2pred_prop_c_ec_cc<-glmer(prop_pred~z.diff_veg_h_mean_shoot_h+z.n_redants+z.avg_d_mean+(1|population)+(1|id),family="binomial",na.action = "na.fail")

R_tr_ec_cc<-r.squaredLR(model2pred_prop_c_tr_ec_cc, null.RE = TRUE)
R_tr<-r.squaredLR(model2pred_prop_c_tr, null.RE = TRUE)
R_ec<-r.squaredLR(model2pred_prop_c_ec, null.RE = TRUE)
R_cc<-r.squaredLR(model2pred_prop_c_cc, null.RE = TRUE)
R_tr_ec<-r.squaredLR(model2pred_prop_c_tr_ec, null.RE = TRUE)
R_tr_cc<-r.squaredLR(model2pred_prop_c_tr_cc, null.RE = TRUE)
R_ec_cc<-r.squaredLR(model2pred_prop_c_ec_cc, null.RE = TRUE)

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





