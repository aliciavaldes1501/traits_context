#Construct models for fitness
#First measured as n_intact_fruits_max
#using attack

#Model with interactions of all variables with population
model1fr_a<-glm(n_intact_fruits_max~(z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                  z.n_fl_corrected+z.n_redants+z.avg_d_mean+attack)*population
                ,family="poisson",na.action = "na.fail")
summary(model1fr_a)
#Model with significant interactions-->Lowest AIC
model1fr_b<-glm(n_intact_fruits_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                  z.n_fl_corrected+z.n_redants+attack+population*z.avg_d_mean
                ,family="poisson",na.action = "na.fail")
summary(model1fr_b)
r.squaredLR(model1fr_b) 

#Model without interactions and population as a random effctt
model1fr_c<-glmer(n_intact_fruits_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                z.n_fl_corrected+z.n_redants+z.avg_d_mean+attack
              +(1|population),family="poisson",na.action = "na.fail")
overdisp.glmer(model1fr_c)
summary(model1fr_c)
r.squaredLR(model1fr_c,null.RE=T) 

AIC(model1fr_a,model1fr_b,model1fr_c)
AIC(model1fr_b,model1fr_c)

#Model diagnostics
plot(model1fr)
plot(fitted(model1fr), residuals(model1fr),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(model1fr), residuals(model1fr)))

library(lattice)
xyplot(residuals(model1fr) ~ fitted(model1fr) | data1$population, main = "model1fr – full model by plot",
       panel=function(x, y){ 
         panel.xyplot(x, y) 
         panel.loess(x, y, span = 0.75) 
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       } 
)

plot(model1fr, add.smooth = T, which = 1)
hist(resid(model1fr), xlab = "Residuals", main = "")
plot(z.avg_d_mean, resid(model1fr), xlab = "Mean temperature",
     ylab = "Residuals")
plot(population, resid(model1fr), xlab = "Population",
     ylab = "Residuals")
qqnorm(resid(model1fr))
qqline(resid(model1fr))

#Model selcttion
models1fr<-dredge(model1fr)
subset(models1fr, delta <4) 
importance(models1fr) 

model1fravg<-model.avg(models1fr, subset=delta <4,revised.var = TRUE)
summary(model1fravg)

#Variation partitioning --> using model with population=random
model1fr_c
#tr=traits
#ct=context (environmental + community)
#it=interaction
model1fr_c_tr_ct_it<-glmer(n_intact_fruits_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.avg_d_mean+attack+(1|population),family="poisson",na.action = "na.fail")
model1fr_c_tr<-glmer(n_intact_fruits_max~z.shoot_h+z.most_adv+z.n_fl_corrected+(1|population),family="poisson",na.action = "na.fail")
model1fr_c_ct<-glmer(n_intact_fruits_max~z.diff_veg_h_mean_shoot_h+z.avg_d_mean+z.n_redants+(1|population),family="poisson",na.action = "na.fail")
model1fr_c_it<-glmer(n_intact_fruits_max~attack+(1|population)+(1|id),family="poisson",na.action = "na.fail")
model1fr_c_tr_ct<-glmer(n_intact_fruits_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.avg_d_mean+z.n_redants+(1|population),family="poisson",na.action = "na.fail")
model1fr_c_tr_it<-glmer(n_intact_fruits_max~z.shoot_h+z.most_adv+z.n_fl_corrected+attack+(1|population),family="poisson",na.action = "na.fail")
model1fr_c_ct_it<-glmer(n_intact_fruits_max~z.diff_veg_h_mean_shoot_h+attack+z.avg_d_mean+z.n_redants+(1|population),family="poisson",na.action = "na.fail")

R_tr_ct_it<-r.squaredLR(model1fr_c_tr_ct_it, null.RE = TRUE)
R_tr<-r.squaredLR(model1fr_c_tr, null.RE = TRUE)
R_ct<-r.squaredLR(model1fr_c_ct, null.RE = TRUE)
R_it<-r.squaredLR(model1fr_c_it, null.RE = TRUE)
R_tr_ct<-r.squaredLR(model1fr_c_tr_ct, null.RE = TRUE)
R_tr_it<-r.squaredLR(model1fr_c_tr_it, null.RE = TRUE)
R_ct_it<-r.squaredLR(model1fr_c_ct_it, null.RE = TRUE)

tr<-R_tr_ct_it-R_ct_it
ct<-R_tr_ct_it-R_tr_it
it<-R_tr_ct_it-R_tr_ct
tr_ct<-R_tr_ct_it-R_it-tr-ct
tr_it<-R_tr_ct_it-R_ct-tr-it
ct_it<-R_tr_ct_it-R_tr-ct-it
tr_ct_it<-R_tr_ct_it-tr-ct-it-tr_ct-tr_it-ct_it

(tr/R_tr_ct_it)*100
(ct/R_tr_ct_it)*100
(it/R_tr_ct_it)*100
(tr_ct/R_tr_ct_it)*100
(tr_it/R_tr_ct_it)*100
(ct_it/R_tr_ct_it)*100
(tr_ct_it/R_tr_ct_it)*100

tr+ct+it+tr_ct+tr_it+ct_it+tr_ct_it


#Using n_eggs

#Model with interactions of all variables with population
model1fr_a<-glm(n_intact_fruits_max~(z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                                       z.n_fl_corrected+z.n_redants+z.avg_d_mean+z.n_eggs_max)*population
                ,family="poisson",na.action = "na.fail")
summary(model1fr_a)
#Model with significant interactions-->Lowest AIC
model1fr_b<-glm(n_intact_fruits_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                  z.n_fl_corrected+z.n_redants+z.n_eggs_max+population*z.avg_d_mean
                ,family="poisson",na.action = "na.fail")
summary(model1fr_b)
r.squaredLR(model1fr_b) 

#Model without interactions and population as a random effctt
model1fr_c<-glmer(n_intact_fruits_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                    z.n_fl_corrected+z.n_redants+z.avg_d_mean+z.n_eggs_max
                  +(1|population),family="poisson",na.action = "na.fail")
overdisp.glmer(model1fr_c)
summary(model1fr_c)
r.squaredLR(model1fr_c,null.RE=T) 

AIC(model1fr_a,model1fr_b,model1fr_c)
AIC(model1fr_b,model1fr_c)

#Variation partitioning --> using model with population=random
model1fr_c
#tr=traits
#ct=context (environmental + community)
#it=interaction
model1fr_c_tr_ct_it<-glmer(n_intact_fruits_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.avg_d_mean+z.n_eggs_max+(1|population),family="poisson",na.action = "na.fail")
model1fr_c_tr<-glmer(n_intact_fruits_max~z.shoot_h+z.most_adv+z.n_fl_corrected+(1|population),family="poisson",na.action = "na.fail")
model1fr_c_ct<-glmer(n_intact_fruits_max~z.diff_veg_h_mean_shoot_h+z.avg_d_mean+z.n_redants+(1|population),family="poisson",na.action = "na.fail")
model1fr_c_it<-glmer(n_intact_fruits_max~z.n_eggs_max+(1|population)+(1|id),family="poisson",na.action = "na.fail")
model1fr_c_tr_ct<-glmer(n_intact_fruits_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.avg_d_mean+z.n_redants+(1|population),family="poisson",na.action = "na.fail")
model1fr_c_tr_it<-glmer(n_intact_fruits_max~z.shoot_h+z.most_adv+z.n_fl_corrected+z.n_eggs_max+(1|population),family="poisson",na.action = "na.fail")
model1fr_c_ct_it<-glmer(n_intact_fruits_max~z.diff_veg_h_mean_shoot_h+z.n_eggs_max+z.avg_d_mean+z.n_redants+(1|population),family="poisson",na.action = "na.fail")

R_tr_ct_it<-r.squaredLR(model1fr_c_tr_ct_it, null.RE = TRUE)
R_tr<-r.squaredLR(model1fr_c_tr, null.RE = TRUE)
R_ct<-r.squaredLR(model1fr_c_ct, null.RE = TRUE)
R_it<-r.squaredLR(model1fr_c_it, null.RE = TRUE)
R_tr_ct<-r.squaredLR(model1fr_c_tr_ct, null.RE = TRUE)
R_tr_it<-r.squaredLR(model1fr_c_tr_it, null.RE = TRUE)
R_ct_it<-r.squaredLR(model1fr_c_ct_it, null.RE = TRUE)

tr<-R_tr_ct_it-R_ct_it
ct<-R_tr_ct_it-R_tr_it
it<-R_tr_ct_it-R_tr_ct
tr_ct<-R_tr_ct_it-R_it-tr-ct
tr_it<-R_tr_ct_it-R_ct-tr-it
ct_it<-R_tr_ct_it-R_tr-ct-it
tr_ct_it<-R_tr_ct_it-tr-ct-it-tr_ct-tr_it-ct_it

(tr/R_tr_ct_it)*100
(ct/R_tr_ct_it)*100
(it/R_tr_ct_it)*100
(tr_ct/R_tr_ct_it)*100
(tr_it/R_tr_ct_it)*100
(ct_it/R_tr_ct_it)*100
(tr_ct_it/R_tr_ct_it)*100

tr+ct+it+tr_ct+tr_it+ct_it+tr_ct_it


#fruit_set
data1$fruit_set1<-cbind(data1$n_fruits_total_max,data1$n_fl_corrected)
data1comp$fruit_set1<-cbind(data1comp$n_fruits_total_max,data1comp$n_fl_corrected)

#Model with interactions of all variables with population
model1frs_a<-glm(fruit_set1~(z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                     z.n_fl_corrected+z.n_redants+z.avg_d_mean+attack)*population
                   ,family="binomial",na.action = "na.fail")
summary(model1frs_a)
#Model with (nearly) significant interaction-->Lowest AIC
model1frs_b<-glm(fruit_set1~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                     z.n_fl_corrected+z.n_redants+z.avg_d_mean*population+attack
                   ,family="binomial",na.action = "na.fail")
summary(model1frs_b)
r.squaredLR(model1frs_b) 

#Model without interactions and population as a random effctt
model1frs_c<-glmer(fruit_set1~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                  z.n_fl_corrected+z.n_redants+z.avg_d_mean+attack
                +(1|population),family="binomial",na.action = "na.fail")
overdisp.glmer(model1frs_c)
summary(model1frs_c)
r.squaredLR(model1frs_c,null.RE=T) 

AIC(model1frs_a,model1frs_b,model1frs_c)
AIC(model1frs_b,model1frs_c)

#Model diagnostics
plot(model1frs)
plot(fitted(model1frs), residuals(model1frs),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(model1frs), residuals(model1frs)))

library(lattice)
xyplot(residuals(model1frs) ~ fitted(model1frs) | data1$population, main = "model1frs – full model by plot",
       panel=function(x, y){ 
         panel.xyplot(x, y) 
         panel.loess(x, y, span = 0.75) 
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       } 
)

plot(model1frs, add.smooth = T, which = 1)
hist(resid(model1frs), xlab = "Residuals", main = "")
plot(z.avg_d_mean, resid(model1frs), xlab = "Mean temperature",
     ylab = "Residuals")
plot(population, resid(model1frs), xlab = "Population",
     ylab = "Residuals")
qqnorm(resid(model1frs))
qqline(resid(model1frs))

#Model selcttion
models1frs<-dredge(model1frs)
subset(models1frs, delta <4) 
importance(models1frs) 

model1frsavg<-model.avg(models1frs, subset=delta <4,revised.var = TRUE)
summary(model1frsavg)

#Variation partitioning --> using model with population=random
model1fr_c
#tr=traits
#ct=context (environmental + community)
#it=interaction

model1frs_c_tr_ct_it<-glmer(fruit_set1~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.avg_d_mean+attack+(1|population),family="binomial",na.action = "na.fail")
model1frs_c_tr<-glmer(fruit_set1~z.shoot_h+z.most_adv+z.n_fl_corrected+(1|population),family="binomial",na.action = "na.fail")
model1frs_c_ct<-glmer(fruit_set1~z.diff_veg_h_mean_shoot_h+z.avg_d_mean+z.n_redants+(1|population),family="binomial",na.action = "na.fail")
model1frs_c_it<-glmer(fruit_set1~attack+(1|population)+(1|id),family="binomial",na.action = "na.fail")
model1frs_c_tr_ct<-glmer(fruit_set1~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.avg_d_mean+z.n_redants+(1|population),family="binomial",na.action = "na.fail")
model1frs_c_tr_it<-glmer(fruit_set1~z.shoot_h+z.most_adv+z.n_fl_corrected+attack+(1|population),family="binomial",na.action = "na.fail")
model1frs_c_ct_it<-glmer(fruit_set1~z.diff_veg_h_mean_shoot_h+attack+z.avg_d_mean+z.n_redants+(1|population),family="binomial",na.action = "na.fail")

R_tr_ct_it<-r.squaredLR(model1frs_c_tr_ct_it, null.RE = TRUE)
R_tr<-r.squaredLR(model1frs_c_tr, null.RE = TRUE)
R_ct<-r.squaredLR(model1frs_c_ct, null.RE = TRUE)
R_it<-r.squaredLR(model1frs_c_it, null.RE = TRUE)
R_tr_ct<-r.squaredLR(model1frs_c_tr_ct, null.RE = TRUE)
R_tr_it<-r.squaredLR(model1frs_c_tr_it, null.RE = TRUE)
R_ct_it<-r.squaredLR(model1frs_c_ct_it, null.RE = TRUE)

tr<-R_tr_ct_it-R_ct_it
ct<-R_tr_ct_it-R_tr_it
it<-R_tr_ct_it-R_tr_ct
tr_ct<-R_tr_ct_it-R_it-tr-ct
tr_it<-R_tr_ct_it-R_ct-tr-it
ct_it<-R_tr_ct_it-R_tr-ct-it
tr_ct_it<-R_tr_ct_it-tr-ct-it-tr_ct-tr_it-ct_it

(tr/R_tr_ct_it)*100
(ct/R_tr_ct_it)*100
(it/R_tr_ct_it)*100
(tr_ct/R_tr_ct_it)*100
(tr_it/R_tr_ct_it)*100
(ct_it/R_tr_ct_it)*100
(tr_ct_it/R_tr_ct_it)*100

tr+ct+it+tr_ct+tr_it+ct_it+tr_ct_it

#Using n_eggs

#Model with interactions of all variables with population
model1frs_a<-glm(fruit_set1~(z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                               z.n_fl_corrected+z.n_redants+z.avg_d_mean+z.n_eggs_max)*population
                 ,family="binomial",na.action = "na.fail")
summary(model1frs_a)
#Model with (nearly) significant interaction-->Lowest AIC
model1frs_b<-glm(fruit_set1~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                   z.n_fl_corrected+z.n_redants+z.avg_d_mean*population+z.n_eggs_max
                 ,family="binomial",na.action = "na.fail")
summary(model1frs_b)
r.squaredLR(model1frs_b) 

#Model without interactions and population as a random effctt
model1frs_c<-glmer(fruit_set1~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                     z.n_fl_corrected+z.n_redants+z.avg_d_mean+z.n_eggs_max
                   +(1|population),family="binomial",na.action = "na.fail")
overdisp.glmer(model1frs_c)
summary(model1frs_c)
r.squaredLR(model1frs_c,null.RE=T) 

AIC(model1frs_a,model1frs_b,model1frs_c)
AIC(model1frs_b,model1frs_c)

#Variation partitioning --> using model with population=random
model1fr_c
#tr=traits
#ct=context (environmental + community)
#it=interaction

model1frs_c_tr_ct_it<-glmer(fruit_set1~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.avg_d_mean+z.n_eggs_max+(1|population),family="binomial",na.action = "na.fail")
model1frs_c_tr<-glmer(fruit_set1~z.shoot_h+z.most_adv+z.n_fl_corrected+(1|population),family="binomial",na.action = "na.fail")
model1frs_c_ct<-glmer(fruit_set1~z.diff_veg_h_mean_shoot_h+z.avg_d_mean+z.n_redants+(1|population),family="binomial",na.action = "na.fail")
model1frs_c_it<-glmer(fruit_set1~z.n_eggs_max+(1|population)+(1|id),family="binomial",na.action = "na.fail")
model1frs_c_tr_ct<-glmer(fruit_set1~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+z.n_fl_corrected+z.avg_d_mean+z.n_redants+(1|population),family="binomial",na.action = "na.fail")
model1frs_c_tr_it<-glmer(fruit_set1~z.shoot_h+z.most_adv+z.n_fl_corrected+z.n_eggs_max+(1|population),family="binomial",na.action = "na.fail")
model1frs_c_ct_it<-glmer(fruit_set1~z.diff_veg_h_mean_shoot_h+z.n_eggs_max+z.avg_d_mean+z.n_redants+(1|population),family="binomial",na.action = "na.fail")

R_tr_ct_it<-r.squaredLR(model1frs_c_tr_ct_it, null.RE = TRUE)
R_tr<-r.squaredLR(model1frs_c_tr, null.RE = TRUE)
R_ct<-r.squaredLR(model1frs_c_ct, null.RE = TRUE)
R_it<-r.squaredLR(model1frs_c_it, null.RE = TRUE)
R_tr_ct<-r.squaredLR(model1frs_c_tr_ct, null.RE = TRUE)
R_tr_it<-r.squaredLR(model1frs_c_tr_it, null.RE = TRUE)
R_ct_it<-r.squaredLR(model1frs_c_ct_it, null.RE = TRUE)

tr<-R_tr_ct_it-R_ct_it
ct<-R_tr_ct_it-R_tr_it
it<-R_tr_ct_it-R_tr_ct
tr_ct<-R_tr_ct_it-R_it-tr-ct
tr_it<-R_tr_ct_it-R_ct-tr-it
ct_it<-R_tr_ct_it-R_tr-ct-it
tr_ct_it<-R_tr_ct_it-tr-ct-it-tr_ct-tr_it-ct_it

(tr/R_tr_ct_it)*100
(ct/R_tr_ct_it)*100
(it/R_tr_ct_it)*100
(tr_ct/R_tr_ct_it)*100
(tr_it/R_tr_ct_it)*100
(ct_it/R_tr_ct_it)*100
(tr_ct_it/R_tr_ct_it)*100

tr+ct+it+tr_ct+tr_it+ct_it+tr_ct_it





#####################################################################################################
#Selcttion analyses
#Fitness = n_intact_fruits_max
data1comp$mfitness_fruits<-data1comp$n_intact_fruits_max/mean(data1comp$n_intact_fruits_max)

#Model with interactions of all variables with population
model1fr_a<-lm(mfitness_fruits~(z.shoot_h+z.most_adv+z.n_fl_corrected+z.diff_veg_h_mean_shoot_h+
                 z.n_redants+z.avg_d_mean+attack)*population,na.action = "na.fail")
summary(model1fr_a)
#Model with (nearly) significant interaction-->Lowest AIC
model1fr_b<-lm(mfitness_fruits~z.shoot_h+z.most_adv+z.n_fl_corrected+z.diff_veg_h_mean_shoot_h+
                 z.n_redants+z.avg_d_mean+attack+population+population:z.n_fl_corrected+
                 population:z.avg_d_mean+population:attack,na.action = "na.fail")
summary(model1fr_b)
#Model without interactions and population as a random effctt
model1fr_c<-lmer(mfitness_fruits~z.shoot_h+z.most_adv+z.n_fl_corrected+z.diff_veg_h_mean_shoot_h+
               z.n_redants+z.avg_d_mean+attack+(1|population),na.action = "na.fail")
summary(model1fr_c)
r.squaredGLMM(model1fr_c) 

AIC(model1fr_a,model1fr_b,model1fr_c)



