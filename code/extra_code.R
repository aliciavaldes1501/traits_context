#glmmADMB
library(glmmADMB)

model2eg.zi<-glmmadmb(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                        z.n_fl_corrected+z.n_redants+z.avg_d_mean,random=~1|population,
                      data=data1comp,family="poisson",zeroInflation=T)
model2eg.zi1<-glmmadmb(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                         z.n_fl_corrected+z.n_redants+z.avg_d_mean,random=~1|population,
                       data=data1comp,family="nbinom",zeroInflation=T)
model2eg.zi_f<-glmmadmb(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                          z.n_fl_corrected+z.n_redants+z.avg_d_mean,random=~1|population,
                        data=data1comp,family="poisson",zeroInflation=F)
model2eg.zi_f1<-glmmadmb(n_eggs_max~z.shoot_h+z.diff_veg_h_mean_shoot_h+z.most_adv+
                           z.n_fl_corrected+z.n_redants+z.avg_d_mean,random=~1|population,
                         data=data1comp,family="nbinom",zeroInflation=F)
summary(model2eg.zi)
summary(model2eg.zi1)
summary(model2eg.zi_f)
summary(model2eg.zi_f1)
AIC(model2eg,model2eg.zi,model2eg.zi1,model2eg.zi_f,model2eg.zi_f1)

#Keep model2eg.zi1
summary(model2eg.zi1)
plot(fitted(model2eg.zi1),residuals(model2eg.zi1))
plot.glmmADMB(model2eg.zi1) #ERROR!
qqnorm(resid(model2eg.zi1))
