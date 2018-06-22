library(lme4)
library(lmerTest)

#Single models####

summary(lmer(PC1~veg_h_mean+(1|pop),data=data3))
summary(lmer(PC2~veg_h_mean+(1|pop),data=data3))
summary(glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3))
summary(glmer(attack~PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"))
summary(glmer(attack~PC2+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"))
summary(glmer(attack~PC1+(1|pop),data=data3,family="binomial"))
summary(glmer(attack~PC2+(1|pop),data=data3,family="binomial"))
summary(glmer(attack~PC1+PC2+(1|pop),data=data3,family="binomial"))
summary(glmer(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+attack+(1|pop),data=data3,family="binomial"))
summary(glmer(cbind(n_intact_fruits,n_fl)~PC2+veg_h_mean+attack+(1|pop),data=data3,family="binomial"))


summary(glmer.nb(n_eggs_max~PC1+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")))
summary(glmer.nb(n_eggs_max~PC1+PC2+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")))
summary(glmer(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial"))

#SEM attack with pop as random####

sem1<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer(attack~PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)
sem.fit(sem1,data3) 
sem.coefs(sem1,data3)
sem.model.fits(sem1,data3)

#SEM n eggs with pop as random####

sem2<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~PC1+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)
sem.fit(sem2,data3) 
sem.coefs(sem2,data3)
sem.model.fits(sem2,data3)

#Use also PC2

data3$PC2<-PCA_traits_scores$PC2
hist(data3$PC2)

summary(lmer(PC2~veg_h_mean+(1|pop),data=data3))

#SEM attack with pop as random  and PC2####

sem3<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),  
  lmer(PC2~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer(attack~PC1+PC2+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~PC1+PC2+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)
sem.fit(sem3,data3) 
sem.coefs(sem3,data3)
sem.model.fits(sem3,data3)

#SEM n eggs with pop as random and PC2####

sem4<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  lmer(PC2~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~PC1+PC2+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~PC1+PC2+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)
sem.fit(sem4,data3) 
sem.coefs(sem4,data3)
sem.model.fits(sem4,data3)

#PC2 affected by vegetation height
#But no effects of PC2 on oviposition or fruit set

#SEM attack with pop as random and PC1_env####

sem5<-list(
  lmer(PC1~PC1_env+(1|pop),data=data3),
  glmer.nb(n_redants~PC1_env+(1|pop),data=data3),
  glmer(attack~PC1+PC1_env+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~PC1+PC1_env+attack+(1|pop),data=data3,family="binomial")
)
sem.fit(sem5,data3) 
sem.coefs(sem5,data3)
sem.model.fits(sem5,data3)

#SEM n eggs with pop as random and PC1_env####

sem6<-list(
  lmer(PC1~PC1_env+(1|pop),data=data3),
  glmer.nb(n_redants~PC1_env+(1|pop),data=data3),
  glmer.nb(n_eggs_max~PC1+PC1_env+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~PC1+PC1_env+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)
sem.fit(sem6,data3) 
sem.coefs(sem6,data3)
sem.model.fits(sem6,data3)

#No effects of PC1_env on attack, and positive effects on n eggs

#SEM attack with pop as random and meanT####

sem7<-list(
  lmer(PC1~meanT+(1|pop),data=data3),
  glmer.nb(n_redants~meanT+(1|pop),data=data3),
  glmer(attack~PC1+meanT+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~PC1+meanT+attack+(1|pop),data=data3,family="binomial")
)
sem.fit(sem7,data3) 
sem.coefs(sem7,data3)
sem.model.fits(sem7,data3)

#SEM n eggs with pop as random and meanT####

sem8<-list(
  lmer(PC1~meanT+(1|pop),data=data3),
  glmer.nb(n_redants~meanT+(1|pop),data=data3),
  glmer.nb(n_eggs_max~PC1+meanT+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~PC1+meanT+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)
sem.fit(sem8,data3) 
sem.coefs(sem8,data3)
sem.model.fits(sem8,data3)

#No effects of meanT on attack, and negative effects on n eggs

#SEM attack with pop as random and meanT+veg_h####

sem9<-list(
  lmer(PC1~meanT+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~meanT+veg_h_mean+(1|pop),data=data3),
  glmer(attack~PC1+meanT+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~PC1+meanT+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)
sem.fit(sem9,data3,corr.errors="meanT~~veg_h_mean") 
sem.coefs(sem9,data3,corr.errors="meanT~~veg_h_mean")
sem.model.fits(sem9,data3,corr.errors="meanT~~veg_h_mean")

#SEM n eggs with pop as random and meanT+veg_h####

sem10<-list(
  lmer(PC1~meanT+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~meanT+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~PC1+meanT+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~PC1+meanT+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)
sem.fit(sem10,data3,corr.errors="meanT~~veg_h_mean") 
sem.coefs(sem10,data3,corr.errors="meanT~~veg_h_mean")
sem.model.fits(sem10,data3,corr.errors="meanT~~veg_h_mean")

#












