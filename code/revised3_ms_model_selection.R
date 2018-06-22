data3$zeros <- rep(0,nrow(data3))

sem1<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer(attack~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
) 

sem.fit(sem1,data3) #adding zeros same C, p and AIC
sem.coefs(sem1,data3) 

sem1_a<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer(attack~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~zeros+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)

sem1_b<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer(attack~zeros+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)

sem1_c<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer(attack~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+(1|pop),data=data3,family="binomial")
)

sem1_d<-list(
  lmer(PC1~zeros+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer(attack~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)

sem1_e<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer(attack~zeros+PC1+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)

sem1_f<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer(attack~zeros+PC1+veg_h_mean+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)

sem1_g<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+(1|pop),data=data3),
  glmer(attack~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)

sem1_h<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer(attack~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+attack+(1|pop),data=data3,family="binomial")
)

sem.fit(sem1_a,data3)
sem.fit(sem1_b,data3)
sem.fit(sem1_c,data3)
sem.fit(sem1_d,data3)
sem.fit(sem1_e,data3)
sem.fit(sem1_f,data3)
sem.fit(sem1_g,data3)
sem.fit(sem1_h,data3)

sem2<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem.fit(sem2,data3)
sem.coefs(sem2,data3)

sem2_a<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~zeros+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem2_b<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~zeros+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem2_c<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem2_d<-list(
  lmer(PC1~zeros+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem2_e<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~zeros+PC1+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem2_f<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~zeros+PC1+veg_h_mean+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem2_g<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+(1|pop),data=data3),
  glmer.nb(n_eggs_max~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem2_h<-list(
  lmer(PC1~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~zeros+veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~zeros+PC1+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~zeros+PC1+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem.fit(sem2_a,data3) 
sem.fit(sem2_b,data3) 
sem.fit(sem2_c,data3) 
sem.fit(sem2_d,data3) 
sem.fit(sem2_e,data3) 
sem.fit(sem2_f,data3) 
sem.fit(sem2_g,data3) 
sem.fit(sem2_h,data3) 




