sem1<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer(attack~PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)

sem1_a<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer(attack~PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)

sem1_b<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer(attack~PC1+veg_h_mean+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+attack+(1|pop),data=data3,family="binomial")
)

sem1_c<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer(attack~PC1+veg_h_mean+n_redants+(1|pop),data=data3,family="binomial"),#Interactions trait*context NS
  glmer(cbind(n_intact_fruits,n_fl)~PC1+attack+(1|pop),data=data3,family="binomial")
)

sem.fit(sem1,data3) 
sem.fit(sem1_a,data3)
sem.fit(sem1_b,data3)
sem.fit(sem1_c,data3)

#Diff. in AICc < 2 units for all three models

sem2<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~PC1+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem2_a<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~PC1+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem2_b<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~PC1+veg_h_mean+n_redants+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~PC1+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem2_c<-list(
  lmer(PC1~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_redants~veg_h_mean+(1|pop),data=data3),
  glmer.nb(n_eggs_max~PC1+veg_h_mean+(1|pop),data=subset(data3,n_eggs_max>0),control=glmerControl(optimizer="Nelder_Mead")),
  glmer(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max+(1|pop),data=subset(data3,n_eggs_max>0),family="binomial")
)

sem.fit(sem2,data3) 
sem.fit(sem2_a,data3) 
sem.fit(sem2_b,data3) 
sem.fit(sem2_c,data3) 


