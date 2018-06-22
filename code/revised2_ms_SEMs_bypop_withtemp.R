#SEMs attack ####

#By pop --> Only use these?
sem1_H<-list(
  lm(PC1~meanT,data=subset(data3,pop=="H")),
  glm.nb(n_redants~meanT,data=subset(data3,pop=="H")),
  glm(attack~PC1+meanT+n_redants,data=subset(data3,pop=="H"),family="binomial"),#Interactions trait*context NS
  glm(cbind(n_intact_fruits,n_fl)~PC1+meanT+attack,family="binomial",data=subset(data3,pop=="H"))
)
sem.fit(sem1_H,subset(data3,pop=="H")) 
sem.coefs(sem1_H,subset(data3,pop=="H"))
sem.model.fits(sem1_H,subset(data3,pop=="H"))

sem1_R<-list(
  lm(PC1~meanT,data=subset(data3,pop=="R")),
  glm.nb(n_redants~meanT,data=subset(data3,pop=="R")),
  glm(attack~PC1+meanT+n_redants,data=subset(data3,pop=="R"),family="binomial"),#Interactions trait*context NS
  glm(cbind(n_intact_fruits,n_fl)~PC1+meanT+attack,family="binomial",data=subset(data3,pop=="R"))
)
sem.fit(sem1_R,subset(data3,pop=="R")) 
sem.coefs(sem1_R,subset(data3,pop=="R")) #attack on fr set NS
sem.model.fits(sem1_R,subset(data3,pop=="R"))

sem1_T<-list(
  lm(PC1~meanT,data=subset(data3,pop=="T")),
  glm.nb(n_redants~meanT,data=subset(data3,pop=="T")),
  glm(attack~PC1+meanT+n_redants,data=subset(data3,pop=="T"),family="binomial"),#Interactions trait*context NS
  glm(cbind(n_intact_fruits,n_fl)~PC1+meanT+attack,family="binomial",data=subset(data3,pop=="T"))
)
sem.fit(sem1_T,subset(data3,pop=="T")) 
sem.coefs(sem1_T,subset(data3,pop=="T")) #veg on ants and on attack NS
sem.model.fits(sem1_T,subset(data3,pop=="T"))

#SEMs n eggs ####

#By pop --> Only use these?
sem2_H<-list(
  lm(PC1~meanT,data=subset(data3,pop=="H")),
  glm.nb(n_redants~meanT,data=subset(data3,pop=="H")),
  glm.nb(n_eggs_max~PC1+meanT+n_redants,data=subset(data3,pop=="H"&n_eggs_max>0)),
  glm(cbind(n_intact_fruits,n_fl)~PC1+meanT+n_eggs_max,family="binomial",data=subset(data3,pop=="H"&n_eggs_max>0))
)
sem.fit(sem2_H,subset(data3,pop=="H")) 
sem.coefs(sem2_H,subset(data3,pop=="H"))
sem.model.fits(sem2_H,subset(data3,pop=="H"))

sem2_R<-list(
  lm(PC1~meanT,data=subset(data3,pop=="R")),
  glm.nb(n_redants~meanT,data=subset(data3,pop=="R")),
  glm.nb(n_eggs_max~PC1+meanT+n_redants,data=subset(data3,pop=="R"&n_eggs_max>0)),
  glm(cbind(n_intact_fruits,n_fl)~PC1+meanT+n_eggs_max,family="binomial",data=subset(data3,pop=="R"&n_eggs_max>0))
)
sem.fit(sem2_R,subset(data3,pop=="R")) 
sem.coefs(sem2_R,subset(data3,pop=="R"))
sem.model.fits(sem2_R,subset(data3,pop=="R"))

sem2_T<-list(
  lm(PC1~meanT,data=subset(data3,pop=="T")),
  glm.nb(n_redants~meanT,data=subset(data3,pop=="T")),
  glm.nb(n_eggs_max~PC1+meanT+n_redants,data=subset(data3,pop=="T"&n_eggs_max>0)),
  glm(cbind(n_intact_fruits,n_fl)~PC1+meanT+n_eggs_max,family="binomial",data=subset(data3,pop=="T"&n_eggs_max>0))
)
sem.fit(sem2_T,subset(data3,pop=="T")) 
sem.coefs(sem2_T,subset(data3,pop=="T"))
sem.model.fits(sem2_T,subset(data3,pop=="T"))
