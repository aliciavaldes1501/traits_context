library(MASS)
library(piecewiseSEM)
library(ggplot2)
library(ggfortify)
library(ggthemes)

#hurdle not allowed in piecewiseSEM
#first attack, then n eggs

data3$phen_index1_log<-log(data3$phen_index1)

#Correlation traits ####
cor.test(data3$phen_index1,data3$n_fl,na.rm=T)
cor.test(data3$phen_index1,data3$shoot_h,na.rm=T)
cor.test(data3$n_fl,data3$shoot_h,na.rm=T)

#PCA traits ####
PCA_traits<-prcomp(~data3$phen_index1+data3$shoot_h+data3$n_fl,center=T,scale=T)
summary(PCA_traits)
plot(PCA_traits)
biplot(PCA_traits)
PCA_traits_scores<-as.data.frame(predict(PCA_traits))
data3$PC1<-PCA_traits_scores$PC1
hist(data3$PC1)

##Graph PCA
theme_set( theme_base( base_family= "Times"))

pdf("./results/figures/PCA_traits.pdf", width=5,height=4)
autoplot(PCA_traits, loadings = TRUE, loadings.colour = "black", loadings.label = TRUE,loadings.label.colour="black",
  loadings.label.label = c("Shoot h","Phenology","N flowers"), colour="grey",data = data3)+
  xlab("PC1 (64.5%)")+ylab("PC2 (24.5%)")+geom_hline(aes(yintercept=0), colour="darkgrey",linetype="dashed")+
  geom_vline(aes(xintercept=0), colour="darkgrey",linetype="dashed")
dev.off()

#PCA env ####
PCA_env<-prcomp(~data3$meanT+data3$veg_h_mean,center=T,scale=T)
PCA_env_scores<-as.data.frame(predict(PCA_env))
data3$PC1_env<-PCA_env_scores$PC1
hist(data3$PC1_env)

#Single models ####
#Effect of context on traits
summary(lm(PC1~veg_h_mean,data=subset(data3,pop=="H")))
summary(lm(PC1~veg_h_mean,data=subset(data3,pop=="R")))
summary(lm(PC1~veg_h_mean,data=subset(data3,pop=="T")))

summary(lm(PC1~meanT,data=subset(data3,pop=="H")))
summary(lm(PC1~meanT,data=subset(data3,pop=="R")))
summary(lm(PC1~meanT,data=subset(data3,pop=="T")))

summary(lm(PC1~PC1_env,data=subset(data3,pop=="H")))
summary(lm(PC1~PC1_env,data=subset(data3,pop=="R")))
summary(lm(PC1~PC1_env,data=subset(data3,pop=="T")))

#Effect of context (veg) on context (ants)
summary(glm.nb(n_redants~veg_h_mean,data=subset(data3,pop=="H")))
summary(glm.nb(n_redants~veg_h_mean,data=subset(data3,pop=="R")))
summary(glm.nb(n_redants~veg_h_mean,data=subset(data3,pop=="T"))) #NS (pop with lowest veg)

summary(glm.nb(n_redants~meanT,data=subset(data3,pop=="H")))
summary(glm.nb(n_redants~meanT,data=subset(data3,pop=="R")))
summary(glm.nb(n_redants~meanT,data=subset(data3,pop=="T"))) 

summary(glm.nb(n_redants~PC1_env,data=subset(data3,pop=="H")))
summary(glm.nb(n_redants~PC1_env,data=subset(data3,pop=="R")))
summary(glm.nb(n_redants~PC1_env,data=subset(data3,pop=="T"))) #NS, p=0.06 (pop with lowest veg)

#Effects of traits and context on attack
summary(glm(attack~PC1+veg_h_mean+n_redants,data=subset(data3,pop=="H"),family="binomial"))
summary(glm(attack~PC1+veg_h_mean+n_redants,data=subset(data3,pop=="R"),family="binomial"))
summary(glm(attack~PC1+veg_h_mean+n_redants,data=subset(data3,pop=="T"),family="binomial")) #veg NS (pop with lowest veg)

summary(glm(attack~PC1+meanT+n_redants,data=subset(data3,pop=="H"),family="binomial"))
summary(glm(attack~PC1+meanT+n_redants,data=subset(data3,pop=="R"),family="binomial"))
summary(glm(attack~PC1+meanT+n_redants,data=subset(data3,pop=="T"),family="binomial")) #meanT NS

summary(glm(attack~PC1+PC1_env+n_redants,data=subset(data3,pop=="H"),family="binomial")) #PC1_env NS
summary(glm(attack~PC1+PC1_env+n_redants,data=subset(data3,pop=="R"),family="binomial")) 
summary(glm(attack~PC1+PC1_env+n_redants,data=subset(data3,pop=="T"),family="binomial")) #PC1_env NS (pop with lowest veg)

#Effects of traits, context and predation on fruit set
summary(glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+attack,
            family="binomial",data=subset(data3,pop=="H")))
summary(glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+attack,
            family="binomial",data=subset(data3,pop=="R"))) #NS (could include?)
summary(glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+attack,
            family="binomial",data=subset(data3,pop=="T")))

summary(glm(cbind(n_intact_fruits,n_fl)~PC1+PC1_env+attack,
            family="binomial",data=subset(data3,pop=="H")))
summary(glm(cbind(n_intact_fruits,n_fl)~PC1+PC1_env+attack,
            family="binomial",data=subset(data3,pop=="R"))) #NS (could include?)
summary(glm(cbind(n_intact_fruits,n_fl)~PC1+PC1_env+attack,
            family="binomial",data=subset(data3,pop=="T")))

#Effects of traits and context on n eggs
summary(glm.nb(n_eggs_max~PC1+veg_h_mean+n_redants,data=subset(data3,n_eggs_max>0&pop=="H"))) #Only PC1 *
summary(glm.nb(n_eggs_max~PC1+veg_h_mean+n_redants,data=subset(data3,n_eggs_max>0&pop=="R"))) #Only PC1 *
summary(glm.nb(n_eggs_max~PC1+veg_h_mean+n_redants,data=subset(data3,n_eggs_max>0&pop=="T"))) #Only PC1 *

summary(glm.nb(n_eggs_max~PC1+PC1_env+n_redants,data=subset(data3,n_eggs_max>0&pop=="H"))) #Only PC1 *
summary(glm.nb(n_eggs_max~PC1+PC1_env+n_redants,data=subset(data3,n_eggs_max>0&pop=="R"))) #Only PC1 *
summary(glm.nb(n_eggs_max~PC1+PC1_env+n_redants,data=subset(data3,n_eggs_max>0&pop=="T"))) #Only PC1 *

#Effects of traits, context and predation on fruit set
summary(glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max,
            family="binomial",data=subset(data3,n_eggs_max>0&pop=="H")))
summary(glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max,
            family="binomial",data=subset(data3,n_eggs_max>0&pop=="R"))) 
summary(glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max,
            family="binomial",data=subset(data3,n_eggs_max>0&pop=="T"))) #PC1 and n eggs *

summary(glm(cbind(n_intact_fruits,n_fl)~PC1+PC1_env+n_eggs_max,
            family="binomial",data=subset(data3,n_eggs_max>0&pop=="H")))
summary(glm(cbind(n_intact_fruits,n_fl)~PC1+PC1_env+n_eggs_max,
            family="binomial",data=subset(data3,n_eggs_max>0&pop=="R"))) 
summary(glm(cbind(n_intact_fruits,n_fl)~PC1+PC1_env+n_eggs_max,
            family="binomial",data=subset(data3,n_eggs_max>0&pop=="T"))) #PC1 and n eggs *

#SEMs attack ####

#By pop --> Only use these?
sem1_H<-list(
  lm(PC1~veg_h_mean,data=subset(data3,pop=="H")),
  glm.nb(n_redants~veg_h_mean,data=subset(data3,pop=="H")),
  glm(attack~PC1+veg_h_mean+n_redants,data=subset(data3,pop=="H"),family="binomial"),#Interactions trait*context NS
  glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+attack,family="binomial",data=subset(data3,pop=="H"))
)
sem.fit(sem1_H,subset(data3,pop=="H")) 
sem.coefs(sem1_H,subset(data3,pop=="H"))
sem.model.fits(sem1_H,subset(data3,pop=="H"))

sem1_R<-list(
  lm(PC1~veg_h_mean,data=subset(data3,pop=="R")),
  glm.nb(n_redants~veg_h_mean,data=subset(data3,pop=="R")),
  glm(attack~PC1+veg_h_mean+n_redants,data=subset(data3,pop=="R"),family="binomial"),#Interactions trait*context NS
  glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+attack,family="binomial",data=subset(data3,pop=="R"))
)
sem.fit(sem1_R,subset(data3,pop=="R")) 
sem.coefs(sem1_R,subset(data3,pop=="R")) #attack on fr set NS
sem.model.fits(sem1_R,subset(data3,pop=="R"))

sem1_T<-list(
  lm(PC1~veg_h_mean,data=subset(data3,pop=="T")),
  glm.nb(n_redants~veg_h_mean,data=subset(data3,pop=="T")),
  glm(attack~PC1+veg_h_mean+n_redants,data=subset(data3,pop=="T"),family="binomial"),#Interactions trait*context NS
  glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+attack,family="binomial",data=subset(data3,pop=="T"))
)
sem.fit(sem1_T,subset(data3,pop=="T")) 
sem.coefs(sem1_T,subset(data3,pop=="T")) #veg on ants and on attack NS
sem.model.fits(sem1_T,subset(data3,pop=="T"))

#SEMs n eggs ####

#By pop --> Only use these?
sem2_H<-list(
  lm(PC1~veg_h_mean,data=subset(data3,pop=="H")),
  glm.nb(n_redants~veg_h_mean,data=subset(data3,pop=="H")),
  glm.nb(n_eggs_max~PC1+veg_h_mean+n_redants,data=subset(data3,pop=="H"&n_eggs_max>0)),
  glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max,family="binomial",data=subset(data3,pop=="H"&n_eggs_max>0))
)
sem.fit(sem2_H,subset(data3,pop=="H")) 
sem.coefs(sem2_H,subset(data3,pop=="H"))
sem.model.fits(sem2_H,subset(data3,pop=="H"))

sem2_R<-list(
  lm(PC1~veg_h_mean,data=subset(data3,pop=="R")),
  glm.nb(n_redants~veg_h_mean,data=subset(data3,pop=="R")),
  glm.nb(n_eggs_max~PC1+veg_h_mean+n_redants,data=subset(data3,pop=="R"&n_eggs_max>0)),
  glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max,family="binomial",data=subset(data3,pop=="R"&n_eggs_max>0))
)
sem.fit(sem2_R,subset(data3,pop=="R")) 
sem.coefs(sem2_R,subset(data3,pop=="R"))
sem.model.fits(sem2_R,subset(data3,pop=="R"))

sem2_T<-list(
  lm(PC1~veg_h_mean,data=subset(data3,pop=="T")),
  glm.nb(n_redants~veg_h_mean,data=subset(data3,pop=="T")),
  glm.nb(n_eggs_max~PC1+veg_h_mean+n_redants,data=subset(data3,pop=="T"&n_eggs_max>0)),
  glm(cbind(n_intact_fruits,n_fl)~PC1+veg_h_mean+n_eggs_max,family="binomial",data=subset(data3,pop=="T"&n_eggs_max>0))
)
sem.fit(sem2_T,subset(data3,pop=="T")) 
sem.coefs(sem2_T,subset(data3,pop=="T"))
sem.model.fits(sem2_T,subset(data3,pop=="T"))
