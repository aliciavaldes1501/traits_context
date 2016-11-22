library(piecewiseSEM)

#Model - FIGURE 1,model 1
#Create component models and store in a list
fruits.modList<-list(
  #Predicting number of intact fruits
  glm(n_intact_fruits~most_adv+shoot_h+n_fl_corrected+avg_d_min_ja+n_eggs_max+population,family="poisson",na.action="na.fail",data=data1comp),
  #Predicting number of eggs
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants+population,na.action="na.fail",data=data1comp),
  #Predicting number of ants
  glm.nb(n_redants~veg_h_mean+population,na.action="na.fail",data=data1comp)
  )

#Traits and context scaled --> "standardized" coefficients (use this in Fig. 1)
fruits.modList<-list(
  glm(n_intact_fruits~z.most_adv+z.shoot_h+z.n_fl_corrected+z.avg_d_min_ja+n_eggs_max+population,family="poisson",na.action="na.fail",data=data1comp),
  glm.nb(n_eggs_max~z.most_adv+z.n_fl_corrected+z.shoot_h+z.avg_d_min_ja+z.veg_h_mean+z.n_redants+population,na.action="na.fail",data=data1comp),
  glm(z.n_redants~z.veg_h_mean+population,family="gaussian",na.action="na.fail",data=data1comp)
)

summary(glm(n_intact_fruits~z.most_adv+z.shoot_h+z.n_fl_corrected+z.avg_d_min_ja+z.n_eggs_max+population,family="poisson",na.action="na.fail",data=data1comp))
summary(glm.nb(n_redants~z.veg_h_mean+population,na.action="na.fail",data=data1comp))

sem.fit(fruits.modList, data1comp,
        corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))
sem.model.fits(fruits.modList)
sem.coefs(fruits.modList, data1comp,
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#Same model but with scaled values and using lm
data1comp$z.n_intact_fruits<-scale(data1comp$n_intact_fruits, center = T, scale = T)
fruits.modList<-list(
  lm(z.n_intact_fruits~z.most_adv+z.shoot_h+z.n_fl_corrected+z.avg_d_min_ja+z.n_eggs_max+population,na.action="na.fail",data=data1comp),
  lm(z.n_eggs_max~z.most_adv+z.n_fl_corrected+z.shoot_h+z.avg_d_min_ja+z.veg_h_mean+z.n_redants+population,na.action="na.fail",data=data1comp),
  lm(z.n_redants~z.veg_h_mean+population,na.action="na.fail",data=data1comp)
)

sem.fit(fruits.modList, data1comp,
        corr.errors = c("z.most_adv~~z.n_fl_corrected","z.shoot_h~~z.n_fl_corrected","z.most_adv~~z.shoot_h"))
sem.model.fits(fruits.modList)
sem.coefs(fruits.modList, data1comp,
          corr.errors = c("z.most_adv~~z.n_fl_corrected","z.shoot_h~~z.n_fl_corrected","z.most_adv~~z.shoot_h"))

#Including interaction pop*veg on eggs
fruits.modList<-list(
  glm(n_intact_fruits~most_adv+shoot_h+n_fl_corrected+avg_d_min_ja+n_eggs_max+population,family="poisson",na.action="na.fail",data=data1comp),
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean*population+n_redants,na.action="na.fail",data=data1comp),
  glm.nb(n_redants~veg_h_mean+population,na.action="na.fail",data=data1comp)
)

sem.fit(fruits.modList, data1comp,
        corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))
sem.model.fits(fruits.modList)
sem.coefs(fruits.modList, data1comp,
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#Seeds, distribution=quasipoisson - FIGURE 1,model 2
seeds.modList<-list(
  glm(round(seed_n_per_shoot)~most_adv+shoot_h+n_fl_corrected+avg_d_min_ja+n_eggs_max+population+n_intact_fruits,family="quasipoisson",na.action="na.fail",data=data2compA),
  glm(n_intact_fruits~most_adv+shoot_h+n_fl_corrected+avg_d_min_ja+n_eggs_max+population,family="poisson",na.action="na.fail",data=data2compA),
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants+population,na.action="na.fail",data=data2compA),
  glm.nb(n_redants~veg_h_mean+population,na.action="na.fail",data=data2compA)
)

#Traits and context scaled --> "standardized" coefficients (use this in Fig. 1)
seeds.modList<-list(
  glm(round(seed_n_per_shoot)~z.most_adv+z.shoot_h+z.n_fl_corrected+z.avg_d_min_ja+n_eggs_max+population+n_intact_fruits,family="quasipoisson",na.action="na.fail",data=data2compA),
  glm(n_intact_fruits~z.most_adv+z.shoot_h+z.n_fl_corrected+z.avg_d_min_ja+n_eggs_max+population,family="poisson",na.action="na.fail",data=data2compA),
  glm.nb(n_eggs_max~z.most_adv+z.n_fl_corrected+z.shoot_h+z.avg_d_min_ja+z.veg_h_mean+z.n_redants+population,na.action="na.fail",data=data2compA),
  glm(z.n_redants~z.veg_h_mean+population,family="gaussian",na.action="na.fail",data=data2compA)
)

summary(glm(round(seed_n_per_shoot)~z.most_adv+z.shoot_h+z.n_fl_corrected+z.avg_d_min_ja+n_eggs_max+population+z.n_intact_fruits,family="quasipoisson",na.action="na.fail",data=data2compA))

sem.fit(seeds.modList, data2compA)
sem.model.fits(seeds.modList)
sem.coefs(seeds.modList, data2compA,
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#Same model but with scaled values and using lm
data2compA$z.seed_n_per_shoot<-scale(data2compA$seed_n_per_shoot, center = T, scale = T)
data2compA$z.n_intact_fruits<-scale(data2compA$n_intact_fruits, center = T, scale = T)

seeds.modList<-list(
  lm(z.seed_n_per_shoot~z.most_adv+z.shoot_h+z.n_fl_corrected+z.avg_d_min_ja+z.n_eggs_max+population+z.n_intact_fruits,na.action="na.fail",data=data2compA),
  lm(z.n_intact_fruits~z.most_adv+z.shoot_h+z.n_fl_corrected+z.avg_d_min_ja+z.n_eggs_max+population,na.action="na.fail",data=data2compA),
  lm(z.n_eggs_max~z.most_adv+z.n_fl_corrected+z.shoot_h+z.avg_d_min_ja+z.veg_h_mean+z.n_redants+population,na.action="na.fail",data=data2compA),
  lm(z.n_redants~z.veg_h_mean+population,na.action="na.fail",data=data2compA)
)

sem.fit(seeds.modList, data2compA,
        corr.errors = c("z.most_adv~~z.n_fl_corrected","z.shoot_h~~z.n_fl_corrected","z.most_adv~~z.shoot_h"))
sem.model.fits(seeds.modList)
sem.coefs(seeds.modList, data2compA,
          corr.errors = c("z.most_adv~~z.n_fl_corrected","z.shoot_h~~z.n_fl_corrected","z.most_adv~~z.shoot_h"))
partial.resid(z.seed_n_per_shoot~z.n_fl_corrected,seeds.modList,data2compA, return.data.frame = FALSE)
visreg(lm(seed_n_per_shoot~most_adv+shoot_h+n_fl_corrected+avg_d_min_ja+n_eggs_max+population+n_intact_fruits,na.action="na.fail",data=data2compA))

####################################################################################################
#Rethink part of the model context-eggs

#All possible relationships
eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants+population,na.action="na.fail",data=data1comp),
  glm(most_adv~avg_d_min_ja+population,na.action="na.fail",data=data1comp,family="poisson"),
  lm(avg_d_min_ja~veg_h_mean+population,na.action="na.fail",data=data1comp),
  glm.nb(n_redants~veg_h_mean+avg_d_min_ja+population,na.action="na.fail",data=data1comp)
)

sem.fit(eggs.modList, data1comp,
        corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))
sem.model.fits(eggs.modList)
sem.coefs(eggs.modList, data1comp,
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#Try removing non-significant paths one by one

#Removing path from temperature to phenology
eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants+population,na.action="na.fail",data=data1comp),
  lm(avg_d_min_ja~veg_h_mean+population,na.action="na.fail",data=data1comp),
  glm.nb(n_redants~veg_h_mean+avg_d_min_ja+population,na.action="na.fail",data=data1comp)
)

sem.fit(eggs.modList, data1comp,
        corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))
sem.model.fits(eggs.modList)
sem.coefs(eggs.modList, data1comp,
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#Removing path from temperature to ants
eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants+population,na.action="na.fail",data=data1comp),
  glm(most_adv~avg_d_min_ja+population,na.action="na.fail",data=data1comp,family="poisson"),
    lm(avg_d_min_ja~veg_h_mean+population,na.action="na.fail",data=data1comp),
  glm.nb(n_redants~veg_h_mean+population,na.action="na.fail",data=data1comp)
)

sem.fit(eggs.modList, data1comp,
        corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))
sem.model.fits(eggs.modList)
sem.coefs(eggs.modList, data1comp,
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#Removing both previous paths
eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants+population,na.action="na.fail",data=data1comp),
  lm(avg_d_min_ja~veg_h_mean+population,na.action="na.fail",data=data1comp),
  glm.nb(n_redants~veg_h_mean+population,na.action="na.fail",data=data1comp)
)

sem.fit(eggs.modList, data1comp,
        corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))
sem.model.fits(eggs.modList)
sem.coefs(eggs.modList, data1comp,
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#Removing both previous paths + path from vegetation height to temperature (marginally signif)
#As it is now in Fig. 1!

eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants+population,na.action="na.fail",data=data1comp),
  glm.nb(n_redants~veg_h_mean+population,na.action="na.fail",data=data1comp)
)

sem.fit(eggs.modList, data1comp,
        corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))
sem.model.fits(eggs.modList)
sem.coefs(eggs.modList, data1comp,
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#Check model with all possible relationships for each pop separately
#H
eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants,na.action="na.fail",data=subset(data1comp,population=="Högsjön")),
  glm(most_adv~avg_d_min_ja,na.action="na.fail",data=subset(data1comp,population=="Högsjön"),family="poisson"),
  lm(avg_d_min_ja~veg_h_mean,na.action="na.fail",data=subset(data1comp,population=="Högsjön")),
  glm.nb(n_redants~veg_h_mean+avg_d_min_ja,na.action="na.fail",data=subset(data1comp,population=="Högsjön"))
)

eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants,na.action="na.fail",data=subset(data1comp,population=="Högsjön")),
  lm(avg_d_min_ja~veg_h_mean,na.action="na.fail",data=subset(data1comp,population=="Högsjön")),
  glm.nb(n_redants~veg_h_mean+avg_d_min_ja,na.action="na.fail",data=subset(data1comp,population=="Högsjön"))
)

eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants,na.action="na.fail",data=subset(data1comp,population=="Högsjön")),
  lm(avg_d_min_ja~veg_h_mean,na.action="na.fail",data=subset(data1comp,population=="Högsjön")),
  glm.nb(n_redants~veg_h_mean,na.action="na.fail",data=subset(data1comp,population=="Högsjön"))
)

eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+shoot_h+avg_d_min_ja+veg_h_mean+n_redants,na.action="na.fail",data=subset(data1comp,population=="Högsjön")),
  lm(avg_d_min_ja~veg_h_mean,na.action="na.fail",data=subset(data1comp,population=="Högsjön")),
  glm.nb(n_redants~veg_h_mean,na.action="na.fail",data=subset(data1comp,population=="Högsjön"))
)

eggs.modList<-list(
  glm.nb(n_eggs_max~shoot_h+avg_d_min_ja+veg_h_mean+n_redants,na.action="na.fail",data=subset(data1comp,population=="Högsjön")),
  lm(avg_d_min_ja~veg_h_mean,na.action="na.fail",data=subset(data1comp,population=="Högsjön")),
  glm.nb(n_redants~veg_h_mean,na.action="na.fail",data=subset(data1comp,population=="Högsjön"))
)

sem.fit(eggs.modList, subset(data1comp,population=="Högsjön"),
        corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))
sem.model.fits(eggs.modList)
sem.coefs(eggs.modList, subset(data1comp,population=="Högsjön"),
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#R
eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants,na.action="na.fail",data=subset(data1comp,population=="Remmene")),
  glm(most_adv~avg_d_min_ja,na.action="na.fail",data=subset(data1comp,population=="Remmene"),family="poisson"),
  lm(avg_d_min_ja~veg_h_mean,na.action="na.fail",data=subset(data1comp,population=="Remmene")),
  glm.nb(n_redants~veg_h_mean+avg_d_min_ja,na.action="na.fail",data=subset(data1comp,population=="Remmene"))
)

sem.fit(eggs.modList, subset(data1comp,population=="Remmene"),
        corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))
sem.model.fits(eggs.modList)
sem.coefs(eggs.modList, subset(data1comp,population=="Remmene"),
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#T
eggs.modList<-list(
  glm.nb(n_eggs_max~most_adv+n_fl_corrected+shoot_h+avg_d_min_ja+veg_h_mean+n_redants,na.action="na.fail",data=subset(data1comp,population!="Högsjön"&population!="Remmene")),
  glm(most_adv~avg_d_min_ja,na.action="na.fail",data=subset(data1comp,population!="Högsjön"&population!="Remmene"),family="poisson"),
  lm(avg_d_min_ja~veg_h_mean,na.action="na.fail",data=subset(data1comp,population!="Högsjön"&population!="Remmene")),
  glm.nb(n_redants~veg_h_mean+avg_d_min_ja,na.action="na.fail",data=subset(data1comp,population!="Högsjön"&population!="Remmene"))
)

sem.fit(eggs.modList, subset(data1comp,population!="Högsjön"&population!="Remmene"),
        corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))
sem.model.fits(eggs.modList)
sem.coefs(eggs.modList, subset(data1comp,population!="Högsjön"&population!="Remmene"),
          corr.errors = c("most_adv~~n_fl_corrected","shoot_h~~n_fl_corrected","most_adv~~shoot_h"))

#Test relationships phenology, temp, veg height and interactions with pop

with(data1comp,summary(glm(most_adv~avg_d_min_ja,family="poisson")))
with(data1comp,summary(glm(most_adv~avg_d_min_ja*population,family="poisson")))
with(data1comp,summary(glm(most_adv~avg_d_min_ja+population,family="poisson")))

with(subset(data1comp,population=="Högsjön"),summary(glm(most_adv~avg_d_min_ja,family="poisson")))
with(subset(data1comp,population=="Remmene"),summary(glm(most_adv~avg_d_min_ja,family="poisson")))
with(subset(data1comp,population!="Högsjön"&population!="Remmene"),summary(glm(most_adv~avg_d_min_ja,family="poisson")))

with(data1comp,summary(lm(avg_d_min_ja~veg_h_mean)))
with(data1comp,summary(lm(avg_d_min_ja~veg_h_mean*population)))
with(data1comp,summary(lm(avg_d_min_ja~veg_h_mean+population)))

with(subset(data1comp,population=="Högsjön"),summary(lm(avg_d_min_ja~veg_h_mean,family="poisson")))
with(subset(data1comp,population=="Remmene"),summary(lm(avg_d_min_ja~veg_h_mean,family="poisson")))
with(subset(data1comp,population!="Högsjön"&population!="Remmene"),summary(lm(avg_d_min_ja~veg_h_mean,family="poisson")))

with(data1comp,summary(glm.nb(n_redants~veg_h_mean+avg_d_min_ja)))
with(data1comp,summary(glm.nb(n_redants~(veg_h_mean+avg_d_min_ja)*population)))
with(data1comp,summary(glm.nb(n_redants~veg_h_mean+avg_d_min_ja+population)))

with(data1comp,summary(glm.nb(n_redants~veg_h_mean)))
with(data1comp,summary(glm.nb(n_redants~veg_h_mean*population)))
with(data1comp,summary(glm.nb(n_redants~veg_h_mean+population)))

with(data1comp,summary(glm.nb(n_redants~avg_d_min_ja)))
with(data1comp,summary(glm.nb(n_redants~avg_d_min_ja*population)))
with(data1comp,summary(glm.nb(n_redants~avg_d_min_ja+population)))



