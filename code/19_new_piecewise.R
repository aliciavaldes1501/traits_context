#Construct piecewise SEMs
#Include effect of veg h on temp, of temp on phen
#and of veg h / phen (interaction?) on ants

#Models that worked before alone
summary(lm(meanT~veg_h_mean+pop,data=subset(data3))) 
summary(glm(phen_index~meanT+veg_h_mean+population,family="gaussian",data=data3))
summary(glm(phen_index~meanT+population,family="gaussian",data=data3))
#OR
summary(glm(phen_index~meanT+veg_h_mean+population,family="gaussian",data=data3))
summary(glm.nb(n_redants~veg_h_mean*meanT+population,na.action="na.fail",data=data3))
#Interaction veg h*meanT is significant

#Use most_adv for phen with poisson distr
summary(glm(most_adv~meanT+veg_h_mean+population,family="poisson",data=data3))
summary(glm(most_adv~meanT+population,family="poisson",data=data3))
plot(glm(most_adv~meanT+veg_h_mean+population,family="poisson",data=data3))
rsquared(glm(most_adv~meanT+veg_h_mean+population,family="poisson",data=data3))
#Better to use most_adv for phenology
#Fits more to a poisson distr than phen_index fits to a normal distr

#Build piecewiseSEM with these 3 models only
#phen_index
#1
sem1<-list(
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+veg_h_mean+pop,family="gaussian",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT",
        "meanT:veg_h_mean~~veg_h_mean"))#p=0.191
#As in Duffy et al. 2015: One path in the final model,connecting phen_index and veg_h_mean,
#was considered a correlated error rather than a directed causal path because the positive 
#coefficient seemed best interpreted as reflecting parallel responses to unmeasured forcing variables.
sem.model.fits(sem1,data3)
sem.coefs(sem1,data3) #Effect of temperature is * -- KEEP THIS MODEL!
lapply(sem1,plot)

#2
sem1<-list(
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+pop,family="gaussian",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT",
"phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean"))#p=0.191
#As in Duffy et al. 2015: One path in the final model,connecting phen_index and veg_h_mean,
#was considered a correlated error rather than a directed causal path because the positive 
#coefficient seemed best interpreted as reflecting parallel responses to unmeasured forcing variables.
sem.model.fits(sem1,data3)
sem.coefs(sem1,data3) #Effect of temperature is * -- KEEP THIS MODEL!
lapply(sem1,plot)

#most_adv
sem1<-list(
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(most_adv~meanT+pop,family="poisson",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT",
                                 "most_adv~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean"))#p=0.191
#As in Duffy et al. 2015: One path in the final model,connecting phen_index and veg_h_mean,
#was considered a correlated error rather than a directed causal path because the positive 
#coefficient seemed best interpreted as reflecting parallel responses to unmeasured forcing variables.
sem.model.fits(sem1,data3)
sem.coefs(sem1,data3) #Effect of temperature is not*
lapply(sem1,plot)

sem1<-list(
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(most_adv~meanT+veg_h_mean+pop,family="poisson",data=data3) #Why + effect of veg h on phen?
)

sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","meanT:veg_h_mean~~veg_h_mean"))#Best fit
sem.model.fits(sem1,data3)
sem.coefs(sem1,data3) #No effect of temp on phen! (marg) But how to justify effect of veg h?
##########################################################################################        

#Add model for eggs (only w phen by now)
#based on glm
summary(glm.nb(n_eggs_max~phen_index+veg_h_mean+n_redants+pop,data=data3))
summary(glm.nb(n_eggs_max~phen_index+veg_h_mean+n_redants+meanT+pop,data=data3))

plot(glm.nb(n_eggs_max~phen_index+veg_h_mean+n_redants+pop,data=data3))

sem1<-list(
  glm.nb(n_eggs_max~phen_index+veg_h_mean+n_redants+pop,data=data3),
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+pop,family="gaussian",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT",
                                 "phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean"))#p=0
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","phen_index~~veg_h_mean",
            "meanT:veg_h_mean~~veg_h_mean","meanT~~n_eggs_max"))#p=0.024
sem.model.fits(sem1,data3)
sem.coefs(sem1,data3) #No effect of ants and veg h on eggs
#Effect of ants appears if we include meanT in the model for eggs!
lapply(sem1,plot)

#Add also other traits
sem1<-list(
  glm.nb(n_eggs_max~phen_index+shoot_h+n_fl_corrected+veg_h_mean+n_redants+pop,data=data3),
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+pop,family="gaussian",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","n_eggs_max~~meanT",
                                 "phen_index~~shoot_h","phen_index~~n_fl_corrected","shoot_h~~n_fl_corrected",
                                 "phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean"))#p=0.0
sem.coefs(sem1,data3) #These are really good, but p=0 for the model :(
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","n_eggs_max~~meanT",
"phen_index~~shoot_h","phen_index~~n_fl_corrected","shoot_h~~n_fl_corrected",
"phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean","meanT~~shoot_h",
"n_redants~~shoot_h","n_redants~~n_fl_corrected","n_redants~~phen_index"))#p=0.161
sem.coefs(sem1,data3,corr.errors=c("n_eggs_max~~meanT",
                                 "phen_index~~shoot_h","phen_index~~n_fl_corrected","shoot_h~~n_fl_corrected",
                                 "phen_index~~veg_h_mean",
                                 "meanT~~shoot_h","n_redants~~shoot_h","n_redants~~n_fl_corrected","n_redants~~phen_index"))
#remove shoot-->NO
sem1<-list(
  glm.nb(n_eggs_max~phen_index+n_fl_corrected+veg_h_mean+n_redants+pop,data=data3),
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+pop,family="gaussian",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","n_eggs_max~~meanT",
        "phen_index~~n_fl_corrected","phen_index~~veg_h_mean",
        "meanT:veg_h_mean~~veg_h_mean"))#p=0.003
sem.model.fits(sem1,data3)
sem.coefs(sem1,data3) #No effect of ants and veg h on eggs
lapply(sem1,plot)

##########################################################################################        
#Add model for fruits
#based on glm
#only phen
summary(glm(n_intact_fruits~phen_index+shoot_h+n_fl_corrected+meanT+n_eggs_max+pop,family="poisson",data=data3))

sem1<-list(
  glm(n_intact_fruits~phen_index+meanT+n_eggs_max+pop,family="poisson",data=data3),
  glm.nb(n_eggs_max~phen_index+veg_h_mean+n_redants+pop,data=data3),
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+pop,family="gaussian",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","n_eggs_max~~meanT","phen_index~~n_redants",
                                 "phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean"))#p=0.055
sem.model.fits(sem1,data3)
sem.coefs(sem1,data3)#No effect of ants and veg h on eggs

#Add n fl
sem1<-list(
  glm(n_intact_fruits~phen_index+n_fl_corrected+meanT+n_eggs_max+pop,family="poisson",data=data3),
  glm.nb(n_eggs_max~phen_index+n_fl_corrected+veg_h_mean+n_redants+pop,data=data3),
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+pop,family="gaussian",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","n_eggs_max~~meanT","phen_index~~n_redants",
                                 "phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean",
                                 "phen_index~~n_fl_corrected"))#p=0.011
sem.model.fits(sem1,data3)
sem.coefs(sem1,data3)#Effect of ants on eggs*

#Add also shoot h-->MODEL WITH THE 3 TRAITS
sem1<-list(
  glm(n_intact_fruits~phen_index+n_fl_corrected+shoot_h+meanT+n_eggs_max+pop,family="poisson",data=data3),
  glm.nb(n_eggs_max~phen_index+n_fl_corrected+shoot_h+veg_h_mean+n_redants+pop,data=data3),
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+pop,family="gaussian",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","n_eggs_max~~meanT","phen_index~~n_redants",
                                 "phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean",
                                 "phen_index~~n_fl_corrected","shoot_h~~phen_index",
                                 "shoot_h~~n_fl_corrected"))#p=0
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","n_eggs_max~~meanT","phen_index~~n_redants",
                                 "phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean",
                                 "phen_index~~n_fl_corrected","shoot_h~~phen_index",
                                 "shoot_h~~n_fl_corrected","meanT~~shoot_h","n_redants~~n_fl_corrected",
                                 "n_redants~~shoot_h","n_redants~~n_intact_fruits"))#p=0.274
sem.model.fits(sem1,data3)
sem.coefs(sem1,data3,corr.errors=c("n_eggs_max~~meanT","phen_index~~n_redants","phen_index~~veg_h_mean",
                                   "phen_index~~n_fl_corrected","shoot_h~~phen_index","shoot_h~~n_fl_corrected",
                                   "meanT~~shoot_h","n_redants~~n_fl_corrected","n_redants~~shoot_h",
                                   "n_redants~~n_intact_fruits"))


