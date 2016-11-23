########################################################### Ants
sem1<-list(
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+veg_h_mean+pop,family="gaussian",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT",
"meanT:veg_h_mean~~veg_h_mean"))#p=0.297

sem1<-list(
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+pop,family="gaussian",data=data3) 
)

sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT",
"phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean"))#p=0.191

########################################################### Eggs
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

sem1<-list(
  glm.nb(n_eggs_max~phen_index+shoot_h+n_fl_corrected+veg_h_mean+n_redants+pop,data=data3),
  glm.nb(n_redants~meanT+veg_h_mean+meanT:veg_h_mean+pop,data=data3),
  glm(meanT~veg_h_mean+pop,family="gaussian",data=data3),
  glm(phen_index~meanT+pop,family="gaussian",data=data3) 
)
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","n_eggs_max~~meanT",
"phen_index~~shoot_h","phen_index~~n_fl_corrected","shoot_h~~n_fl_corrected",
"phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean"))#p=0.0
sem.fit(sem1,data3,corr.errors=c("meanT:veg_h_mean~~meanT","n_eggs_max~~meanT",
"phen_index~~shoot_h","phen_index~~n_fl_corrected","shoot_h~~n_fl_corrected",
"phen_index~~veg_h_mean","meanT:veg_h_mean~~veg_h_mean","meanT~~shoot_h",
"n_redants~~shoot_h","n_redants~~n_fl_corrected","n_redants~~phen_index"))#p=0.161

########################################################### Fruits
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


#Similar to old model
sem1<-list(
  glm(n_intact_fruits~phen_index+n_fl_corrected+shoot_h+meanT+n_eggs_max+pop,family="poisson",data=data3),
  glm.nb(n_eggs_max~phen_index+n_fl_corrected+shoot_h+veg_h_mean+n_redants+pop,data=data3),
  glm.nb(n_redants~meanT+veg_h_mean+pop,data=data3)  
)
sem.fit(sem1,data3,corr.errors=c("phen_index~~n_fl_corrected",
                                 "phen_index~~shoot_h",
                                 "shoot_h","n_fl_corrected"))
sem.coefs(sem1,data3)






