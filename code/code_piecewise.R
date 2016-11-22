library(piecewiseSEM)
library(MASS)

datafile_piecewise<-read.table("datafile_piecewise.txt",header=T,sep="\t",dec=",")

fruits.modList<-list(
  glm(n_intact_fruits~fl_phenology+shoot_h+fl_number+soil_temp+n_eggs+population,family="poisson",na.action="na.fail",data=datafile_piecewise),
  glm.nb(n_eggs~fl_phenology+fl_number+shoot_h+n_myrmica+veg_height+soil_temp+population,na.action="na.fail",data=datafile_piecewise),
  glm.nb(n_myrmica~veg_height+population,na.action="na.fail",data=datafile_piecewise)
)

sem.fit(fruits.modList, datafile_piecewise,
        corr.errors = c("fl_phenology~~fl_number","shoot_h~~fl_number","fl_phenology~~shoot_h"))
sem.model.fits(fruits.modList)
sem.coefs(fruits.modList, datafile_piecewise,
          corr.errors = c("fl_phenology~~fl_number","shoot_h~~fl_number","fl_phenology~~shoot_h"))

