library(ggplot2)
library(MASS)
library(MuMIn)

#Rebuild data file with new temperature data (only based on day temperatures)
#and leaving only needed variables
names(data1comp)
data3<-data1comp[c(1:2,4,7,17:18,23,28:30)]
head(data3)
str(data3)

data3<-merge(data3,loggers_day_agg,by="id_pl")
write.table(data3,file="data3.txt",sep="\t",dec=".",col.names=T) #MODIFY NAME / LOCATION!

#Relation among temperature and vegetation height (hypothesis ok)
with(data3,hist(meanT)) #Quite normally distributed

summary(lm(meanT~veg_h_mean*pop,data=subset(data3,veg_h_mean<90))) 
summary(lm(maxT~veg_h_mean*pop,data=subset(data3,veg_h_mean<90))) 
summary(lm(minT~veg_h_mean*pop,data=subset(data3,veg_h_mean<90))) 
summary(lm(sdT~veg_h_mean*pop,data=subset(data3,veg_h_mean<90))) 
summary(lm(rangeT~veg_h_mean*pop,data=subset(data3,veg_h_mean<90)))
#When removing two outliers where veg_h_mean>90 in Remmene, no interaction veg*pop
#with any of the temp measures! 
#Always negative relationship (higher veg = lower and less variable temp)
p1<-ggplot(subset(data3,veg_h_mean<90), aes(veg_h_mean,meanT, colour = population)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+
  theme(legend.position="none")
p2<-ggplot(subset(data3,veg_h_mean<90), aes(veg_h_mean,maxT, colour = population)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+
  theme(legend.position="none")
p3<-ggplot(subset(data3,veg_h_mean<90), aes(veg_h_mean,minT, colour = population)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+ 
  theme(legend.position="none")
p4<-ggplot(subset(data3,veg_h_mean<90), aes(veg_h_mean,sdT, colour = population)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+
  theme(legend.position="none")
p5<-ggplot(subset(data3,veg_h_mean<90), aes(veg_h_mean,rangeT, colour = population)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+
  theme(legend.position="none")
grid.arrange(p1, p2, p3,p4,p5, ncol = 3)
#Example separated by pop with meanT
ggplot(subset(data3,veg_h_mean<90), aes(veg_h_mean,meanT)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+facet_grid(.~population)+
  theme(legend.position="none")

#Remove the two observations with veg_h_mean>90 from the dataset
data3<-subset(data3,veg_h_mean<90)
with(data3,table(population))

#Fit model with meanT for each population - significant for all!
summary(lm(meanT~veg_h_mean,data=subset(data3,pop=="H")))
summary(lm(meanT~veg_h_mean,data=subset(data3,pop=="R")))
summary(lm(meanT~veg_h_mean,data=subset(data3,pop=="T")))

#Relation among phenology and temperature (/Veg h) (hypothesis ok)
#Using phen_index instead of most_adv, cause easier to model
with(data3,hist(phen_index)) #Very skewed
with(data3,hist(log(phen_index))) #Does not make it more normal

summary(glm(phen_index~meanT+veg_h_mean+population,family="gaussian",data=data3)) #Both temp and veg h! But no int
#Including interaction of veg h*shoot h - phenology of higher shoots might be less affected by veg h
summary(glm(phen_index~scale(meanT)+scale(veg_h_mean)*scale(shoot_h)+population,family="gaussian",data=data3)) #Both temp and veg h! But no int
summary(lmer(phen_index~meanT+veg_h_mean*shoot_h+(1|population),data=data3)) #Both temp and veg h! But no int

ggplot(data3, aes(x = veg_h_mean, y = phen_index, color = shoot_h)) + geom_point(size = 2) +
scale_color_gradient(low = "red", high = "green") + 
geom_abline(intercept = -2.8356831, slope = 0.0337129, color = "red", size = 2) + 
geom_abline(intercept = -2.8356831, slope = -0.0011079*15, color = "green", size = 2) + 
geom_abline(intercept = -2.8356831, slope = -0.0011079*35, color = "brown", size = 2) + 
ylim(c(-3, 5)) #Does not work
                                                                                                                                                                                                                                                                                                       x = 9, y = 160, color = "green", angle = 30, size = 8) + ylim(c(-20, 250))
summary(glm(most_adv~meanT+veg_h_mean+population,family="poisson",data=data3)) #Both temp and veg h! But no int
plot(glm(most_adv~meanT+veg_h_mean*shoot_h+population,family="poisson",data=data3)) #Both temp and veg h! But no int
plot(glm(phen_index~meanT+veg_h_mean*shoot_h+population,family="gaussian",data=data3)) #Both temp and veg h! But no int


model1<-glm(phen_index~meanT+veg_h_mean*shoot_h+population,family="gaussian",data=data3, na.action = "na.fail")
tic()
models1 <- dredge(model1)
toc()

summary(model.avg(models1, subset = delta < 2))
models1[1]

summary(glm(phen_index~maxT*population,family="gaussian",data=data3))
summary(glm(phen_index~minT*population,family="gaussian",data=data3)) #Interaction temp*pop
summary(glm(phen_index~sdT*population,family="gaussian",data=data3)) #temp NS
summary(glm(phen_index~rangeT*population,family="gaussian",data=data3)) #temp NS

summary(glm(phen_index~meanT*population,family="quasipoisson",data=data3))

summary(glm(phen_index~meanT+veg_h_mean+
              population:veg_h_mean+
              population,family="gaussian",data=data3))


#Interaction not signif
plot(glm(phen_index~meanT*population,family="gaussian",data=data3))
plot(glm(phen_index~meanT*population,family="quasipoisson",data=data3))

#Not awful??

summary(glm(phen_index~meanT+population,family="gaussian",data=data3))

#Fit model with meanT for each population
summary(lm(phen_index~meanT,data=subset(data3,pop=="H"))) #*
summary(lm(phen_index~meanT,data=subset(data3,pop=="R"))) #NS
summary(lm(phen_index~meanT,data=subset(data3,pop=="T"))) #marg

ggplot(data3, aes(meanT,phen_index, colour = population)) +
  geom_smooth(method = "glm", se = T, method.args = list(family = "quasipoisson"),fullrange=T)+
  geom_point(size = 1)+facet_grid(.~population)+
  theme(legend.position="none") #Very similar to linear

ggplot(data3,aes(meanT,phen_index,colour=pop))+
  geom_smooth(method="lm",se=T,fullrange=T)+
  geom_point(size=1)+facet_grid(.~pop)+theme(legend.position="none")

p1<-ggplot(data3, aes(meanT,phen_index, colour = population)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+facet_grid(.~population)+
  theme(legend.position="none")
p2<-ggplot(data3, aes(maxT,phen_index, colour = population)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+facet_grid(.~population)+
  theme(legend.position="none")
p3<-ggplot(data3, aes(minT,phen_index, colour = population)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)+facet_grid(.~population)+
  theme(legend.position="none")
grid.arrange(p1, p2, p3, ncol = 1)

ggplot(data3, aes(veg_h_mean,phen_index, colour = pop)) +
  geom_smooth(method = "lm",  se = T,fullrange=T)+
  geom_point(size = 1)

ggplot(data1comp,aes(avg_day_mean_ja,a))+geom_smooth(method="lm",se=T,fullrange=T)+
  geom_point()+facet_grid(.~population)
ggplot(data1comp,aes(avg_day_mean_ja,c))+geom_smooth(method="lm",se=T,fullrange=T)+
  geom_point()+facet_grid(.~population)
ggplot(data1comp,aes(population,c))+geom_boxplot()
ggplot(data3,aes(pop,n_redants))+geom_boxplot()

ggplot(data1comp,aes(a))+geom_histogram()+facet_grid(.~population)
ggplot(data1comp,aes(b))+geom_histogram()+facet_grid(.~population)
ggplot(data1comp,aes(c))+geom_histogram()+facet_grid(.~population)
ggplot(data1comp,aes(d))+geom_histogram()+facet_grid(.~population)
ggplot(data1comp,aes(e))+geom_histogram()+facet_grid(.~population)
ggplot(data1comp,aes(f))+geom_histogram()+facet_grid(.~population)
ggplot(data1comp,aes(most_adv))+geom_histogram()+facet_grid(.~population)
ggplot(data1comp,aes(phen_index))+geom_histogram()+facet_grid(.~population)

#Ants - related to temperature and / or vegetation height?
with(data3,hist(n_redants))
with(data3,hist(log(n_redants)))
with(data3,hist(log(n_redants+1))) 
with(data3,hist(sqrt(n_redants)))


summary(glm.nb(n_redants~scale(veg_h_mean)*scale(meanT)+population,na.action="na.fail",data=data3))
summary(glm.nb(n_redants~veg_h_mean+population,na.action="na.fail",data=data3))
summary(glm.nb(n_redants~meanT+population,na.action="na.fail",data=data3))#Stronger coef
summary(glm.nb(n_redants~meanT,na.action="na.fail",data=data3)) #Effect changes sign if not including pop

summary(glm.nb(n_redants~veg_h_mean*meanT+population,na.action="na.fail",data=data3))
summary(lm(log(n_redants+1)~meanT+population,na.action="na.fail",data=data3))
#No int when using log+1 and lm, only effect of temp is *
plot(lm(log(n_redants+1)~meanT+population,na.action="na.fail",data=data3)) #Fit acceptable

#Interactions veg_h_mean*pop and meanT*pop are not significant

ggplot(data3, aes(veg_h_mean,n_redants)) +
  geom_smooth(method = MASS::glm.nb,  se = T)+
  geom_point(size = 1)+facet_grid(.~population)+ 
  theme(legend.position="none")
ggplot(data3, aes(meanT,n_redants)) +
  geom_smooth(method = MASS::glm.nb,  se = T)+
  geom_point(size = 1)+facet_grid(.~population)+ 
  theme(legend.position="none")

ggplot(data3, aes(veg_h_mean,log(n_redants+1))) +
  geom_smooth(method = lm)+
  geom_point(size = 1)+facet_grid(.~population)+ 
  theme(legend.position="none")
ggplot(data3, aes(meanT,log(n_redants+1))) +
  geom_smooth(method = lm)+
  geom_point(size = 1)+facet_grid(.~population)+ 
  theme(legend.position="none")

ggplot(data3, aes(veg_h_mean,n_redants)) +
  geom_smooth(method = MASS::glm.nb,  se = T,fullrange=T)+
  geom_point(size = 1) 
ggplot(data3, aes(meanT,n_redants)) +
  geom_smooth(method = MASS::glm.nb,  se = T,fullrange=T)+
  geom_point(size = 1) 

summary(glm.nb(n_redants~meanT,data=subset(data3,pop=="H"))) #NS
summary(glm.nb(n_redants~meanT,data=subset(data3,pop=="R"))) #*
summary(glm.nb(n_redants~meanT,data=subset(data3,pop=="T"))) #*

data3$redants_pres<-as.factor(with(data3,ifelse(n_redants>0,"1","0")))
with(data3,plot(redants_pres,n_eggs_max))
ggplot(data3, aes(redants_pres,n_eggs_max)) +geom_boxplot+facet_grid(.~population)+ 
  theme(legend.position="none")#Error
summary(glm.nb(n_eggs_max~redants_pres+population,na.action="na.fail",data=data3))

#Relationships n_eggs ~ predictors
ggplot(data3, aes(phen_index, n_eggs_max)) +
  geom_smooth(method = MASS::glm.nb,  se = T,colour="black")+
  geom_point(size = 1)+facet_grid(.~population)+ 
  theme(legend.position="none")+theme_bw()
ggplot(data3, aes(n_redants, n_eggs_max)) +
  geom_smooth(method = MASS::glm.nb,  se = T,colour="black")+
  geom_point(size = 1)+facet_grid(.~population)+ 
  theme(legend.position="none")+theme_bw()
ggplot(data3, aes(veg_h_mean, n_eggs_max)) +
  geom_smooth(method = MASS::glm.nb,  se = T,colour="black")+
  geom_point(size = 1)+facet_grid(.~population)+ 
  theme(legend.position="none")+theme_bw()
ggplot(data3, aes(meanT, n_eggs_max)) +
  geom_smooth(method = MASS::glm.nb,  se = T,colour="black")+
  geom_point(size = 1)+facet_grid(.~population)+ 
  theme(legend.position="none")+theme_bw()

#Some model selection with n_eggs
##########################################################################################        
summary(glm.nb(n_eggs_max~phen_index+n_fl_corrected+shoot_h+veg_h_mean+n_redants+pop,
               data=data3))
#Without temp
modeleg_a1_nb<-glm.nb(n_eggs_max~(scale(phen_index)+scale(n_fl_corrected)+scale(shoot_h)+
                                    scale(veg_h_mean)+scale(n_redants))*population,data=data3, na.action = "na.fail")
summary(modeleg_a1_nb)

tic()
modelseg_a1_nb <- dredge(modeleg_a1_nb)
toc()

summary(model.avg(modelseg_a1_nb, subset = delta < 2)) #some high coef not* (collinearity? - use pca?)
modelseg_a1_nb[1]

#With temp
modeleg_a2_nb<-glm.nb(n_eggs_max~(scale(phen_index)+scale(n_fl_corrected)+scale(shoot_h)+
                                    scale(veg_h_mean)+scale(n_redants)+scale(meanT))*population,data=data3, na.action = "na.fail")
summary(modeleg_a2_nb)

tic()
modelseg_a2_nb <- dredge(modeleg_a2_nb)
toc()

summary(model.avg(modelseg_a2_nb, subset = delta < 2))


#Interaction phen*ants

summary(glm(n_intact_fruits~(phen_index*redants_pres)+pop,family="poisson",data=data3))
summary(glm(n_intact_fruits~(phen_index*n_redants)+pop,family="poisson",data=data3))
summary(glm(n_intact_fruits~(t_phen_index*t_n_redants),family="poisson",data=data3))

summary(glm.nb(n_eggs_max~(phen_index*redants_pres)+pop,data=data3))
summary(glm.nb(n_eggs_max~(phen_index*n_redants)+pop,data=data3))

model1<-glm(n_intact_fruits~(t_phen_index*redants_pres)+pop,family="poisson",data=data3)
data3$predicted=predict(model1)

ggplot(data3, aes(x=t_phen_index, y=n_intact_fruits, shape=redants_pres))+
  geom_point()+
  geom_line(aes(x = t_phen_index, y = predicted, linetype=redants_pres))




