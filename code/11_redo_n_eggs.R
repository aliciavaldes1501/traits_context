#Response=n_eggs_max
########################################### Poisson models

#Model with interactions of all variables with population
#With minimum temperature
modeleg_a1<-glmer(n_eggs_max~(z.shoot_h+z.veg_h_mean+z.most_adv+
                  z.n_fl_corrected+z.n_redants+z.avg_d_min_ja)*population+z.n_redants:z.most_adv
                  +(1|id),family="poisson",na.action = "na.fail",
                  control=glmerControl(optimizer="bobyqa"))

#With maximum temperature
modeleg_a2<-glmer(n_eggs_max~(z.shoot_h+z.veg_h_mean+z.most_adv+
                  z.n_fl_corrected+z.n_redants+z.avg_d_max_ja)*population+z.n_redants:z.most_adv
                  +(1|id),family="poisson",na.action = "na.fail",
                  control=glmerControl(optimizer="bobyqa"))
#With temperature sd
modeleg_a3<-glmer(n_eggs_max~(z.shoot_h+z.veg_h_mean+z.most_adv+
                  z.n_fl_corrected+z.n_redants+z.avg_d_sd_ja)*population+z.n_redants:z.most_adv
                  +(1|id),family="poisson",na.action = "na.fail",
                  control=glmerControl(optimizer="bobyqa"))
#With temperature range
modeleg_a4<-glmer(n_eggs_max~(z.shoot_h+z.veg_h_mean+z.most_adv+
                  z.n_fl_corrected+z.n_redants+z.avg_d_range_ja)*population+z.n_redants:z.most_adv
                  +(1|id),family="poisson",na.action = "na.fail",
                  control=glmerControl(optimizer="bobyqa"))

summary(modeleg_a1)
summary(modeleg_a2)
summary(modeleg_a3)
summary(modeleg_a4)

########### MODEL SELECTIONS THAT TAKE FOREVER
########### DO NOT RUN THIS AGAIN!!
tic()
modelseg_a1 <- dredge(modeleg_a1)
toc()
tic()
modelseg_a2 <- dredge(modeleg_a2)
toc()
tic()
modelseg_a3 <- dredge(modeleg_a3)
toc()
tic()
modelseg_a4 <- dredge(modeleg_a4)
toc()
###############################################

#Model averaging
summary(model.avg(modelseg_a1, subset = delta < 2))
summary(model.avg(modelseg_a2, subset = delta < 2)) #Only one model
modelseg_a2[1]
summary(model.avg(modelseg_a3, subset = delta < 2))
summary(model.avg(modelseg_a4, subset = delta < 2)) #Only one model
modelseg_a4[1]



#################################### Negative binomial models - best fit to data, lower AIC
#With minimum temperature
modeleg_a1_nb<-glm.nb(n_eggs_max~(z.shoot_h+z.veg_h_mean+z.most_adv+
               z.n_fl_corrected+z.n_redants+z.avg_d_min_ja)*population,
               data=data1comp, na.action = "na.fail")
vif(glm.nb(n_eggs_max~z.shoot_h+z.veg_h_mean+z.most_adv+
           z.n_fl_corrected+z.n_redants+z.avg_d_min_ja+population,
           data=data1comp, na.action = "na.fail"))
#With maximum temperature
modeleg_a2_nb<-glm.nb(n_eggs_max~(z.shoot_h+z.veg_h_mean+z.most_adv+
               z.n_fl_corrected+z.n_redants+z.avg_d_max_ja)*population,
               data=data1comp, na.action = "na.fail")
#With temperature sd
modeleg_a3_nb<-glm.nb(n_eggs_max~(z.shoot_h+z.veg_h_mean+z.most_adv+
               z.n_fl_corrected+z.n_redants+z.avg_d_sd_ja)*population,
               data=data1comp, na.action = "na.fail")
#With temperature range
modeleg_a4_nb<-glm.nb(n_eggs_max~(z.shoot_h+z.veg_h_mean+z.most_adv+
               z.n_fl_corrected+z.n_redants+z.avg_d_range_ja)*population,
               data=data1comp, na.action = "na.fail")

########### MODEL SELECTIONS 
########### DO NOT RUN THIS AGAIN!!
#Set cluster for parallel computations
library(parallel)
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 3), type = clusterType))
clusterExport(clust, "data1comp")
clusterEvalQ(clust, library(MASS))

tic()
modelseg_a1_nb <- pdredge(modeleg_a1_nb,cluster=clust)
toc()
tic()
modelseg_a2_nb <- pdredge(modeleg_a2_nb,cluster=clust)
toc()
tic()
modelseg_a3_nb <- pdredge(modeleg_a3_nb,cluster=clust)
toc()
tic()
modelseg_a4_nb <- pdredge(modeleg_a4_nb,cluster=clust)
toc()
###############################################

#Model averaging
summary(model.avg(modelseg_a1_nb, subset = delta < 2))
#See the best model and fit it
modelseg_a1_nb[1]
modeleg_a1_nb_best<-glm.nb(n_eggs_max~population+z.avg_d_min_ja+z.most_adv+z.n_fl_corrected+
                    z.n_redants+z.shoot_h+z.veg_h_mean+population:z.avg_d_min_ja+population:z.n_fl_corrected+
                    population:z.shoot_h+population:z.veg_h_mean+population:z.n_redants,
                    data=data1comp,na.action = "na.fail")


par(bg = "black",fg="white",bty="l",col.axis="white",col.lab="white")
colors=c(adjustcolor( "red", alpha.f = 0.4),adjustcolor( "green", alpha.f = 0.4),adjustcolor( "blue", alpha.f = 0.4))
visreg(modeleg_a1_nb_best,"veg_h_mean",by="population",xlab="Mean vegetation height (cm)",
       ylab="Number of eggs",overlay=T,band=F,line=list(col=c("red","green","blue")),
       points=list(cex=1.1,pch=16,col=colors),legend=F,par(cex.lab=1.3,cex.axis=1.3))
visreg(modeleg_a1_nb_best,"veg_h_mean",by="population",xlab="Vegetation height (cm)",
       ylab="Predicted N eggs",overlay=T,band=F,line=list(col=c("red","green","blue")),
       points=list(cex=1.1,pch=16,col=colors),legend=F,par(cex.lab=1.3,cex.axis=1.3),scale="response",
       ylim=c(0,15),xlim=c(0,120))

with(data1comp,lineplot.CI(population,n_eggs_max,cex.lab=1.5,cex=1.5,cex.axis=1.1,
    xlab="",ylab="N eggs",type="p",bty="l",legend=F,col="white"))
with(data1comp,lineplot.CI(population,most_adv,cex.lab=1.5,cex=1.5,cex.axis=1.5,
    xlab="",ylab="",type="p",bty="l",legend=F,col="white"))
with(data1comp,lineplot.CI(population,avg_d_min_ja,cex.lab=1.5,cex=1.5,cex.axis=1.5,ylim=c(11.8,14),
     xlab="",ylab="",type="p",bty="l",legend=F,col="white"))
with(data1comp,lineplot.CI(population,veg_h_mean,cex.lab=1.5,cex=1.5,cex.axis=1.5,
     xlab="",ylab="",type="p",bty="l",legend=F,col="white"))



#R square for the best model
r.squaredLR(modeleg_a1_nb_best)

summary(model.avg(modelseg_a2_nb, subset = delta < 2)) 
summary(model.avg(modelseg_a3_nb, subset = delta < 2))
summary(model.avg(modelseg_a4_nb, subset = delta < 2)) 

modelseg_a3_nb[1]
summary(glm.nb(n_eggs_max~population+z.avg_d_sd_ja+z.most_adv+z.n_fl_corrected+
  z.n_redants+z.shoot_h+z.veg_h_mean+population:z.avg_d_sd_ja+population:z.n_fl_corrected+
  population:z.veg_h_mean+population:z.n_redants,
  data=data1comp,na.action = "na.fail"))

modelseg_a4_nb[1]
summary(glm.nb(n_eggs_max~population+z.avg_d_range_ja+z.most_adv+z.n_fl_corrected+
  z.n_redants+z.shoot_h+z.veg_h_mean+population:z.avg_d_range_ja+population:z.n_fl_corrected+
  population:z.veg_h_mean+population:z.n_redants,
  data=data1comp,na.action = "na.fail"))

#PCA for traits
PCA1<-prcomp(~most_adv+n_fl_corrected+shoot_h,scale=T)
plot(PCA1)
biplot(PCA1)
PCA1
summary(PCA1)
predict(PCA1)
data1comp$pca1<-predict(PCA1)[,1]

dfPCA<-data1comp[,c("most_adv","n_fl_corrected","shoot_h")]
PCA2<-PCA(dfPCA)

t1<-theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.background = element_blank()
)

ggbiplot(PCA1,obs.scale = 1,circle = TRUE)+ theme_bw()+
  geom_point(colour = "grey")+t1

theme_set(theme_bw(20))


plotPCA <- ggbiplot(PCA1, choices = 1:2, scale = 1, 
  obs.scale = 1, var.scale = 1,alpha=1,varname.size=5,varname.adjust=2,
  circle = TRUE)+
  geom_hline(yintercept=0, colour="grey55") +
  geom_vline(xintercept=0, colour="grey55") +
  theme(axis.title = element_text(family = "serif",size=20),
        axis.text = element_text(family = "serif",size=20),
        panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0.5))+
  theme(legend.text = element_text(size = 18, family = 'serif'))+
  theme(legend.title = element_text(size = 18, family = 'serif', face = 'bold'))+
  scale_colour_brewer(palette="Set1")+
  theme(legend.position = "none")+
  geom_point(color="grey")
plot(plotPCA)

#Model using pca1
modeleg_a1_nb_pca<-glm.nb(n_eggs_max~(scale(pca1)+z.veg_h_mean+z.n_redants+z.avg_d_min_ja)*population,
                      data=data1comp, na.action = "na.fail")
vif(glm.nb(n_eggs_max~scale(pca1)+z.veg_h_mean+z.n_redants+z.avg_d_min_ja+population,
           data=data1comp, na.action = "na.fail"))
summary(modeleg_a1_nb_pca)

modelseg_a1_nb_pca <- dredge(modeleg_a1_nb_pca)
summary(model.avg(modelseg_a1_nb_pca, subset = delta < 2)) #TABLE 1
importance(modelseg_a1_nb_pca)

modelseg_a1_nb_pca[1]
modeleg_a1_nb_best<-glm.nb(n_eggs_max~scale(pca1)+z.veg_h_mean+z.n_redants+z.avg_d_min_ja+
                    population+population:z.avg_d_min_ja+population:z.n_redants+
                    population:z.veg_h_mean,data=data1comp, na.action = "na.fail")
r.squaredLR(modeleg_a1_nb_best)

#Test effect of temperature "anomaly" - NOT USED
data_anomaly<-read.table("mean_t_anomaly.txt",header=T,sep="\t",dec=",")
data1comp<-merge(data1comp,data_anomaly,by="id_pl")

modeleg_a1_nb_pca<-glm.nb(n_eggs_max~(scale(pca1)+z.veg_h_mean+z.n_redants+scale(mean_t_anomaly))*population,
                          data=data1comp, na.action = "na.fail")
modelseg_a1_nb_pca <- dredge(modeleg_a1_nb_pca)
summary(model.avg(modelseg_a1_nb_pca, subset = delta < 2))
importance(modelseg_a1_nb_pca)
#Also negative effect! + interaction with pop

#################################################################################3
