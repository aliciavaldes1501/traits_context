#Construct models for fitness
#First measured as n_intact_fruits_max

#Using n_eggs as interaction variable

#Model with interactions of all variables with population
#With minimum temperature - removed ants and veg height, leaving
model1fr_a1<-glm(n_intact_fruits~(z.shoot_h+z.most_adv+z.n_fl_corrected+
                z.avg_d_min_ja+z.n_eggs_max)*population
                ,family="poisson",na.action = "na.fail",data=data1comp)
summary(model1fr_a1)
vif(glm(n_intact_fruits~z.shoot_h+z.most_adv+z.n_fl_corrected+
                           z.avg_d_min_ja+z.n_eggs_max+population
        ,family="poisson",na.action = "na.fail",data=data1comp))
vif(model1fr_a1)

model1fr_a1_nb<-glm.nb(n_intact_fruits~(z.shoot_h+z.most_adv+
                 z.n_fl_corrected+z.avg_d_min_ja+z.n_eggs_max)*population
                ,na.action = "na.fail",data=data1comp)
summary(model1fr_a1_nb)

AIC(model1fr_a1,model1fr_a1_nb) #Poisson is better1

#With maximum temperature
model1fr_a2<-glm(n_intact_fruits~(z.shoot_h+z.most_adv+z.n_fl_corrected+
                 z.avg_d_max_ja+z.n_eggs_max)*population
                 ,family="poisson",na.action = "na.fail",data=data1comp)
#With temperature sd
model1fr_a3<-glm(n_intact_fruits~(z.shoot_h+z.most_adv+z.n_fl_corrected+
                 z.avg_d_sd_ja+z.n_eggs_max)*population
                 ,family="poisson",na.action = "na.fail",data=data1comp)
#With temperature range
model1fr_a4<-glm(n_intact_fruits~(z.shoot_h+z.most_adv+z.n_fl_corrected+
                z.avg_d_range_ja+z.n_eggs_max)*population
                 ,family="poisson",na.action = "na.fail",data=data1comp)

#Set cluster for parallel computations
library(parallel)
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 3), type = clusterType))
clusterExport(clust, "data1comp")

########### MODEL SELECTIONS
tic()
models1fr_a1 <- pdredge(model1fr_a1,cluster=clust)
toc()
tic()
models1fr_a2 <- pdredge(model1fr_a2,cluster=clust)
toc()
tic()
models1fr_a3 <- pdredge(model1fr_a3,cluster=clust)
toc()
tic()
models1fr_a4 <- pdredge(model1fr_a4,cluster=clust)
toc()

###############################################

#Model averaging
summary(model.avg(models1fr_a1, subset = delta < 2))
summary(model.avg(models1fr_a2, subset = delta < 2))
summary(model.avg(models1fr_a3, subset = delta < 2))
summary(model.avg(models1fr_a4, subset = delta < 2))

summary(glm(n_intact_fruits~z.shoot_h+z.most_adv+z.n_fl_corrected+
                      z.avg_d_min_ja+z.n_eggs_max+population
   ,family="poisson",na.action = "na.fail",data=data1comp))

with(data1comp,lineplot.CI(population,n_intact_fruits,cex.lab=1.5,cex=1.5,cex.axis=1.5,
     xlab="",ylab="N intact fruits per plant",type="p",bty="l",legend=F,col="white",ylim=c(0,4)))

#Model using pca1
model1fr_a1_pca<-glm(n_intact_fruits~(scale(pca1)+
                                    z.avg_d_min_ja+z.n_eggs_max)*population
                 ,family="poisson",na.action = "na.fail",data=data1comp)
summary(model1fr_a1_pca)
vif(glm(n_intact_fruits~scale(pca1)+z.avg_d_min_ja+z.n_eggs_max+population,
        family="poisson",na.action = "na.fail",data=data1comp))

models1fr_a1_pca <- dredge(model1fr_a1_pca)
summary(model.avg(models1fr_a1_pca, subset = delta < 2)) #Only one!
models1fr_a1_pca[1]

models1fr_a1_pca_best<-glm(n_intact_fruits~scale(pca1)+z.avg_d_min_ja+z.n_eggs_max+population
                           ,family="poisson",na.action = "na.fail",data=data1comp)
summary(models1fr_a1_pca_best) #TABLE 2
vif(models1fr_a1_pca_best)
importance(models1fr_a1_pca)

r.squaredLR(models1fr_a1_pca_best)


###################################################################################
