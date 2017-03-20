library(pscl)
library(MuMIn)
library(parallel)
library(fmsb)

#### Model 1: Eggs ####
#Include interactions among traits and community context
model_eggs_extra<-hurdle(n_eggs_max~(scale(phen_index1)+scale(shoot_h)+
                  scale(veg_h_mean)+scale(n_redants))*pop+
                  scale(phen_index1):scale(veg_h_mean)+scale(phen_index1):scale(n_redants)+
                  scale(shoot_h):scale(veg_h_mean)+scale(shoot_h):scale(n_redants),
                  data=data3,dist="negbin",zero.dist="binomial",na.action="na.fail")
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 3), type = clusterType))
clusterExport(clust, "data3")
clusterEvalQ(clust, library(pscl))
models_eggs_extra<-pdredge(model_eggs_extra,cluster=clust)
summary(model.avg(models_eggs_extra, subset = delta < 2)) #Table1

#### Model 5: Fruit set ####
##Without Remmene (only 14 plants where fruitset>0)
##Model without predation
model_fruitset_extra<-glm(cbind(n_intact_fruits,n_fl)~(scale(phen_index1)+scale(shoot_h)+
                  scale(veg_h_mean)+scale(meanT))*pop,family="binomial",
                  data=subset(data3,!pop=="R"),na.action="na.fail")
models_fruitset_extra<-dredge(model_fruitset_extra)
summary(model.avg(models_fruitset_extra,subset=delta<2)) 

model_fruitset_extra[1]
model_fruitset_extra_best<-glm(cbind(n_intact_fruits,n_fl)~scale(phen_index1)+scale(shoot_h)+
                          scale(veg_h_mean)+scale(meanT)+pop+
                          scale(phen_index1):pop+scale(shoot_h):pop+
                          scale(veg_h_mean):pop+scale(meanT):pop,family="binomial",
                          data=subset(data3,!pop=="R"),na.action="na.fail")
summary(model_fruitset_extra_best)
NagelkerkeR2(model_fruitset_extra_best)


