#Repeat analyses last paper

#Standardize  reproductive traits

mean_fitness<-aggregate(n_intact_fruits_max~population, data1, mean)

data1<-merge(data1,mean_fitness, by="population")

head(data1)

mean_shoot_h<-aggregate(shoot_h~population, data1, mean)
mean_n_fl<-aggregate(n_fl_corrected~population, data1, mean)
mean_phen_index<-aggregate(phen_index~population, data1, mean)
mean_most_adv<-aggregate(most_adv~population, data1, mean)

sd_shoot_h<-aggregate(shoot_h~population, data1, sd)
sd_n_fl<-aggregate(n_fl_corrected~population, data1, sd)
sd_phen_index<-aggregate(phen_index~population, data1, sd)
sd_most_adv<-aggregate(most_adv~population, data1, sd)

data1<-merge(data1,mean_shoot_h, by="population")
data1<-merge(data1,mean_n_fl, by="population")
data1<-merge(data1,mean_phen_index, by="population")
data1<-merge(data1,mean_most_adv, by="population")

head(data1)
names(data1)

names(data1)[c(4,17:18,21,25,39:43)]<-c("shoot_h","phen_index","most_adv",
                                        "n_intact_fruits_max","n_fl_corrected","n_intact_fruits_mean",
                                        "shoot_h_mean","n_fl_mean","phen_index_mean","most_adv_mean")

data1<-merge(data1,sd_shoot_h, by="population")
data1<-merge(data1,sd_n_fl, by="population")
data1<-merge(data1,sd_phen_index, by="population")
data1<-merge(data1,sd_most_adv, by="population")

names(data1)
names(data1)[c(4,17:18,25,44:47)]<-c("shoot_h","phen_index","most_adv",
                                     "n_fl_corrected","shoot_h_sd","n_fl_sd","phen_index_sd","most_adv_sd")
names(data1)

data1$shoot_h_sta<-(data1$shoot_h-data1$shoot_h_mean)/data1$shoot_h_sd
data1$n_fl_sta<-(data1$n_fl_corrected-data1$n_fl_mean)/data1$n_fl_sd
data1$phen_index_sta<-(data1$phen_index-data1$phen_index_mean)/data1$phen_index_sd
data1$most_adv_sta<-(data1$most_adv-data1$most_adv_mean)/data1$most_adv_sd

summary(data1,by="population")
aggregate(shoot_h_sta~population, data1, sd )
aggregate(n_fl_sta~population, data1, sd )
aggregate(phen_index_sta~population, data1, sd )
aggregate(most_adv_sta~population, data1, sd )

data1$n_intact_fruits_sta<-data1$n_intact_fruits_max/data1$n_intact_fruits_mean

summary(data1,by="population")
aggregate(n_intact_fruits_sta~population, data1, sd )

names(data1)

#Effects of traits on fitness

model1<-lm(n_intact_fruits_sta~most_adv_sta+n_fl_sta+shoot_h_sta+
             population:most_adv_sta+population:n_fl_sta+population:shoot_h_sta,
           data=data1)
library(car)
Anova(model1,type="II")
