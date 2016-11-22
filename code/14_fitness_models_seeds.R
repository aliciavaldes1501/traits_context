#####################################################################################
#############    Models using seeds for fitness / reproductive output   #############
#####################################################################################

#Reading data from ~100 marked plants per population
data2<-read.table("datafile1_seeds.txt",header=T,sep="\t",dec=",")
head(data2)
tail(data2)
str(data2)

#Construct new variable: attack
data2$attack<-as.factor(with(data2,ifelse(n_eggs_max>0,"1","0")))

head(data2)
tail(data2)
summary(data2)
names(data2)

data2comp<-data2[complete.cases(data2[37:54]),]
data2comp<-subset(data2comp,seed_n_per_shoot<10000) #Removed 1 outlier from Tanga with 15 fruits!
####NOT RUN data2comp<-subset(data2comp,population!="Remmene"&population!="Högsjön") #Removed Remmene cause only 8 plants with seeds
data2compA<-data2comp[complete.cases(data2comp[55]),]
data2compB<-data2comp[complete.cases(data2comp[56]),]

#Standardize X variables
z <- scale(data2compA[,c("bud_h","shoot_h","veg_h_min","veg_h_max","veg_h_mean",
                        "phen_index","most_adv","n_fl_corrected","n_eggs_max","n_redants",
                        "dist_closest_redants","dist_closest_redants1","avg_d_mean","avg_d_sd","avg_d_min","avg_d_max",
                        "avg_d_range","avg_day_mean","avg_day_sd","avg_day_min","avg_day_max",            
                        "avg_day_range","avg_d_mean_ja","avg_d_sd_ja","avg_d_min_ja",
                        "avg_d_max_ja","avg_d_range_ja","avg_day_mean_ja","avg_day_sd_ja",
                        "avg_day_min_ja","avg_day_max_ja","avg_day_range_ja")])
data2compA$z.bud_h <- z[,1]
data2compA$z.shoot_h <- z[,2]
data2compA$z.veg_h_min <- z[,3]
data2compA$z.veg_h_max <- z[,4]
data2compA$z.veg_h_mean <- z[,5]
data2compA$z.phen_index <- z[,6]
data2compA$z.most_adv <- z[,7]
data2compA$z.n_fl_corrected <- z[,8]
data2compA$z.n_eggs_max<-z[,9]
data2compA$z.n_redants <- z[,10]
data2compA$z.dist_closest_redants <- z[,11]
data2compA$z.dist_closest_redants1 <- z[,12]
data2compA$z.avg_d_mean <- z[,13]
data2compA$z.avg_d_sd <- z[,14]
data2compA$z.avg_d_min <- z[,15]
data2compA$z.avg_d_max <- z[,16]
data2compA$z.avg_d_range <- z[,17]
data2compA$z.avg_day_mean <- z[,18]
data2compA$z.avg_day_sd <- z[,19]
data2compA$z.avg_day_min <- z[,20]
data2compA$z.avg_day_max <- z[,21]
data2compA$z.avg_day_range <- z[,22]
data2compA$z.avg_d_mean_ja <- z[,23]
data2compA$z.avg_d_sd_ja <- z[,24]
data2compA$z.avg_d_min_ja <- z[,25]
data2compA$z.avg_d_max_ja <- z[,26]
data2compA$z.avg_d_range_ja <- z[,27]
data2compA$z.avg_day_mean_ja <- z[,28]
data2compA$z.avg_day_sd_ja <- z[,29]
data2compA$z.avg_day_min_ja <- z[,30]
data2compA$z.avg_day_max_ja <- z[,31]
data2compA$z.avg_day_range_ja <- z[,32]

z <- scale(data2compB[,c("bud_h","shoot_h","veg_h_min","veg_h_max","veg_h_mean",
                         "phen_index","most_adv","n_fl_corrected","n_eggs_max","n_redants",
                         "dist_closest_redants","dist_closest_redants1","avg_d_mean","avg_d_sd","avg_d_min","avg_d_max",
                         "avg_d_range","avg_day_mean","avg_day_sd","avg_day_min","avg_day_max",            
                         "avg_day_range","avg_d_mean_ja","avg_d_sd_ja","avg_d_min_ja",
                         "avg_d_max_ja","avg_d_range_ja","avg_day_mean_ja","avg_day_sd_ja",
                         "avg_day_min_ja","avg_day_max_ja","avg_day_range_ja")])
data2compB$z.bud_h <- z[,1]
data2compB$z.shoot_h <- z[,2]
data2compB$z.veg_h_min <- z[,3]
data2compB$z.veg_h_max <- z[,4]
data2compB$z.veg_h_mean <- z[,5]
data2compB$z.phen_index <- z[,6]
data2compB$z.most_adv <- z[,7]
data2compB$z.n_fl_corrected <- z[,8]
data2compB$z.n_eggs_max<-z[,9]
data2compB$z.n_redants <- z[,10]
data2compB$z.dist_closest_redants <- z[,11]
data2compB$z.dist_closest_redants1 <- z[,12]
data2compB$z.avg_d_mean <- z[,13]
data2compB$z.avg_d_sd <- z[,14]
data2compB$z.avg_d_min <- z[,15]
data2compB$z.avg_d_max <- z[,16]
data2compB$z.avg_d_range <- z[,17]
data2compB$z.avg_day_mean <- z[,18]
data2compB$z.avg_day_sd <- z[,19]
data2compB$z.avg_day_min <- z[,20]
data2compB$z.avg_day_max <- z[,21]
data2compB$z.avg_day_range <- z[,22]
data2compB$z.avg_d_mean_ja <- z[,23]
data2compB$z.avg_d_sd_ja <- z[,24]
data2compB$z.avg_d_min_ja <- z[,25]
data2compB$z.avg_d_max_ja <- z[,26]
data2compB$z.avg_d_range_ja <- z[,27]
data2compB$z.avg_day_mean_ja <- z[,28]
data2compB$z.avg_day_sd_ja <- z[,29]
data2compB$z.avg_day_min_ja <- z[,30]
data2compB$z.avg_day_max_ja <- z[,31]
data2compB$z.avg_day_range_ja <- z[,32]

head(data2compA)
head(data2compB)
summary(data2compA)
summary(data2compB)
str(data2compA)
str(data2compB)

#Construct models for fitness
#First measured as seed_n_per_shoot
#Construct new variable: observation-level random effect
data2compA$id<-1:245
attach(data2compA)
#using n_eggs

#From here NOT RUN
#Full model
# model2se_a<-glmer(round(seed_n_per_shoot)~(z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+
#                   z.n_redants+z.dist_closest_redants+z.avg_d_min_ja+n_eggs_max)*population+(1|id),
#                   family="poisson",na.action = "na.fail",control=glmerControl(optimizer ="bobyqa",optCtrl=list(maxfun=1e5)))
# summary(model2se_a)
# Anova(model2se_a)
# r.squaredLR(model2se_a,null.RE=T)
# plot(model2se_a)

#Reduced model
#NOT RUN models1se_b<-dredge(model1se_a,trace=T)
#NOT RUN head(models1se_b)
# model2se_b<-glmer(round(seed_n_per_shoot)~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+
#                     z.n_redants+z.dist_closest_redants+z.avg_d_min_ja+z.n_eggs_max+population+
#                     (1|id),family="poisson",na.action = "na.fail",
#                   control=glmerControl(optimizer ="bobyqa",optCtrl=list(maxfun=1e5)))
# summary(model2se_b)
# Anova(mode21se_b)
# r.squaredLR(model2se_b,null.RE=T) 
# plot(model2se_b)
#Till here NOT RUN

#Zero-inflated model
model2se_c1<-zeroinfl(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
            z.avg_d_min_ja+z.n_eggs_max)*population,dist="negbin",na.action="na.fail",data=data2compA)
model2se_c1bis<-zeroinfl(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
             z.avg_d_min_ja+z.n_eggs_max+z.n_intact_fruits)*population,dist="negbin",na.action="na.fail",data=data2compA)
model2se_c2<-zeroinfl(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
             z.n_eggs_max)*population,dist="negbin",na.action="na.fail",data=data2compA)
model2se_c3<-zeroinfl(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected)*population,dist="negbin",na.action="na.fail",data=data2compA)
model2se_c4<-zeroinfl(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
             z.avg_d_max_ja+z.n_eggs_max)*population,dist="negbin",na.action="na.fail",data=data2compA)
model2se_c5<-zeroinfl(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
             z.avg_d_sd_ja+z.n_eggs_max)*population,dist="negbin",na.action="na.fail",data=data2compA)
model2se_c6<-zeroinfl(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
             z.avg_d_range_ja+z.n_eggs_max)*population,dist="negbin",na.action="na.fail",data=data2compA)

summary(model2se_c1)
r.squaredLR(model2se_c1)
plot(fitted(model2se_c1,type="response"),resid(model2se_c1))

#Set cluster for parallel computations
library(parallel)
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 3), type = clusterType))
clusterExport(clust, "data2compA")
clusterEvalQ(clust, library(pscl))

#MODEL SELECTION
library(tictoc)
library(beepr)
tic()
models2se_c1 <- pdredge(model2se_c1,cluster=clust);beep(sound=8)
toc()
tic()
models2se_c1bis <- pdredge(model2se_c1bis,cluster=clust);beep(sound=8)
toc()
tic()
models2se_c2 <- pdredge(model2se_c2,cluster=clust);beep(sound=8)
toc()
tic()
models2se_c3 <- pdredge(model2se_c3,cluster=clust);beep(sound=8)
toc()
models2se_c4 <- pdredge(model2se_c4,cluster=clust)
models2se_c5 <- pdredge(model2se_c5,cluster=clust)
models2se_c6 <- pdredge(model2se_c6,cluster=clust);beep(sound=8)

#Model averaging
summary(model.avg(models2se_c1,subset=delta<2)) #results

model2se_c1<-zeroinfl(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
z.avg_d_min_ja+z.n_eggs_max+z.n_intact_fruits)*population,dist="negbin",na.action="na.fail")
#quasipoisson - The good one? ;)
model2se_c1_q<-glm(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
z.avg_d_min_ja+z.n_eggs_max)*population,family="quasipoisson",na.action="na.fail")
#negative binomial
model2se_c1_nb<-glm.nb(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
              z.avg_d_min_ja+z.n_eggs_max)*population,na.action="na.fail",data=data2compA)
model2se_c1_p<-glm(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
              z.avg_d_min_ja+z.n_eggs_max+z.n_intact_fruits)*population,family="poisson",na.action="na.fail",data=data2compA)

chat<-deviance(model2se_c1_q)/df.residual(model2se_c1_q)

x.quasipoisson <- function(...) {
res <- quasipoisson(...)
res$aic <- poisson(...)$aic
res
} 
model2se_c1_q<-update(model2se_c1_q,family = "x.quasipoisson") 
models2se_c1_q <- dredge(model2se_c1_q,rank="QAIC",chat=chat)

summary(model.avg(models2se_c1_q,subset=delta<2)) #Result model averaging with quasipoisson
AIC(model2se_c1,model2se_c1_nb)
QAIC(model2se_c1_q,chat=chat)
importance(models2se_c1_q)

#Comparisons
plot(hist(fitted(model2se_c1)))
plot(hist(fitted(model2se_c1_q)))
plot(hist(fitted(model2se_c1_nb)))
plot(fitted(model2se_c1_p), fitted(model2se_c1_q),
     xlab="Fitted values poisson",ylab="Fitted values quasipoisson") #Identical
#Quasipoisson actually similar fit than zeroinfl nb
plot(fitted(model2se_c1), fitted(model2se_c1_q),
     xlab="Fitted values zero-inflated negative binomial",ylab="Fitted values quasipoisson")
plot(fitted(model2se_c1), fitted(model2se_c1_nb))#nb worse fit
plot(fitted(model2se_q), fitted(model2se_c1_nb))#nb worse fit

boxplot(abs(resid(model2se_c1) - resid(model2se_c1_q)))
sum(abs(resid(model2se_c1) > 1))
sum(abs(resid(model2se_c1_q) > 1))

summary(model.avg(get.models(models2se_c2,subset=delta<2,cluster=clust)))
summary(model.avg(get.models(models2se_c3,subset=delta<2,cluster=clust)))
summary(model.avg(get.models(models2se_c4,subset=delta<2,cluster=clust)))
summary(model.avg(get.models(models2se_c5,subset=delta<2,cluster=clust)))
summary(model.avg(get.models(models2se_c6,subset=delta<2,cluster=clust)))


PCA2<-prcomp(~most_adv+n_fl_corrected+shoot_h,scale=T)
plot(PCA2)
biplot(PCA2)
PCA2
summary(PCA2)
predict(PCA2)
data2compA$pca1<-predict(PCA2)[,1]

#Models with pca1
model2se_c1<-zeroinfl(round(seed_n_per_shoot)~(scale(pca1)+
             z.avg_d_min_ja+z.n_eggs_max+z.n_intact_fruits)*population,dist="negbin",na.action="na.fail")
models2se_c1 <- dredge(model2se_c1)


#quasipoisson
model2se_c1_q<-glm(round(seed_n_per_shoot)~(scale(pca1)+
              z.avg_d_min_ja+z.n_eggs_max+z.n_intact_fruits)*population,family="quasipoisson",na.action="na.fail")


#################################################################################################

with(data2compA,lineplot.CI(population,seed_n_per_shoot,cex.lab=1.5,cex=1.5,cex.axis=1.5,
     xlab="",ylab="N seeds per plant",type="p",bty="l",legend=F,col="white"))

with(data2compA,lineplot.CI(population,seed_n_per_shoot,cex.lab=1.5,cex=1.5,cex.axis=1.5,
    xlab="",ylab="N seeds per plant",type="p",bty="l",legend=F,col="white"))
with(subset(data2compA,seed_n_per_shoot>0),lineplot.CI(population,seed_n_per_shoot,cex.lab=1.5,cex=1.5,cex.axis=1.5,
     xlab="",ylab="N seeds per plant",type="p",bty="l",legend=F,col="white"))
with(data2compA,lineplot.CI(population,log(seed_n_per_shoot+1),cex.lab=1.5,cex=1.5,cex.axis=1.5,
    xlab="",ylab="N seeds per plant",type="p",bty="l",legend=F,col="white"))
with(subset(data2compA,seed_n_per_shoot>0),lineplot.CI(population,log(seed_n_per_shoot),cex.lab=1.5,cex=1.5,cex.axis=1.5,
    xlab="",ylab="N seeds per plant",type="p",bty="l",legend=F,col="white"))

with(subset(data2compA,seed_n_per_shoot>0),plot(population,seed_n_per_shoot))
with(subset(data2compA,seed_n_per_shoot>0),plot(population,log(seed_n_per_shoot)))
with(subset(data2compA,seed_n_per_shoot>0),hist(log(seed_n_per_shoot)))
with(subset(data2compA,seed_n_per_shoot>0),summary(lm(log(seed_n_per_shoot)~population)))
with(subset(data2compA,seed_n_per_shoot>0),summary(glm(log(seed_n_per_shoot)~population)))


###################################################################################################
##Varpart

model2se_c1_tr_c_i_p<-zeroinfl(round(seed_n_per_shoot)~(z.shoot_h+z.most_adv+z.n_fl_corrected+
                                             z.avg_d_min_ja+z.n_eggs_max)*population
                          ,dist="negbin",na.action = "na.fail",data=data2compA)
r.squaredLR(model2se_c1_tr_c_i_p) #R2 with pop in the model

model2se_c1_tr_c_i<-zeroinfl(round(seed_n_per_shoot)~z.shoot_h+z.most_adv+z.n_fl_corrected+z.avg_d_min_ja+
                          z.n_eggs_max,dist="negbin",na.action = "na.fail",data=data2compA)
model2se_c1_tr<-zeroinfl(round(seed_n_per_shoot)~z.shoot_h+z.most_adv+z.n_fl_corrected,
                    dist="negbin",na.action = "na.fail",data=data2compA)
model2se_c1_c<-zeroinfl(round(seed_n_per_shoot)~z.avg_d_min_ja,
                   dist="negbin",na.action = "na.fail",data=data2compA)
model2se_c1_i<-zeroinfl(round(seed_n_per_shoot)~
                     z.n_eggs_max,dist="negbin",na.action = "na.fail",data=data2compA)
model2se_c1_tr_c<-zeroinfl(round(seed_n_per_shoot)~z.shoot_h+z.most_adv+z.n_fl_corrected+z.avg_d_min_ja
                      ,dist="negbin",na.action = "na.fail",data=data2compA)
model2se_c1_tr_i<-zeroinfl(round(seed_n_per_shoot)~z.shoot_h+z.most_adv+z.n_fl_corrected+
                        z.n_eggs_max,dist="negbin",na.action = "na.fail",data=data2compA)
model2se_c1_c_i<-zeroinfl(round(seed_n_per_shoot)~z.avg_d_min_ja+
                       z.n_eggs_max,dist="negbin",na.action = "na.fail",data=data2compA)

R_tr_c_i<-r.squaredLR(model2se_c1_tr_c_i)     #R2 removing the effect of pop and interactions
R_tr<-r.squaredLR(model2se_c1_tr)
R_c<-r.squaredLR(model2se_c1_c)
R_i<-r.squaredLR(model2se_c1_i)
R_tr_c<-r.squaredLR(model2se_c1_tr_c)
R_tr_i<-r.squaredLR(model2se_c1_tr_i)
R_c_i<-r.squaredLR(model2se_c1_c_i)

tr<-R_tr_c_i-R_c_i
c<-R_tr_c_i-R_tr_i
i<-R_tr_c_i-R_tr_c
tr_c<-R_tr_c_i-R_i-tr-c
tr_i<-R_tr_c_i-R_c-tr-i
c_i<-R_tr_c_i-R_tr-c-i
tr_c_i<-R_tr_c_i-tr-c-i-tr_c-tr_i-c_i

(tr/R_tr_c_i)*100
(c/R_tr_c_i)*100
(i/R_tr_c_i)*100
(tr_c/R_tr_c_i)*100
(tr_i/R_tr_c_i)*100
(c_i/R_tr_c_i)*100
(tr_c_i/R_tr_c_i)*100

tr+c+i+tr_c+tr_i+c_i+tr_c_i
