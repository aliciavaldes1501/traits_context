#Choosing between temperature variables
summary(with(data1,glm(attack ~ avg_d_mean,family="binomial")))
summary(with(data1,glm(attack ~ avg_d_sd,family="binomial")))
summary(with(data1,glm(attack ~ avg_d_min,family="binomial")))
summary(with(data1,glm(attack ~ avg_d_max,family="binomial")))
summary(with(data1,glm(attack ~ avg_d_range,family="binomial")))
summary(with(data1,glm(attack ~ avg_day_mean,family="binomial")))
summary(with(data1,glm(attack ~ avg_day_sd,family="binomial")))
summary(with(data1,glm(attack ~ avg_day_min,family="binomial")))
summary(with(data1,glm(attack ~ avg_day_max,family="binomial")))
summary(with(data1,glm(attack ~ avg_day_range,family="binomial")))
summary(with(data1,glm(attack ~ avg_d_mean_ja,family="binomial")))
summary(with(data1,glm(attack ~ avg_d_sd_ja,family="binomial")))
summary(with(data1,glm(attack ~ avg_d_min_ja,family="binomial")))
summary(with(data1,glm(attack ~ avg_d_max_ja,family="binomial")))
summary(with(data1,glm(attack ~ avg_d_range_ja,family="binomial")))
summary(with(data1,glm(attack ~ avg_day_mean_ja,family="binomial")))
summary(with(data1,glm(attack ~ avg_day_sd_ja,family="binomial")))
summary(with(data1,glm(attack ~ avg_day_min_ja,family="binomial")))
summary(with(data1,glm(attack ~ avg_day_max_ja,family="binomial")))
summary(with(data1,glm(attack ~ avg_day_range_ja,family="binomial")))

summary(with(data1comp,glm(attack ~ z.veg_h_mean,family="binomial")))
summary(with(data1comp,glm(attack ~ z.veg_h_min,family="binomial"))) #Best
summary(with(data1comp,glm(attack ~ z.veg_h_max,family="binomial")))

summary(with(data1,glm(n_eggs_max ~ avg_d_mean,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_d_sd,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_d_min,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_d_max,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_d_range,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_day_mean,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_day_sd,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_day_min,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_day_max,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_day_range,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_d_mean_ja,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_d_sd_ja,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_d_min_ja,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_d_max_ja,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_d_range_ja,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_day_mean_ja,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_day_sd_ja,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_day_min_ja,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_day_max_ja,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_day_range_ja,family="poisson")))

#Best + and -
summary(with(data1,glm(attack ~ avg_d_sd_ja,family="binomial")))
summary(with(data1,glm(attack ~ avg_d_min_ja,family="binomial")))

summary(with(data1,glm(n_eggs_max ~ avg_d_sd_ja,family="poisson")))
summary(with(data1,glm(n_eggs_max ~ avg_d_min_ja,family="poisson")))

with(data1,plot(avg_d_sd_ja~n_eggs_max))
with(data1,plot(avg_d_min_ja~n_eggs_max))

data1comp<-data1[complete.cases(data1[37:54]),]

#######################################################################
par(mfrow=c(1,2))
with(data1,plot(avg_d_max_ja,avg_d_range_ja,pch=20))
abline(lm(avg_d_range_ja~avg_d_max_ja))
with(data1,plot(avg_d_min_ja,avg_d_range_ja,pch=20))
abline(lm(avg_d_range_ja~avg_d_min_ja))


#Standardize X variables
z <- scale(data1comp[,c("bud_h","shoot_h","veg_h_min","veg_h_max","veg_h_mean","diff_veg_h_max_shoot_h","diff_veg_h_mean_shoot_h",
                    "phen_index","most_adv","n_fl_corrected","n_eggs_max","n_redants",
                    "dist_closest_redants","dist_closest_redants1","avg_d_mean","avg_d_sd","avg_d_min","avg_d_max",
                    "avg_d_range","avg_day_mean","avg_day_sd","avg_day_min","avg_day_max",            
                    "avg_day_range","avg_d_mean_ja","avg_d_sd_ja","avg_d_min_ja",
                    "avg_d_max_ja","avg_d_range_ja","avg_day_mean_ja","avg_day_sd_ja",
                    "avg_day_min_ja","avg_day_max_ja","avg_day_range_ja")])
data1comp$z.bud_h <- z[,1]
data1comp$z.shoot_h <- z[,2]
data1comp$z.veg_h_min <- z[,3]
data1comp$z.veg_h_max <- z[,4]
data1comp$z.veg_h_mean <- z[,5]
data1comp$z.diff_veg_h_max_shoot_h <- z[,6]
data1comp$z.diff_veg_h_mean_shoot_h <- z[,7]
data1comp$z.phen_index <- z[,8]
data1comp$z.most_adv <- z[,9]
data1comp$z.n_fl_corrected <- z[,10]
data1comp$z.n_eggs_max<-z[,11]
data1comp$z.n_redants <- z[,12]
data1comp$z.dist_closest_redants <- z[,13]
data1comp$z.dist_closest_redants1 <- z[,14]
data1comp$z.avg_d_mean <- z[,15]
data1comp$z.avg_d_sd <- z[,16]
data1comp$z.avg_d_min <- z[,17]
data1comp$z.avg_d_max <- z[,18]
data1comp$z.avg_d_range <- z[,19]
data1comp$z.avg_day_mean <- z[,20]
data1comp$z.avg_day_sd <- z[,21]
data1comp$z.avg_day_min <- z[,22]
data1comp$z.avg_day_max <- z[,23]
data1comp$z.avg_day_range <- z[,24]
data1comp$z.avg_d_mean_ja <- z[,25]
data1comp$z.avg_d_sd_ja <- z[,26]
data1comp$z.avg_d_min_ja <- z[,27]
data1comp$z.avg_d_max_ja <- z[,28]
data1comp$z.avg_d_range_ja <- z[,29]
data1comp$z.avg_day_mean_ja <- z[,30]
data1comp$z.avg_day_sd_ja <- z[,31]
data1comp$z.avg_day_min_ja <- z[,32]
data1comp$z.avg_day_max_ja <- z[,33]
data1comp$z.avg_day_range_ja <- z[,34]

head(data1comp)
summary(data1comp)

#Construct models for interaction (intensity)
#Response=attack
attach(data1comp)
library(lme4)
library(MuMIn)
#Model with interactions of all variables with population
#With minimum temperature
model2a1<-glm(attack~(z.shoot_h+z.most_adv+z.n_fl_corrected+
               z.veg_h_mean+z.avg_d_min_ja+z.n_redants)*
               population,family="binomial",na.action = "na.fail",
              data=data1comp)
#With maximum temperature
model2a2<-glm(attack~(z.shoot_h+z.most_adv+z.n_fl_corrected+
                z.veg_h_mean+z.avg_d_max_ja+z.n_redants)*
                population,family="binomial",na.action = "na.fail",
              data=data1comp)
#With temperature sd
model2a3<-glm(attack~(z.shoot_h+z.most_adv+z.n_fl_corrected+
                z.veg_h_mean+z.avg_d_sd_ja+z.n_redants)*
                population,family="binomial",na.action = "na.fail",
              data=data1comp)
#With temperature range
model2a4<-glm(attack~(z.shoot_h+z.most_adv+z.n_fl_corrected+
                z.veg_h_mean+z.avg_d_range_ja+z.n_redants)*
                population,family="binomial",na.action = "na.fail",
              data=data1comp)

summary(model2a1)
summary(model2a2)
summary(model2a3)
summary(model2a4)

#Set cluster for parallel computations
library(parallel)
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 3), type = clusterType))

clusterExport(clust, "data1comp")
clusterEvalQ(clust, library(lme4))

#MODEL SELECTIONS
tic()
models2a1 <- pdredge(model2a1,cluster=clust)
models2a2 <- pdredge(model2a2,cluster=clust)
models2a3 <- pdredge(model2a3,cluster=clust)
models2a4 <- pdredge(model2a4,cluster=clust)
toc()

#Model averaging
summary(model.avg(models2a1, subset = delta < 2))
summary(model.avg(models2a2, subset = delta < 2))
summary(model.avg(models2a3, subset = delta < 2))
summary(model.avg(models2a4, subset = delta < 2))

#See the best model
models2a1[1]
#R square for the best model
r.squaredLR(glm(attack~z.shoot_h+z.most_adv+z.n_fl_corrected+z.veg_h_mean+z.avg_d_min_ja+
           population+population:z.veg_h_mean,family="binomial",na.action = "na.fail"))

#See the best model
models2a2[1]
#R square for the best model
r.squaredLR(glm(attack~,family="binomial",na.action = "na.fail"))

#See the best model
models2a3[1]
#R square for the best model
r.squaredLR(glm(attack~,family="binomial",na.action = "na.fail"))

#See the best model
models2a4[1]
#R square for the best model
r.squaredLR(glm(attack~,family="binomial",na.action = "na.fail"))







##################### MODIFY FROM HERE




#Varpart using env. context, comm. context
#Model with population as fixed effect but using R2 for model without pop effect ¿?

#tr=traits
#ec=environmental context
#cc=community context

model2c_tr_ec_cc<-glmer(attack~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+z.n_redants+z.dist_closest_redants+z.avg_d_min_ja+(1|population),family="binomial")
model2c_tr<-glmer(attack~z.shoot_h+z.most_adv+z.n_fl_corrected+(1|population),family="binomial")
model2c_ec<-glmer(attack~z.veg_h_mean+z.avg_d_min_ja+(1|population),family="binomial")
model2c_cc<-glmer(attack~z.n_redants+z.dist_closest_redants+(1|population),family="binomial")
model2c_tr_ec<-glmer(attack~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+z.avg_d_min_ja+(1|population),family="binomial")
model2c_tr_cc<-glmer(attack~z.shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.dist_closest_redants+(1|population),family="binomial")
model2c_ec_cc<-glmer(attack~z.veg_h_mean+z.n_redants+z.dist_closest_redants+z.avg_d_min_ja+(1|population),family="binomial")

R_tr_ec_cc<-r.squaredLR(model2c_tr_ec_cc, null.RE = TRUE)
R_tr<-r.squaredLR(model2c_tr, null.RE = TRUE)
R_ec<-r.squaredLR(model2c_ec, null.RE = TRUE)
R_cc<-r.squaredLR(model2c_cc, null.RE = TRUE)
R_tr_ec<-r.squaredLR(model2c_tr_ec, null.RE = TRUE)
R_tr_cc<-r.squaredLR(model2c_tr_cc, null.RE = TRUE)
R_ec_cc<-r.squaredLR(model2c_ec_cc, null.RE = TRUE)

tr<-R_tr_ec_cc-R_ec_cc
ec<-R_tr_ec_cc-R_tr_cc
cc<-R_tr_ec_cc-R_tr_ec
tr_ec<-R_tr_ec_cc-R_cc-tr-ec
tr_cc<-R_tr_ec_cc-R_ec-tr-cc
ec_cc<-R_tr_ec_cc-R_tr-ec-cc
tr_ec_cc<-R_tr_ec_cc-tr-ec-cc-tr_ec-tr_cc-ec_cc

(tr/R_tr_ec_cc)*100
(ec/R_tr_ec_cc)*100
(cc/R_tr_ec_cc)*100
(tr_ec/R_tr_ec_cc)*100
(tr_cc/R_tr_ec_cc)*100
(ec_cc/R_tr_ec_cc)*100
(tr_ec_cc/R_tr_ec_cc)*100

tr+ec+cc+tr_ec+tr_cc+ec_cc+tr_ec_cc


