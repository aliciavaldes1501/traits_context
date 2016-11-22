#With prop_pred (binomial) --> because n_predated depends on n_fl
data1$prop_pred<-cbind(data1$n_predated,data1$n_fl_corrected)
data1comp$prop_pred<-cbind(data1comp$n_predated,data1comp$n_fl_corrected)

#Model with interactions of all variables with population
model2pred_prop_a<-glm(prop_pred~(z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+
                          z.n_redants+z.dist_closest_redants+z.avg_d_min_ja)*population
                         ,family="binomial",na.action = "na.fail")
summary(model2pred_prop_a)
#Model with population (no significant interactions in the model above)
model2pred_prop_b<-glm(prop_pred~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+
                           z.n_redants+z.dist_closest_redants+z.avg_d_min_ja+population,
                         family="binomial",na.action = "na.fail")
summary(model2pred_prop_b)
Anova(model2pred_prop_b)
r.squaredLR(model2pred_prop_b) 
#Model without interactions and population as a random effect
model2pred_prop_c<-glmer(prop_pred~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+
                           z.n_redants+z.dist_closest_redants+z.avg_d_min_ja
                         +(1|population),family="binomial",na.action = "na.fail")
summary(model2pred_prop_c)
r.squaredLR(model2pred_prop_c,null.RE=T) 

AIC(model2pred_prop_a,model2pred_prop_b,model2pred_prop_c)

#Varpart using model2pred_prop_b and traits, context, pop
model2pred_prop_b

#tr=traits
#co=environmental + community context
#po=population

model2pred_prop_b_tr_co_po<-glm(prop_pred~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+z.n_redants+z.dist_closest_redants+z.avg_d_min_ja+population,family="binomial")
model2pred_prop_b_tr<-glm(prop_pred~z.shoot_h+z.most_adv+z.n_fl_corrected,family="binomial")
model2pred_prop_b_co<-glm(prop_pred~z.veg_h_mean+z.avg_d_min_ja+z.n_redants+z.dist_closest_redants,family="binomial")
model2pred_prop_b_po<-glm(prop_pred~population,family="binomial")
model2pred_prop_b_tr_co<-glm(prop_pred~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+z.avg_d_min_ja+z.n_redants+z.dist_closest_redants,family="binomial")
model2pred_prop_b_tr_po<-glm(prop_pred~z.shoot_h+z.most_adv+z.n_fl_corrected+population,family="binomial")
model2pred_prop_b_co_po<-glm(prop_pred~z.veg_h_mean+z.n_redants+z.dist_closest_redants+z.avg_d_min_ja+population,family="binomial")

R_tr_co_po<-r.squaredLR(model2pred_prop_b_tr_co_po)
R_tr<-r.squaredLR(model2pred_prop_b_tr)
R_co<-r.squaredLR(model2pred_prop_b_co)
R_po<-r.squaredLR(model2pred_prop_b_po)
R_tr_co<-r.squaredLR(model2pred_prop_b_tr_co)
R_tr_po<-r.squaredLR(model2pred_prop_b_tr_po)
R_co_po<-r.squaredLR(model2pred_prop_b_co_po)

tr<-R_tr_co_po-R_co_po
co<-R_tr_co_po-R_tr_po
po<-R_tr_co_po-R_tr_co
tr_co<-R_tr_co_po-R_po-tr-co
tr_po<-R_tr_co_po-R_co-tr-po
co_po<-R_tr_co_po-R_tr-co-po
tr_co_po<-R_tr_co_po-tr-co-po-tr_co-tr_po-co_po

(tr/R_tr_co_po)*100
(co/R_tr_co_po)*100
(po/R_tr_co_po)*100
(tr_co/R_tr_co_po)*100
(tr_po/R_tr_co_po)*100
(co_po/R_tr_co_po)*100
(tr_co_po/R_tr_co_po)*100

tr+co+po+tr_co+tr_po+co_po+tr_co_po

#Varpart using model2b and traits, environmental context, community context
model2pred_prop_b
#No population in the model!

#tr=traits
#ec=environmental context
#cc=community context

model2pred_prop_b_tr_ec_cc<-glm(prop_pred~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+z.n_redants+z.dist_closest_redants+z.avg_d_min_ja,family="binomial")
model2pred_prop_b_tr<-glm(prop_pred~z.shoot_h+z.most_adv+z.n_fl_corrected,family="binomial")
model2pred_prop_b_ec<-glm(prop_pred~z.veg_h_mean+z.avg_d_min_ja,family="binomial")
model2pred_prop_b_cc<-glm(prop_pred~z.n_redants+z.dist_closest_redants,family="binomial")
model2pred_prop_b_tr_ec<-glm(prop_pred~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+z.avg_d_min_ja,family="binomial")
model2pred_prop_b_tr_cc<-glm(prop_pred~z.shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.dist_closest_redants,family="binomial")
model2pred_prop_b_ec_cc<-glm(prop_pred~z.veg_h_mean+z.n_redants+z.dist_closest_redants+z.avg_d_min_ja+population,family="binomial")

R_tr_ec_cc<-r.squaredLR(model2pred_prop_b_tr_ec_cc)
R_tr<-r.squaredLR(model2pred_prop_b_tr)
R_ec<-r.squaredLR(model2pred_prop_b_ec)
R_cc<-r.squaredLR(model2pred_prop_b_cc)
R_tr_ec<-r.squaredLR(model2pred_prop_b_tr_ec)
R_tr_cc<-r.squaredLR(model2pred_prop_b_tr_cc)
R_ec_cc<-r.squaredLR(model2pred_prop_b_ec_cc)

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

#Varpart using model2c and traits, env. context, comm. context-->USE!

model2pred_prop_c

#tr=traits
#ec=environmental context
#cc=community context

model2pred_prop_c_tr_ec_cc<-glmer(prop_pred~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+z.n_redants+z.dist_closest_redants+z.avg_d_min_ja+(1|population),family="binomial")
model2pred_prop_c_tr<-glmer(prop_pred~z.shoot_h+z.most_adv+z.n_fl_corrected+(1|population),family="binomial")
model2pred_prop_c_ec<-glmer(prop_pred~z.veg_h_mean+z.avg_d_min_ja+(1|population),family="binomial")
model2pred_prop_c_cc<-glmer(prop_pred~z.n_redants+z.dist_closest_redants+(1|population),family="binomial")
model2pred_prop_c_tr_ec<-glmer(prop_pred~z.shoot_h+z.veg_h_mean+z.most_adv+z.n_fl_corrected+z.avg_d_min_ja+(1|population),family="binomial")
model2pred_prop_c_tr_cc<-glmer(prop_pred~z.shoot_h+z.most_adv+z.n_fl_corrected+z.n_redants+z.dist_closest_redants+(1|population),family="binomial")
model2pred_prop_c_ec_cc<-glmer(prop_pred~z.veg_h_mean+z.n_redants+z.dist_closest_redants+z.avg_d_min_ja+(1|population),family="binomial")

R_tr_ec_cc<-r.squaredLR(model2pred_prop_c_tr_ec_cc, null.RE = TRUE)
R_tr<-r.squaredLR(model2pred_prop_c_tr, null.RE = TRUE)
R_ec<-r.squaredLR(model2pred_prop_c_ec, null.RE = TRUE)
R_cc<-r.squaredLR(model2pred_prop_c_cc, null.RE = TRUE)
R_tr_ec<-r.squaredLR(model2pred_prop_c_tr_ec, null.RE = TRUE)
R_tr_cc<-r.squaredLR(model2pred_prop_c_tr_cc, null.RE = TRUE)
R_ec_cc<-r.squaredLR(model2pred_prop_c_ec_cc, null.RE = TRUE)

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



