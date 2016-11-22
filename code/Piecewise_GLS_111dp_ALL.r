#111 siter, inga urbana siter, inga med iberiskt lo, 2 med golden jackal, 79 med lo och 64 med varg
#Tot xx med rovdjur, xx med lynx och xx med canis

nourban <- read.delim("GLS_redfox_111dp.txt")
attach(nourban)
names(nourban)
nrow(nourban)
fix(nourban)

snow <- subset(nourban, Method=="Snowtracking")
nrow(snow) #84
dens <- subset(nourban, Method=="Dens")
nrow(dens) #7  
spot <- subset(nourban, Method=="Spotlight")  
nrow(spot) #15 
other <- subset(nourban, Method=="Other")
nrow(other) #5

PP <- subset(nourban, Pred_factor==1)
nrow(PP) #82
snowPP <- subset(PP, Method=="Snowtracking")
nrow(snowPP) #80

PA <- subset(nourban, Pred_factor==0)
snowPA <- subset(LA, Method=="Snowtracking")
nrow(snowPA) #6
nrow(PA) #29

CP <- subset(nourban, Wolf_factor==1)
snowCP <- subset(CP, Method=="Snowtracking")
nrow(snowCP) #62
nrow(CP) #64


quantile(nourban$Abundance)
#   0%    25%    50%    75%   100% 
#0.0000 0.0295 0.1100 0.4350 4.3000 
  
  
#Colinearity

library(ltm)
matrix_cor <- subset(nourban, select =c(34, 71, 83))
rcor.test(matrix_cor, method = "pearson")

matrix_cor2 <- subset(nourban, select =c(50, 51, 70))
rcor.test(matrix_cor2, method = "pearson")  

###Generalized least square model (GLS) enl R-boken###

library(nlme)

##One way analysis of variance using all explanatory variable   ALL

method_all <- nourban$Method
fox_all <- nourban$Abundance
winter_all <- nourban$winter_factors
human_all <- nourban$Hum_dens
temp_all <- Temp_summer
prod_all <- nourban$Prod_year
tree_all <- nourban$Treecover
canis_all <- nourban$Wolf_factor
lynx_all <- nourban$Lynx_factor
lat_all <- nourban$Lat
long_all <- nourban$Long

#(confint(z) ger konfidensintervall för coefficienterna)

#Effekt på räv inklusive metod

names(nourban)

library(nlme)


#Using GLS model to introduce spatial covariance between yields from locations that are close together:
#Introduction of spatial covariance between abundance at locations that are close together

#Use this to fit a model using GLS which allows the errors to be correlated and have unequal variances
#and to have unequal variances:

#Är metod viktigt?
m_all_fox <- formula(fox_all ~ winter_all + temp_all + human_all + prod_all + tree_all + lynx_all + canis_all + method_all, data = nourban)
m1_all_fox <- gls(m_all_fox, data = nourban)
plot(m1_all_fox) #Homoskedasticitet
   
log_fox_all <- log(Abundance+0.0001)
m_all_fox <- formula(log_fox_all ~ winter_all + temp_all + human_all + prod_all + tree_all + lynx_all + canis_all + method_all, data = nourban)
m1_all_fox <- gls(m_all_fox, data = nourban)
plot(m1_all_fox)
summary(m1_all_fox)
#Metod ej sign (den counts är ej med)

log_fox_all <- log(Abundance+0.0001)
m_all_fox <- formula(log_fox_all ~ winter_all + temp_all + human_all + prod_all + tree_all + lynx_all + canis_all|method_all, data = nourban)
m1_all_fox <- gls(m_all_fox, data = nourban)
plot(m1_all_fox)
summary(m1_all_fox)
#Metod ej sign (den counts är ej med)

log_fox_all <- log(Abundance+0.0001)
m_all_fox <- formula(log_fox_all ~ winter_all + temp_all + human_all + prod_all + tree_all + method_all, data = nourban)
m1_all_fox <- gls(m_all_fox, data = nourban)
plot(m1_all_fox)
summary(m1_all_fox)
#Metod ej sign (den counts är ej med)

log_fox_all <- log(Abundance+0.0001)
m_all_fox <- formula(log_fox_all ~ winter_all + temp_all + human_all + prod_all + tree_all|method_all, data = nourban)
m1_all_fox <- gls(m_all_fox, data = nourban)
plot(m1_all_fox)
summary(m1_all_fox)
#Metod ej sign (den counts är ej med)

model2 <- aov(log_fox_all ~ method_all + winter_all + temp_all + human_all + prod_all + tree_all + lynx_all + canis_all + Error(method_all))
summary(model2)
#Winter och tree signifikant men inte lo, motsvarar gls utan korrelationsstruktur

model2 <- aov(log_fox_all ~ method_all + winter_all + temp_all + human_all + prod_all + tree_all + Error(method_all))
summary(model2)
#Winter och tree signifikant, trend i human


###GLS-MODELLER ###

m_all_fox <- formula(fox_all ~ winter_all + temp_all + human_all + prod_all + tree_all + lynx_all + canis_all, data = nourban)
m1_all_fox <- gls(m_all_fox, data = nourban)
plot(m1_all_fox) #Homoskedasticitet
   
log_fox_all <- log(Abundance+0.0001)
m_all_fox <- formula(log_fox_all ~ winter_all + temp_all + human_all + prod_all + tree_all + lynx_all + canis_all, data = nourban)
m1_all_fox <- gls(m_all_fox, data = nourban)
summary(m1_all_fox)
#Winter signifikant, trend i tree
plot(m1_all_fox)
# Plotten ser bra ut
windows()
plot(Variogram(m1_all_fox, form=~lat_all+long_all))

m2_all_fox <- gls(m_all_fox, corr=corSpher(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m2_all_fox)
#Winter signifikant, trend i lo
#(Om man kör gls utan lo och canis så blir denna modell bäst och efter modellreducering är vinter och tree signifikanta)

m3_all_fox <- gls(m_all_fox, correlation=corLin(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
m3_all_fox <- gls(m_all_fox, correlation=corLin(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
#Error in gls(model = m_all_fox, data = nourban, correlation = corLin(c(3,  :  false convergence (8)

m4_all_fox <- gls(m_all_fox, correlation=corRatio(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m4_all_fox)
#Winter signifikant, nästan trend i lo

m5_all_fox <- gls(m_all_fox, correlation=corGaus(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m5_all_fox)
#Winter signifikant, trend i lo

m6_all_fox <- gls(m_all_fox, correlation=corExp(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6_all_fox)
#Winter signifikant

AIC(m1_all_fox, m2_all_fox, m4_all_fox, m5_all_fox, m6_all_fox)
#Model 5 är bäst (lägst AIC-värde)
anova(m1_all_fox, m5_all_fox) #m5 är signifikant bättre
BIC(m1_all_fox, m2_all_fox, m4_all_fox, m5_all_fox, m6_all_fox)

Vario_m5_all_fox <- Variogram(m5_all_fox, form = ~lat_all+long_all, nugget=T, robust = TRUE, resType = "pearson")
plot(Vario_m5_all_fox, smooth = TRUE)

Vario_m5_all_fox <- Variogram(m5_all_fox, form = ~lat_all+long_all, nugget=T, robust = TRUE, resType = "normalized")
windows()
plot(Vario_m5_all_fox, smooth = FALSE)


#Effekt på räv, modellreducering

log_fox_all <- log(Abundance+0.0001)

m_all_fox <- formula(log_fox_all ~ winter_all + temp_all + human_all + prod_all + tree_all + lynx_all + canis_all, data = nourban)
m5_all_fox <- gls(m_all_fox, correlation=corGaus(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m5_all_fox)
#Winter signifikant, trend i lo
#Temp minst signifikant

mb_all_fox <- formula(log_fox_all ~ winter_all + human_all + prod_all + tree_all + lynx_all + canis_all, data = nourban)
m5b_all_fox <- gls(mb_all_fox, correlation=corGaus(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m5b_all_fox)
#Winter signifikant, trend i lo
#Canis minst sign

mc_all_fox <- formula(log_fox_all ~ winter_all + human_all + prod_all + tree_all + lynx_all, data = nourban)
m5c_all_fox <- gls(mc_all_fox, correlation=corGaus(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m5c_all_fox)
#Winter signifikant, trend i lo
#Tree minst sign

md_all_fox <- formula(log_fox_all ~ winter_all + human_all + prod_all + lynx_all, data = nourban)
m5d_all_fox <- gls(md_all_fox, correlation=corGaus(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m5d_all_fox)
#Winter och lo signifikant
#Human minst sign

me_all_fox <- formula(log_fox_all ~ winter_all + prod_all + lynx_all, data = nourban)
m5e_all_fox <- gls(me_all_fox, correlation=corGaus(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m5e_all_fox)
#Winter och lo signifikant
#Prod minst sign

mf_all_fox <- formula(log_fox_all ~ winter_all + lynx_all, data = nourban)
m5f_all_fox <- gls(mf_all_fox, correlation=corGaus(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m5f_all_fox)
#Winter och lo signifikant

#BIC minskade för varje steg, vilket det ska göra
#(Om BIC åter ökar så ska man stanna vid modellen med lägst BIC-värde)

sd(winter_all)
sd(lynx_all)
sd(fox_all)

#Standardiserad koefficient winter = Standardiserad coefficient = Raw coeff winter*(sd winter/sd fox)
-0.6245003*(1.603071/0.6985205)
# Standardiserad koefficient = -1.433198

#Standardiserad koefficient lynx = Standardiserad coefficient = Raw coeff lynx*(sd lynx/sd fox)
-1.1779852*(0.4550202/0.6985205)
# Standardiserad koefficient = -0.7673462


#Structural/residual error

sumres2=sum(resid(m5f_all_fox)*resid(m5f_all_fox))
obs=fitted(m5f_all_fox)+resid(m5f_all_fox)
sumobs=sum(obs)
obs2=obs*obs
sumobs2=sum(obs2)
ss=sumobs2-(sumobs*sumobs/111)
d2=(ss-sumres2)/ss
d2
#d2 = 0.4332151  (motsvarar R2)
1-0.4332151    #Residual error
#Residual error = 0.5667849


#Effekt på lo

m1_all_lynx <- glm(lynx_all ~ winter_all + temp_all + human_all + prod_all + tree_all, family = binomial(link = "logit"))
m1_all_canis <- glm(canis_all ~ winter_all + temp_all + human_all + prod_all + tree_all, family = binomial(link = "logit"))

res_lynx <- resid(m1_all_lynx)
res_canis <- resid(m1_all_canis)

library(ape) #För enkle instr för Moran's I se http://www.ats.ucla.edu/stat/r/faq/morans_i.htm)
space_dist_lynx <- as.matrix(dist(cbind(Lat, Long))) #Distansmatris
space_dist_inv_lynx <- 1/space_dist_lynx   #Invers distansmatris
diag(space_dist_inv_lynx) <- 0  #Ersätter diagonala "entries" med noll
space_dist_inv_lynx[1:5, 1:5]   #Visar första delen av matrisen
#Har skapat en matris där varje "off-diagonal entry" [i, j] i matrisen är lika med 1/(distans mellan punkt i och punkt j).
Moran.I(res_lynx, space_dist_inv_lynx) 
#$observed
#[1] 0.1404917
#$expected
#[1] -0.009090909
#$sd
#[1] 0.02929581
#$p.value
#[1] 3.291543e-07
#Dvs sign autokorrelation för lo!

Moran.I(res_canis, space_dist_inv_lynx) 
#$observed
#[1] 0.1874763
#$expected
#[1] -0.009090909
#$sd
#[1] 0.0300648
#$p.value
#[1] 6.229817e-11
#Dvs sign autokorrelation för canis!

#Autokorrelation beror egentligen på att man utelämnat (en massa)
#förklarandevariabler. Detta är alltså fallet för både varg och lo och genom att
#sätta korrelerade errors mellan lynx och canis anger vi att båda dessa påverkas
#likartat av förklarandevariabler som inte finns med i modellen.

m2_all_lynx <- glm(lynx_all ~ winter_all + temp_all + human_all + prod_all + tree_all + res_canis, family = binomial(link = "logit"))
summary(m2_all_lynx)
#Winter, tree och canis signifikant

#Effekt på lo, modellreducering

m2_all_lynx <- glm(lynx_all ~ winter_all + temp_all + human_all + prod_all + tree_all + res_canis, family = binomial(link = "logit"))
summary(m2_all_lynx)
#Winter, tree och canis signifikant
#Temp minst signifikant

m2b_all_lynx <- glm(lynx_all ~ winter_all + human_all + prod_all + tree_all + res_canis, family = binomial(link = "logit"))
summary(m2b_all_lynx)
#Winter, tree och canis signifikant
#Human minst signifikant

m2c_all_lynx <- glm(lynx_all ~ winter_all + prod_all + tree_all + res_canis, family = binomial(link = "logit"))
summary(m2c_all_lynx)
#Winter, tree och canis signifikant
#Prod minst signifikant

m2d_all_lynx <- glm(lynx_all ~ winter_all + tree_all + res_canis, family = binomial(link = "logit"))
summary(m2d_all_lynx)
#Winter, tree och canis signifikant

BIC(m2_all_lynx, m2b_all_lynx, m2c_all_lynx, m2d_all_lynx)
#BIC minskar

sd(winter_all)
sd(tree_all)
sd(canis_all)
sd(lynx_all)

#Standardiserad koefficient winter = Standardiserad coefficient = Raw coeff winter*(sd winter/sd lynx)
2.03322*(1.603071/0.4550202)
# Standardiserad koefficient = 7.16319

#Standardiserad koefficient tree = Standardiserad coefficient = Raw coeff tree*(sd tree/sd lynx)
7.32719*(0.1943121/0.4550202)
# Standardiserad koefficient =  3.129008

#Standardiserad koefficient canis = Standardiserad coefficient = Raw coeff canis*(sd canis/sd lynx)
2.09808*(0.4963421/0.4550202)
# Standardiserad koefficient =   2.288614

res_canis
library(ltm)
lynx_canis_matrix <- cbind(res_lynx, res_canis)
rcor.test(lynx_canis_matrix)
#r = 0.433, p < 0.001

#Structural/residual error

#100 * (null deviance – residual deviance)/null deviance
(133.336 -  37.883)/133.336    #Explained deviance
#deviance explained = d2 =  0.7158832
1-0.7158832 #R2
#R2 = 0.2841168

library(ecodist)

mantelcoord <- subset(nourban, select=c(2:3))
space_edist <- distance(mantelcoord, "euclidean")#Skapar en matris med differensen mellan siterna, uträknat med: roten ur(((lat1 - lat2)i kvadrat) + ((long1 - long2)i kvadrat))
manteldata <- subset(nourban, select=c(4, 34, 69, 71, 82:83, 90:91))
names(manteldata)

winter_edist <- distance(manteldata$winter_factors, "euclidean") #Skapar en matris med differensen mellan winter factors
temp_edist <- distance(manteldata$Temp_summer, "euclidean")
human_edist <- distance(manteldata$Hum_dens, "euclidean")
tree_edist<- distance(manteldata$Treecover, "euclidean")
prod_y_edist <- distance(manteldata$Prod_year, "euclidean")
lynx_edist <- distance(manteldata$Lynx_factor, "euclidean")
canis_edist <- distance(manteldata$Wolf_factor, "euclidean")

#Lynx (Eurasian and Iberian)
mantel(lynx_edist~winter_edist+temp_edist+human_edist+prod_y_edist+tree_edist+canis_edist+space_edist, nperm=10000)
mantel(lynx_edist~temp_edist+human_edist+prod_y_edist+tree_edist+canis_edist+space_edist+winter_edist, nperm=10000)
mantel(lynx_edist~human_edist+prod_y_edist+tree_edist+canis_edist+space_edist+winter_edist+temp_edist, nperm=10000)
mantel(lynx_edist~prod_y_edist+tree_edist+canis_edist+space_edist+winter_edist+temp_edist+human_edist, nperm=10000)
mantel(lynx_edist~tree_edist+canis_edist+space_edist+winter_edist+temp_edist+human_edist+prod_y_edist, nperm=10000)
mantel(lynx_edist~canis_edist+space_edist+winter_edist+temp_edist+human_edist+prod_y_edist+tree_edist, nperm=10000)
#Bonferroni-korrektion: p/n = 0,05/6 = 0,008333
#Winter, tree och canis signifikant

#En jämförelse av r-värden när space tas bort visar att det inte tycks göra skillnad med och utan space:


#Därför görs ett korrelogram:

lynx_pmgram <-pmgram(lynx_edist, space_edist, nperm=10000)

par(mfrow=c(2,1))
plot(lynx_pmgram, main="Lynx presence/absence")

#Korrelogrammet visar icke-linjäritet i space. Därför görs en piecewise Mantel, dvs Manteltest på residualerna

winter_pmgram <-pmgram(winter_edist, space_edist, nperm=10000)    #Funktionen pmgram beräknar enkla och multivariata partiella korrelogram
temp_pmgram <-pmgram(temp_edist, space_edist, nperm=10000)
human_pmgram <-pmgram(human_edist, space_edist, nperm=10000)
tree_pmgram <-pmgram(tree_edist, space_edist, nperm=10000)
prod_y_pmgram <-pmgram(prod_y_edist, space_edist, nperm=10000)
lynx_pmgram <-pmgram(lynx_edist, space_edist, nperm=10000)
canis_pmgram <-pmgram(canis_edist, space_edist, nperm=10000)
fox_pmgram <-pmgram(fox_edist, space_edist, nperm=10000)

#Piecewise residuals
winter_p_resids <- residuals(pmgram(winter_edist, space_edist, resids = TRUE))
temp_p_resids <- residuals(pmgram(temp_edist, space_edist, resids = TRUE))
human_p_resids <- residuals(pmgram(human_edist, space_edist, resids = TRUE))
tree_p_resids <- residuals(pmgram(tree_edist, space_edist, resids = TRUE))
prod_y_p_resids <- residuals(pmgram(prod_y_edist, space_edist, resids = TRUE))
lynx_p_resids <- residuals(pmgram(lynx_edist, space_edist, resids = TRUE))
canis_p_resids <- residuals(pmgram(canis_edist, space_edist, resids = TRUE))

combined.pmgram <-pmgram(lynx_edist, space_edist, partial = winter_edist, nperm=10000)
plot(combined.pmgram, main = "a. Composition vs forest given space", xlab = "Difference in Forest Cover")
lynx.presids <- residuals(pmgram(graze.bcdist, sitelocation.edist, resids = TRUE, nclass = 6))
mantel(lynx.presids ~ winter.presids, nperm = 10000, nboot = 0)[c("mantelr", "pval1")]
#Ähh, lyckas inte... nöja meig med partial utan piecewise?

mantel(lynx_p_resids~winter_p_resids + human_p_resids + tree_p_resids + prod_y_p_resids + canis_p_resids + temp_p_resids, nperm = 10000)
mantel(lynx_p_resids~temp_p_resids + winter_p_resids + human_p_resids + tree_p_resids + prod_y_p_resids + canis_p_resids, nperm = 10000)
mantel(lynx_p_resids~human_p_resids + tree_p_resids + prod_y_p_resids + canis_p_resids + temp_p_resids + winter_p_resids, nperm = 10000)
mantel(lynx_p_resids~prod_y_p_resids + canis_p_resids + temp_p_resids + winter_p_resids + human_p_resids + tree_p_resids, nperm = 10000)
mantel(lynx_p_resids~tree_p_resids + prod_y_p_resids + canis_p_resids + temp_p_resids + winter_p_resids + human_p_resids, nperm = 10000)
mantel(lynx_p_resids~canis_p_resids + temp_p_resids + winter_p_resids + human_p_resids + tree_p_resids + prod_y_p_resids, nperm = 10000)
#Bonferroni-korrektion: p/n = 0,05/6 = 0,008333
#Winter, tree och canis signifikant
#Fortfarande ingen större skillnad i r-värden förutom för canis

 
#Effekt på varg

m1_all_lynx <- glm(lynx_all ~ winter_all + temp_all + human_all + prod_all + tree_all, family = binomial(link = "logit"))
m1_all_canis <- glm(canis_all ~ winter_all + temp_all + human_all + prod_all + tree_all, family = binomial(link = "logit"))

res_lynx <- resid(m1_all_lynx)
res_canis <- resid(m1_all_canis)

m2_all_canis <- glm(canis_all ~ winter_all + temp_all + human_all + prod_all + tree_all + res_lynx, family = binomial(link = "logit"))
summary(m2_all_canis)
#Winter, temp och lynx signifikant


#Effekt på canis, modellreducering

m2_all_canis <- glm(canis_all ~ winter_all + temp_all + human_all + prod_all + tree_all + res_lynx, family = binomial(link = "logit"))
summary(m2_all_canis)
#Winter, temp och lynx signifikant
#Prod minst signifikant

m2b_all_canis <- glm(canis_all ~ winter_all + temp_all + human_all + tree_all + res_lynx, family = binomial(link = "logit"))
summary(m2b_all_canis)
#Winter, temp och lynx signifikant
#Human minst signifikant

m2c_all_canis <- glm(canis_all ~ winter_all + temp_all + res_lynx, family = binomial(link = "logit"))
summary(m2c_all_canis)
#Winter, temp och lynx signifikant

BIC(m2_all_canis, m2b_all_canis, m2c_all_canis)
#BIC minskar

sd(winter_all)
sd(temp_all)
sd(lynx_all)
sd(canis_all)

#Standardiserad koefficient winter = Standardiserad coefficient = Raw coeff winter*(sd winter/sd canis)
3.0929*(1.603071/0.4963421)
# Standardiserad koefficient = 9.989357
#Ej sign enl Mantel

#Standardiserad koefficient temp = Standardiserad coefficient = Raw coeff tree*(sd temp/sd canis)
1.3289*(3.326597/0.4963421)
# Standardiserad koefficient =  8.906588
#Ej sign enl Mantel

#Standardiserad koefficient lynx = Standardiserad coefficient = Raw coeff lynx*(sd lynx/sd canis)
4.3240*(0.4550202/0.4963421)
# Standardiserad koefficient =  3.964015


#Structural/residual error

#100 * (null deviance – residual deviance)/null deviance
(151.265 -  55.425)/151.265    #Explained deviance
#d2 =  0.6335901
1 - 0.6335901  #R2
#R2 = 0.3664099

library(ecodist)
mantelcoord <- subset(nourban, select=c(2:3))
space_edist <- distance(mantelcoord, "euclidean")#Skapar en matris med differensen mellan siterna, uträknat med: roten ur(((lat1 - lat2)i kvadrat) + ((long1 - long2)i kvadrat))
manteldata <- subset(nourban, select=c(4, 34, 69, 71, 82:83, 90:91))
names(manteldata)

winter_edist <- distance(manteldata$winter_factors, "euclidean") #Skapar en matris med differensen mellan winter factors
temp_edist <- distance(manteldata$Temp_summer, "euclidean")
human_edist <- distance(manteldata$Hum_dens, "euclidean")
tree_edist<- distance(manteldata$Treecover, "euclidean")
prod_y_edist <- distance(manteldata$Prod_year, "euclidean")
lynx_edist <- distance(manteldata$Lynx_factor, "euclidean")
canis_edist <- distance(manteldata$Wolf_factor, "euclidean")

#Canis (Grey wolf and Golden jackal)
mantel(canis_edist~winter_edist+temp_edist+human_edist+prod_y_edist+tree_edist+lynx_edist+space_edist, nperm=10000)
mantel(canis_edist~temp_edist+human_edist+prod_y_edist+tree_edist+lynx_edist+space_edist+winter_edist, nperm=10000)
mantel(canis_edist~human_edist+prod_y_edist+tree_edist+lynx_edist+space_edist+winter_edist+temp_edist, nperm=10000)
mantel(canis_edist~prod_y_edist+tree_edist+lynx_edist+space_edist+winter_edist+temp_edist+human_edist, nperm=10000)
mantel(canis_edist~tree_edist+lynx_edist+space_edist+winter_edist+temp_edist+human_edist+prod_y_edist, nperm=10000)
mantel(canis_edist~lynx_edist+space_edist+winter_edist+temp_edist+human_edist+prod_y_edist+tree_edist, nperm=10000)

#Bonferroni-korrektion: p/n = 0,05/6 = 0,008333
#Endast lynx signifikant  (även före Bonferronikorrektion)


#Effekt på tree cover

m_all_tree <- formula(tree_all ~ winter_all + temp_all + human_all + prod_all)
m1_all_tree <- gls(m_all_tree, data = nourban)
plot(m1_all_tree)
summary(m1_all_tree)
#Prod och winter signifikant
vario_m1_all_tree <- Variogram(m1_all_tree, form = ~lat_all+long_all, robust = TRUE, resType = "pearson")
plot(vario_m1_all_tree, smooth = TRUE)

m2_all_tree <- gls(m_all_tree, correlation = corSpher(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m2_all_tree)
#Winter, human och prod signifikant.

m3_all_tree <- gls(m_all_tree, correlation = corLin(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
#Error in gls(m1_all_tree, correlation = corLin(form = ~lat_all + long_all,  :  false convergence (8)
m3_all_treeB <- gls(m_all_tree, corr=corLin(c(10, 0.75), form=~lat_all+long_all, nugget=T), data=nourban) 
#Error in gls(m1_all_tree, correlation = corLin(form = ~lat_all + long_all,  :  false convergence (8)  

m4_all_tree <- gls(m_all_tree, correlation = corRatio(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m4_all_tree)
#Winter, human och prod signifikant.

m5_all_tree <- gls(m_all_tree, correlation = corGaus(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m5_all_tree)
#Winter, human och prod signifikant.

m6_all_tree <- gls(m_all_tree, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6_all_tree)
#Winter, human och prod signifikant.

AIC(m1_all_tree, m2_all_tree, m4_all_tree, m5_all_tree, m6_all_tree)
#m6_all_tree har det lägsta AIC-värdet och är alltså den bästa modellen, dvs det förbättrar modellen att lägga till en spatial korrelationsstruktur   
anova(m1_all_tree, m6_all_tree) #m6_all_tree är signifikant bättre

Vario_m6_all_tree <- Variogram(m6_all_tree, form = ~lat_all+long_all, nugget=T, robust = TRUE, resType = "pearson")
plot(Vario_m6_all_tree, smooth = FALSE)
#Assymptot. Autokorr på korta avstånd (ca som lo men ltie bättre)

Vario_m6_all_tree <- Variogram(m6_all_tree, form = ~lat_all+long_all, nugget=T, robust = TRUE, resType = "normalized")
windows()
plot(Vario_m6_all_tree, smooth = FALSE)
#Punkterna ska forma ett horisontellt band, vilket indikerar spatialt oberoende. Det gör de nästan.


#Effekt på tree cover, modellreducering

m_all_tree <- formula(tree_all ~ winter_all + temp_all + human_all + prod_all)
m6_all_tree <- gls(m_all_tree, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6_all_tree)
#Winter, human och prod signifikant.
#Temp minst signifikant

mb_all_tree <- formula(tree_all ~ winter_all + human_all + prod_all)
m6b_all_tree <- gls(mb_all_tree, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6b_all_tree)
#Winter, human och prod signifikant.

#BIC minskade

sd(winter_all)
sd(human_all)
sd(prod_all)
sd(tree_all)

#Standardiserad koefficient winter = Standardiserad coefficient = Raw coeff winter*(sd winter/sd tree)
 0.0583109*(1.603071/ 0.1943121)
# Standardiserad koefficient = 0.4810638

#Standardiserad koefficient human = Standardiserad coefficient = Raw coeff winter*(sd human/sd tree)
 0.2440212*(0.1194409/0.1943121)
# Standardiserad koefficient = 0.1499964

#Standardiserad koefficient prod = Standardiserad coefficient = Raw coeff winter*(sd prod/sd tree)
 1.3563781*(0.08355484/0.1943121)
# Standardiserad koefficient = 0.583247


#Structural/residual error

sumres2=sum(resid(m6b_all_tree)*resid(m6b_all_tree))
obs=fitted(m6b_all_tree)+resid(m6b_all_tree)
sumobs=sum(obs)
obs2=obs*obs
sumobs2=sum(obs2)
ss=sumobs2-(sumobs*sumobs/111)
d2=(ss-sumres2)/ss
d2
#d2 = 0.3740454
1-0.3740454 #Residual error
#Residual error = 0.6259546


#Effekt på produktivitet

m_all_prod <- formula(prod_all ~ winter_all + temp_all + human_all)
m1_all_prod <- gls(m_all_prod, data = nourban)
plot(m1_all_prod)
summary(m1_all_prod)
#Winter och human signifikant, nästan trend i temp
vario_m1_all_prod <- Variogram(m1_all_prod, form = ~lat_all+long_all, robust = TRUE, resType = "pearson")
plot(vario_m1_all_prod, smooth = TRUE)
#Liknande mönster som för räv men inte lika extremt

m2_all_prod <- gls(m_all_prod, correlation = corSpher(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m2_all_prod)
#Winter och human signifikant

m3_all_prod <- gls(m_all_prod, correlation = corLin(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
# Error in gls(f1_prod, corr = corLin(c(18, 0.65), form = ~Lat_all + Long_all,  :  false convergence (8)
m3_all_prodB <- gls(m_all_prod, corr=corLin(c(16, 0.63), form=~lat_all+long_all, nugget=T), data=nourban)  
# Error in gls(f1_prod, corr = corLin(c(18, 0.65), form = ~Lat_all + Long_all,  :  false convergence (8)    

m4_all_prod <- gls(m_all_prod, correlation = corRatio(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m4_all_prod)
#Winter och human signifikant

m5_all_prod <- gls(m_all_prod, correlation = corGaus(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m5_all_prod)
#Winter och human signifikant, trend i temp

m6_all_prod <- gls(m_all_prod, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6_all_prod)
#Winter och human signifikant

AIC(m1_all_prod, m2_all_prod, m4_all_prod, m5_all_prod, m6_all_prod)
#m6_all_prod har det lägsta AIC-värdet och är alltså den bästa modellen, dvs det förbättrar modellen att lägga till en spatial korrelationsstruktur
anova(m1_all_prod, m6_all_prod) #m6_all_prod är signifikant bättre

Vario_m6_all_prod <- Variogram(m6_all_prod, form = ~lat_all+long_all, nugget=T, robust = TRUE, resType = "pearson")
plot(Vario_m6_all_prod, smooth = FALSE)
#Assymptot! Autokorr på väldigt korta avstånd (som lo men bättre)

Vario_m6_all_prod <- Variogram(m6_all_prod, form = ~lat_all+long_all, nugget=T, robust = TRUE, resType = "normalized")
windows()
plot(Vario_m6_all_prod, smooth = FALSE)
#Punkterna ska forma ett horisontellt band, vilket indikerar spatialt oberoende. Det gör inte riktigt.


#Effekt på produktivitet, modellreducering

m_all_prod <- formula(prod_all ~ winter_all + temp_all + human_all)
m6_all_prod <- gls(m_all_prod, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6_all_prod)
#Winter och human signifikant
#Temp minst signifikant

mb_all_prod <- formula(prod_all ~ winter_all + human_all)
m6b_all_prod <- gls(mb_all_prod, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6b_all_prod)
#Winter och human signifikant

BIC(m6_all_prod, m6b_all_prod)
#BIC minskar

sd(winter_all)
sd(human_all)
sd(prod_all)

#Standardiserad koefficient winter = Standardiserad coefficient = Raw coeff winter*(sd winter/sd prod)
 -0.0298256*(1.603071/0.08355484)
# Standardiserad koefficient = -0.5722296

#Standardiserad koefficient human = Standardiserad coefficient = Raw coeff human*(sd human/sd prod)
 -0.1987119*(0.1194409/0.08355484)
# Standardiserad koefficient = -0.2840569


#Structural/residual error

sumres2=sum(resid(m6b_all_prod)*resid(m6b_all_prod))
obs=fitted(m6b_all_prod)+resid(m6b_all_prod)
sumobs=sum(obs)
obs2=obs*obs
sumobs2=sum(obs2)
ss=sumobs2-(sumobs*sumobs/111)
d2=(ss-sumres2)/ss
d2
#d2 = 0.2200399
1-0.2200399 #Residual error
#Residual error = 0.7799601


#Effekt på human

m_all_human <- formula(human_all ~ winter_all + temp_all)
m1_all_human <- gls(m_all_human, data = nourban)
plot(m1_all_human)
#Winter signifikant
vario_m1_all_human <- Variogram(m1_all_human, form = ~lat_all+long_all, robust = TRUE, resType = "pearson")
plot(vario_m1_all_human, smooth = TRUE)

log_human_all <- log(human_all)
m_all_human <- formula(log_human_all ~ winter_all + temp_all)
m1_all_human <- gls(m_all_human, data = nourban)
plot(m1_all_human)
summary(m1_all_human)
#Winter signifikant
vario_m1_all_human <- Variogram(m1_all_human, form = ~lat_all+long_all, robust = TRUE, resType = "pearson")
plot(vario_m1_all_human, smooth = TRUE)

m2_all_human <- gls(m_all_human, correlation = corSpher(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m2_all_human)
#Winter signifikant

m3_all_human <- gls(m_all_human, correlation = corLin(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m3_all_human)
#Vinter signifikant

m4_all_human <- gls(m_all_human, correlation = corRatio(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m4_all_human)
#Winter signifikant

m5_all_human <- gls(m_all_human, correlation = corGaus(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m5_all_human)
#Winter signifikant

m6_all_human <- gls(m_all_human, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6_all_human)
#Inget signifikant

AIC(m1_all_human, m2_all_human, m3_all_human, m4_all_human, m5_all_human, m6_all_human)
#m4_all_human har det lägsta AIC-värdet och är alltså den bästa modellen, dvs det förbättrar modellen att lägga till en spatial korrelationsstruktur
anova(m1_all_human, m4_all_human) #m4_all_human är signifikant bättre

Vario_m4_all_human <- Variogram(m4_all_human, form = ~lat_all+long_all, nugget=T, robust = TRUE, resType = "pearson")
plot(Vario_m4_all_human, smooth = FALSE)

Vario_m4_all_human <- Variogram(m4_all_human, form = ~lat_all+long_all, nugget=T, robust = TRUE, resType = "normalized")
windows()
plot(Vario_m4_all_human, smooth = FALSE)
#Fortf stark autokorrelation


#Effekt på human, modellreducering

log_human_all <- log(human_all)
m_all_human <- formula(log_human_all ~ winter_all + temp_all)
m4_all_human <- gls(m_all_human, correlation = corRatio(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m4_all_human)
#Winter signifikant
#Temp minst signifikant

mb_all_human <- formula(log_human_all ~ winter_all)
m4b_all_human <- gls(mb_all_human, correlation = corRatio(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m4b_all_human)
#Winter signifikant    

#BIC minskar    

sd(winter_all)
sd(human_all)

#Standardiserad koefficient winter = Standardiserad coefficient = Raw coeff winter*(sd winter/sd human)
 -0.863400*(1.603071/0.1194409)
# Standardiserad koefficient = -11.58809


#Structural/residual error

sumres2=sum(resid(m4b_all_human)*resid(m4b_all_human))
obs=fitted(m4b_all_human)+resid(m4b_all_human)
sumobs=sum(obs)
obs2=obs*obs
sumobs2=sum(obs2)
ss=sumobs2-(sumobs*sumobs/111)
d2=(ss-sumres2)/ss
d2
#d2 = 0.2392853
1-0.2392853 #Reidual error
#Residual error = 0.7607147


#Effekt på sommartemperatur

library(ape) #För enkle instr för Moran's I se http://www.ats.ucla.edu/stat/r/faq/morans_i.htm)
temp_dist <- as.matrix(dist(cbind(lat_all, long_all))) #Distansmatris
temp_dist_inv <- 1/temp_dist   #Invers distansmatris
diag(temp_dist_inv) <- 0  #Ersätter diagonala "entries" med noll
temp_dist_inv[1:5, 1:5]   #Visar första delen av matrisen
#Har skapat en matris där varje "off-diagonal entry" [i, j] i matrisen är lika med 1/(distans mellan punkt i och punkt j).

Moran.I(temp_all, temp_dist_inv) 
#$observed
#[1] 0.2547141
#$expected
#[1] -0.009090909
#$sd
#[1] 0.03012858
#$p.value
#[1] 0
#Dvs sign positiv autokorr

#Effective sample size = N´ = N-((1-r1)/(N+r1)) = 111((1-0.2547141)/(1+0.2547141)) 
111*((1-0.2547141)/(1+0.2547141))
# N´ = 65.93274
#df för enkel korr : N-2 = 111-2 = 109
#df för enkel korr : N´-2 = 66-2 = 64

library(ltm)
winter_temp_matrix <- subset(nourban, select = c(68, 82))
rcor.test(winter_temp_matrix)
#r = -0.903, p < 0.001

hist(temp_all) #Lite skevt men ok
log_temp <- log(temp_all) 
hist(log_temp) #Blir inte bättre
lm_temp <- lm(winter_all~temp_all)
summary(lm_temp)
print(lm_temp)

#df för enkel korr : N-2 = 111-2 = 109

tempkorr <- lm(temp_all ~ winter_all)
summary(tempkorr)
res_temp <- resid(tempkorr)
Moran.I(res_temp, temp_dist_inv) 
#$observed
#[1] 0.3581451
#$expected
#[1] -0.009090909
#$sd
#[1] 0.03019655
#$p.value
#[1] 0
#Dvs sign positiv autokorr
#Effective sample size = N´ = N-((1-r1)/(N+r1)) = 111((1-0.2547141)/(1+0.2547141)) 
111*((1-0.3581451)/(1+0.3581451))
# N´ = 52.45823
#df för enkel korr : N-2 = 111-2 = 109
#df för enkel korr : N´-2 = 52-2 = 50


#Effekt på winter

library(ape) #För enkle instr för Moran's I se http://www.ats.ucla.edu/stat/r/faq/morans_i.htm)
winter_dist <- as.matrix(dist(cbind(lat_all, long_all))) #Distansmatris
winter_dist_inv <- 1/winter_dist   #Invers distansmatris
diag(winter_dist_inv) <- 0  #Ersätter diagonala "entries" med noll
winter_dist_inv[1:5, 1:5]   #Visar första delen av matrisen
#Har skapat en matris där varje "off-diagonal entry" [i, j] i matrisen är lika med 1/(distans mellan punkt i och punkt j).

Moran.I(winter_all, winter_dist_inv) 
#$observed
#[1] 0.3406877
#$expected
#[1] -0.009090909
#$sd
#[1] 0.03028621
#$p.value
#[1] 0
#Effective sample size = N´ = N-((1-r1)/(N+r1)) = 111((1-0.2547141)/(1+0.2547141)) 
111*((1-0.3406877)/(1+0.3406877))
# N´ = 54.58666
#df för enkel korr : N-2 = 111-2 = 109
#df för enkel korr : N´-2 = 55-2 = 53


library(ltm)
names(nourban)
winter_temp_matrix <- subset(nourban, select = c(68, 82))
winter_cor <- rcor.test(winter_temp_matrix)
#r = -0.903, p < 0.001

#df för enkel korr : N-2 = 111-2 = 109

winterkorr <- lm(winter_all ~ temp_all)
summary(winterkorr)
res_winter <- resid(winterkorr)
Moran.I(res_winter, winter_dist_inv)
#$observed
#[1] 0.4441186
#$expected
#[1] -0.009090909
#$sd
#[1] 0.03020785
#$p.value
#[1] 0
#Dvs sign positiv autokorr
#Effective sample size = N´ = N-((1-r1)/(N+r1)) = 111((1-0.2547141)/(1+0.2547141)) 
111*((1-0.4441186)/(1+0.4441186))
# N´ = 42.72699
#df för enkel korr : N-2 = 111-2 = 109
#df för enkel korr : N´-2 = 43-2 = 41 (40)

m_all_winter <- gls(winter_all ~ temp_all)
m_all_temp <- gls(temp_all ~ winter_all)
res_winter <- resid(m_all_winter)
res_temp <- resid(m_all_temp)

cor(res_temp, res_winter)
#[1] 0.5341015



##Piecewise model, all (111 dp)##

#Causal assumptions: Se ritad modell

library(nlme)

names(nourban)

method_all <- nourban$Method
fox_all <- nourban$Abundance
winter_all <- nourban$winter_factors
temp_all <- nourban$Temp_summer
human_all <- nourban$Hum_dens
product_all <- nourban$Prod_year
tree_all <- nourban$Treecover
canis_all <- nourban$Wolf_factor
lo_all <- nourban$Lynx_factor
lat_all <- nourban$Lat
long_all <- nourban$Long

m_all_winter <- gls(winter_all ~ temp_all)
m_all_temp <- gls(temp_all ~ winter_all)
res_winter <- resid(m_all_winter)
res_temp <- resid(m_all_temp)

cor(res_temp, res_winter)
#[1] 0.5341015

m1_all_winter <- formula(winter_all ~ res_temp)
m6_all_winter <- gls(m1_all_winter, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6_all_winter)
#Temp signifikant

m1_all_temp <- formula(temp_all ~ res_winter)
m4_all_temp <- gls(m1_all_temp, correlation = corRatio(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m4_all_temp)
#Vinter signifikant

log_human_all <- log(human_all)
mb_all_human <- formula(log_human_all ~ winter_all)
m4b_all_human <- gls(mb_all_human, correlation = corRatio(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m4b_all_human)
#Winter signifikant    

mb_all_prod <- formula(prod_all ~ winter_all + human_all)
m6b_all_prod <- gls(mb_all_prod, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6b_all_prod)
#Winter och human signifikant

mb_all_tree <- formula(tree_all ~ winter_all + human_all + prod_all)
m6b_all_tree <- gls(mb_all_tree, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6b_all_tree)
#Winter, human och prod signifikant.

m_all_lynx <- gls(fox_all ~ winter_all + temp_all + human_all + prod_all + tree_all)
m_all_canis <- gls(fox_all ~ winter_all + temp_all + human_all + prod_all + tree_all)
res_lynx <- resid(m_all_lynx)
res_canis <- resid(m_all_canis)

cor(res_lynx, res_canis)
# [1] 1

m1_all_canis <- formula(canis_all ~ winter_all + temp_all + human_all + prod_all + tree_all + res_lynx, data = nourban)
m6_all_canis <- gls(m1_all_canis, correlation = corExp(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m6_all_canis)
#Ingenting signifikant

m1_all_lynx <- formula(lynx_all ~ winter_all + temp_all + human_all + prod_all + tree_all + res_canis, data = nourban)
m4_all_lynx <- gls(m1_all_lynx, correlation = corRatio(form = ~lat_all+long_all, nugget=T), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(m4_all_lynx)
#Winter och tree signifikant

log_fox_all <- log(Abundance+0.0001)
mf_all_fox <- formula(log_fox_all ~ winter_all + lynx_all, data = nourban)
mfe_all_fox <- gls(mf_all_fox, correlation=corGaus(form=~lat_all+long_all, nugget = TRUE), data = nourban) #Olika korrelationsstrukturer (corr[correlation classes]) testas
summary(mfe_all_fox)

summary(m6_all_winter)
summary(m4_all_temp)
summary(m5b_all_human)
summary(m6b_all_prod)
summary(m6b_all_tree)
summary(m6_all_canis) ? 
summary(m4_all_lynx) ?
summary(m4b_all_fox)


###Ta bort icke signifikanta variabler



#winter[i] = dnorm(winter_pred[i], precision_winter)
#(winter_pred[i] = b(1.0) + b(1.1)*temp[i]
#winter_pred[i] =  0.5799400 +  -0.2495641*temp[i]

#temp[i] = dnorm(temp_pred[i], precision_temp)
#(temp_pred[i] = b(1.0) + b(1.1)*winter[i]
#temp_pred[i] =  14.382877 +  -1.486426*winter[i]

#Cor winter_res, temp_res = 0.5341015

#human[i] = dnorm(human_pred[i], precision_human)
#(human_pred[i] = b(1.0) + b(1.1)* winter[i]
#human_pred[i] =  0.05875808 +  -0.02156152*vinter[i]

#prod[i] = dnorm(prod_pred[i], precision_prod)
#(prod_pred[i] = b(1.0) + b(1.1)* vinter[i] + b(1.3)*human[i]
#prod_pred[i] =  0.3332266 + -0.0298256*vinter[i] + -0.1987119*human[i]

#tree[i] = dnorm(tree_pred[i], precision_tree)
#(tree_pred[i] = b(1.0) + b(1.1)* winter[i] + b(1.3)* human[i] + b(1.4)*prod[i]
#tree_pred[i] =  -0.1054002 + 0.0583109*winter[i] + 0.2440212*human[i] + 1.3563781*prod[i]

#canis[i] = dnorm(canis_pred[i], precision_canis)
#(canis_pred[i] = b(1.0) + b(1.1)* winter[i] + b(1.2)* temp[i] + b(1.3)* human[i] + b(1.4)*prod[i] + b(1.5)*tree[i] + b(1.5)*lynx[i]
#canis_pred[i] =  0.31519935 +  -0.03069017*winter[i] + -0.19095466*human[i] + ?*prod[i]  + ?*tree[i] + ?*lynx[i]

#lynx[i] = dnorm(lynx_pred[i], precision_lynx)
#(lynx_pred[i] = b(1.0) + b(1.1)* winter[i] + b(1.2)* temp[i] + b(1.3)* human[i] + b(1.4)*prod[i] + b(1.5)*tree[i] + b(1.5)*canis[i]
#lynx_pred[i] =  0.31519935 +  -0.03069017*winter[i] + -0.19095466*human[i] + ?*prod[i]  + ?*tree[i] + ?*canis[i]

#fox[i] = dnorm(fox_pred[i], precision_fox)
#(fox_pred[i] = b(1.0) + b(1.1)* winter[i]
#fox_pred[i] =  -1.9806383 +  -0.7378225*winter[i]

#fox[i] = dnorm(fox_pred[i], precision_fox)
#(fox_pred[i] = b(1.0) + b(1.1)* winter[i] + b(1.2)* temp[i] + b(1.3)* human[i] + b(1.4)*prod[i] + b(1.5)*tree[i] + b(1.5)*canis[i] + b(1.5)*lynx[i]
#fox_pred[i] =  0.31519935 +  -0.03069017*winter[i] + -0.19095466*human[i] + ?*prod[i]  + ?*tree[i] + ?*canis[i] + ?*lynx[i]



#osv

##Gör en ny modell som endast inkluderar de signifikanta sambanden:

#Modellerna för human och produktivitet är samma som ovan
#Ingen idé att köra förrän autokorrelation och dubbelpilar är klart


