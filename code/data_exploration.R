#####################################################################################
######################## Data exploration (without seed data) #######################
#####################################################################################

library(vcd)
library(MASS)
library(PerformanceAnalytics)

#Reading data from ~100 marked plants per population (3 populations)
data1<-read.table("./data/clean/datafile1.txt",header=T,sep="\t",dec=",")
head(data1)
tail(data1)
str(data1)

#Construct new variable: attack
data1$attack<-as.factor(with(data1,ifelse(n_eggs_max>0,"1","0")))

summary(data1)
names(data1)

#Correlations

head(data1[c(4,7,18,21:27,31)])
data1corr<-data1[c(4,7,18,21:27,31)]

pdf("./results/figures/correlation_chart_data1.pdf", family="Times")
chart.Correlation(data1corr)
dev.off()

head(data1[c(3:9,17:29,31:34)])
data1corr<-data1[c(3:9,17:29,31:34)]
cortable<-round(cor(data1corr,use="pairwise.complete.obs" ),2)
cortable[upper.tri(cortable)]<-""
cortable<-as.data.frame(cortable)
cortable


#Boxplots
for (i in 3:ncol(data1)){boxplot(as.numeric(data1[,i]),main=paste(colnames(data1)[i]))}

#Histograms
for (i in 3:ncol(data1)){hist(as.numeric(data1[,i]),main=paste(colnames(data1)[i]))}

############################################################################

#Differences among populations
#Histograms
par(mfrow=c(1,3),
    oma = c(5,4,1,1) + 0.1,
    mar = c(3,3,2,2) + 0.1)
pdf("./results/figures/diffs_pops_data1.pdf", family="Times")
for (i in c(3:9,16:18,21:27,31:39)){plot(data1$population,data1[,i],main=paste(colnames(data1)[i]))}
dev.off()

for (i in 3:9){print(summary(aov(as.numeric(data1[,i])~data1$population)))}
for (i in 16:ncol(data1)){print(summary(aov(as.numeric(data1[,i])~data1$population)))}

for (i in 3:9){print(TukeyHSD(aov(as.numeric(data1[,i])~population)))}
for (i in 16:ncol(data1)){print(TukeyHSD(aov(as.numeric(data1[,i])~population)))}

