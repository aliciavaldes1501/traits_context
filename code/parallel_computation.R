clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 3), type = clusterType))

clusterExport(clust, "data1comp")
clusterEvalQ(clust, library(lme4))

models2a1 <- pdredge(model2a1,cluster=clust)
