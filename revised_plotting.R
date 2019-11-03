setwd("~/Box Sync/knockoffs/code/results/1028/")
default_par = par()
par(mfrow = c(1,3), oma = c(2,2,1,1) + 0.1, mar = c(4,4.5,2,1) + 0.1)
col = c("blue","mediumpurple","red","gray10","orange", "forestgreen", "deeppink","seagreen","navyblue","magenta")
statistic = c("AIC", "BIC", "path", "cv", "mboost", "randomForest", "Bayesian", "Jonckheere", "uni_cumulative", "uni_sratio")
ylab = c("FDR (%)", "Type I Error Rate (%)", "Power (%)")
### 
### prior-to-control
method = c(rep("glmnetcr",3),"mboost")
stat_index = c(1,2,4,5)
ylim = c(100, 40, 100)
for (i in 1:3){
  plot(0, xlab = "Signal amplitude (A)", ylab = ylab[i], ylim = c(0,ylim[i]), xlim = c(0.2,1), pch = NA)
  for(j in 1:length(method1)){
    load(paste("result1028_", method[j], statistic[stat_index][j], ".RData", sep = "_"))
    res = sapply(results, rowMeans)[c(1,3,2),]
    lines(seq(0.2,1,0.2), res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
  }
  if (i==2) 
    legend("top", legend = statistic[stat_index], cex=1, col=col[stat_index], lwd = 2.5, lty=stat_index, bty = "n")
}
dev2bitmap("../../../manuscript/Submission to BIB/revised figures/Figure1.jpeg", type = "jpeg", 
           res = 600, height = 3, width=10)
### marginal
method = rep("marginal",3)
stat_index = 8:10
for (i in 1:3){
  plot(0, xlab = "Signal amplitude (A)", ylab = ylab[i], ylim = c(0,ylim[i]), xlim = c(0.2,1), pch = NA)
  for(j in 1:length(method2)){
    load(paste("result1028_", method[j], statistic[stat_index][j], ".RData", sep = "_"))
    res = sapply(results, rowMeans)[c(1,3,2),]
    lines(seq(0.2,1,0.2), res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
  }
  if (i==2) 
    legend("top", legend = statistic[stat_index], cex=1, col=col[stat_index], lwd = 2.5, lty=stat_index, bty = "n")
}
dev2bitmap("../../../manuscript/Submission to BIB/revised figures/Figure2.jpeg", type = "jpeg", 
           res = 600, height = 3, width=10)
### RDVS
method = rep("rdvs",5)
stat_index = c(1:4,6)
for (i in 1:3){
  plot(0, xlab = "Signal amplitude (A)", ylab = ylab[i], ylim = c(0,ylim[i]), xlim = c(0.2,1), pch = NA)
  for(j in 1:length(method)){
    load(paste("result1028_", method[j], statistic[stat_index][j], ".RData", sep = "_"))
    res = sapply(results, rowMeans)[c(1,3,2),]
    lines(seq(0.2,1,0.2), res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
  }
  if (i==2) 
    legend("top", legend = statistic[stat_index], cex=1, col=col[stat_index], lwd = 2.5, lty=stat_index, bty = "n")
}
dev2bitmap("../../../manuscript/Submission to BIB/revised figures/Figure3.jpeg", type = "jpeg", 
           res = 600, height = 3, width=10)
### knockoff
method = rep("knockoff",6)
stat_index = c(1:6)
for (i in 1:3){
  plot(0, xlab = "Signal amplitude (A)", ylab = ylab[i], ylim = c(0,ylim[i]), xlim = c(0.2,1), pch = NA)
  for(j in 1:length(method)){
    load(paste("result1028_", method[j], statistic[stat_index][j], ".RData", sep = "_"))
    res = sapply(results, rowMeans)[c(1,3,2),]
    lines(seq(0.2,1,0.2), res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
  }
  if (i==2) 
    legend("top", legend = statistic[stat_index], cex=1, col=col[stat_index], lwd = 2.5, lty=stat_index, bty = "n")
}
dev2bitmap("../../../manuscript/Submission to BIB/revised figures/Figure4.jpeg", type = "jpeg", 
           res = 600, height = 3, width=10)
