setwd("~/Box Sync/knockoffs/code/results/1028/")
default_par = par()
par(mfrow = c(2,3), oma = c(2,2,1,1) + 0.1, mar = c(5,4.5,2,1) + 0.1)
col = c("blue","mediumpurple","red","gray10","orange", "forestgreen", "deeppink","seagreen","navyblue","magenta")
statistic = c("AIC", "BIC", "path", "cv", "mboost", "randomForest", "Bayesian", "Jonckheere", "uni_cumulative", "uni_sratio")
ylab = c("FDR (%)", "Type I Error Rate (%)", "Power (%)")
### 
### prior-to-control
method = c(rep("glmnetcr",3),"mboost")
stat_index = c(1,2,4,5)
ylim = c(100, 40, 100)
for (r in 1:2){
  for (i in 1:3){
    plot(0, xlab = "Signal amplitude (A)", ylab = ylab[i], ylim = c(0,ylim[i]), xlim = c(0.2,1), pch = NA)
    for(j in 1:length(method)){
      if (r==2) {load(paste("result1028_", method[j], statistic[stat_index][j], ".RData", sep = "_"))
      } else {load(paste("rho02/result1028__1234",method[j], statistic[stat_index][j], "0.2", ".RData", sep = "_"))}
      res = sapply(results, rowMeans)[c(1,3,2),]
      lines(seq(0.2,1,0.2), res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
    }
    if (i==2){
      legend("top", legend = statistic[stat_index], cex=1, col=col[stat_index], lwd = 2.5, lty=stat_index, bty = "n", y.intersp = 0.5)
      if (r==2) {title(expression(bold(paste(rho, "=", 0.5))))
      } else {title(expression(bold(paste(rho, "=", 0.2))))}
    } 
  }
}
dev2bitmap("../../../manuscript/Submission to BIB/revised figures/Figure1.jpeg", type = "jpeg", 
           res = 600, height = 6, width=10)
### marginal
method = rep("marginal",3)
stat_index = 8:10
for (r in 1:2){
  for (i in 1:3){
    plot(0, xlab = "Signal amplitude (A)", ylab = ylab[i], ylim = c(0,ylim[i]), xlim = c(0.2,1), pch = NA)
    for(j in 1:length(method)){
      if (r==2) {
        # load(paste("result1028__1234", method[j], statistic[stat_index][j], "0.5_0.005_.RData", sep = "_"))
        load(paste("result1028_", method[j], statistic[stat_index][j], ".RData", sep = "_"))
      } else {load(paste("rho02/result1028__1234",method[j], statistic[stat_index][j], "0.2", ".RData", sep = "_"))}
      res = sapply(results, rowMeans)[c(1,3,2),]
      lines(seq(0.2,1,0.2), res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
    }
    if (i==2) {
      legend("top", legend = statistic[stat_index], cex=1, col=col[stat_index], lwd = 2.5, lty=stat_index, bty = "n", y.intersp = 0.5)
      if (r==2) {title(expression(bold(paste(rho, "=", 0.5))))
      } else {title(expression(bold(paste(rho, "=", 0.2))))}
    }
  }
}
dev2bitmap("../../../manuscript/Submission to BIB/revised figures/Figure2.jpeg", type = "jpeg", 
           res = 600, height = 6, width=10)

### RDVS
res = list()
for (i in 1:5){load(paste0("rho02/result1028_",i*2,"_1234_rdvs_mboost_0.2_0.005_.RData"));res[[i]]=results[[1]]}
results = res
save(results, file = "rho02/result1028__1234_rdvs_mboost_0.2_.RData")
res = list()
for (i in 1:5){load(paste0("rho02/result1028_",i*2,"_1234_rdvs_randomForest_0.2_0.005_.RData"));res[[i]]=results[[1]]}
results = res
save(results, file = "rho02/result1028__1234_rdvs_randomForest_0.2_.RData")

method = rep("rdvs",6)
stat_index = c(1:6)
for (r in 1:2){
  for (i in 1:3){
    plot(0, xlab = "Signal amplitude (A)", ylab = ylab[i], ylim = c(0,ylim[i]), xlim = c(0.2,1), pch = NA)
    for(j in 1:length(method)){
      if (r==2) {
        load(paste("result1028__1234", method[j], statistic[stat_index][j], "0.5_0.005_.RData", sep = "_"))
      } else {load(paste("rho02/result1028__1234",method[j], statistic[stat_index][j], "0.2", ".RData", sep = "_"))}
      res = sapply(results, rowMeans)[c(1,3,2),]
      lines(seq(0.2,1,0.2), res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
    }
    if (i==2) {
      legend("top", legend = statistic[stat_index], cex=1, col=col[stat_index], lwd = 2.5, lty=stat_index, bty = "n", y.intersp = 0.5)
      if (r==2) {title(expression(bold(paste(rho, "=", 0.5))))
      } else {title(expression(bold(paste(rho, "=", 0.2))))}
    }
  }
}
dev2bitmap("../../../manuscript/Submission to BIB/revised figures/Figure3.jpeg", type = "jpeg", 
           res = 600, height = 6, width=10)

### knockoff
method = rep("knockoff",6)
stat_index = c(1:6)
for (r in 1:2){
  for (i in 1:3){
    plot(0, xlab = "Signal amplitude (A)", ylab = ylab[i], ylim = c(0,ylim[i]), xlim = c(0.2,1), pch = NA)
    for(j in 1:length(method)){
      if (r==2) {
        load(paste("result1028_", method[j], statistic[stat_index][j], ".RData", sep = "_"))
      } else {load(paste("rho02/result1028__1234",method[j], statistic[stat_index][j], "0.2", ".RData", sep = "_"))}
      res = sapply(results, rowMeans)[c(1,3,2),]
      lines(seq(0.2,1,0.2), res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
    }
    if (i==2) {
      legend("top", legend = statistic[stat_index], cex=1, col=col[stat_index], lwd = 2.5, lty=stat_index, bty = "n", y.intersp = 0.5)
      if (r==2) {title(expression(bold(paste(rho, "=", 0.5))))
      } else {title(expression(bold(paste(rho, "=", 0.2))))}
    }
      
  }
}
dev2bitmap("../../../manuscript/Submission to BIB/revised figures/Figure4.jpeg", type = "jpeg", 
           res = 600, height = 6, width=10)


######### real data #########
site_previous = c("ALOX12","CD81","RARRES1","SOX17","BCR","GP1BB","ALOX12","DDIT3","ACVR1","KCNK4",
                  "SNCG","HOXB2","EPHX1","ELK3","COL1A1","NEFL","PADI4","ABCC2","FLT3","HS3ST2",
                  "BCR","HOXA5","ASCL2","TSC2","PLAT","EYA4","SERPINA5","PITX2","SERPINE1","HOXA9",
                  "CFTR","LIG3","DBC1","IGF1","TAL1","FES","CALCA","CRIP1","APOA1","MMP14","SLC22A3",
                  "SMARCB1","CRK","TNK1","HOXA5","DLK1","NAT2","TES","ASCL2","GRB10")

### real data

setwd("~/Box Sync/knockoffs/code/results/417/")
file_name = paste("selection417_real",
                  c(paste("glmnetcr",statistic[c(1,2,4)],sep = "_"),"mboost",
                    paste("marginal",statistic[8:10],sep = "_"),
                    paste("rdvs",statistic[1:6],sep = "_"),
                    paste("knockoff",statistic[1:6],sep = "_")),".RData", sep="_")
selected_genes = c()
for(i in 1:length(file_name)) {
  load(file_name[i])
  cat(file_name[i],"\n")
  cat(length(selection),"\n")
  selected_genes = c(selected_genes, selection)
}
library(stringr)
selected_genes = str_extract(selected_genes, "[^_]+")
selected_genes = sort(unique(selected_genes))
selected_compare = matrix(0, length(selected_genes), length(file_name))
for(i in 1:length(file_name)){
  load(file_name[i])
  selection = str_extract(selection, "[^_]+")
  selected_compare[,i] = selected_genes %in% selection
}
colnames(selected_compare) <- c(paste("glmnetcr",statistic[c(1,2,4)],sep = "+"),"mboost",
                                paste("marginal",statistic[8:10],sep = "+"),
                                paste("rdvs",statistic[1:6],sep = "+"),
                                paste("knockoff",statistic[1:6],sep = "+"))
rownames(selected_compare) <- selected_genes
ordered_genes = order(rowSums(selected_compare), decreasing = TRUE)
selected_compare = selected_compare[ordered_genes,]
selected_compare = selected_compare[rowSums(selected_compare)>4,colSums(selected_compare)>0]
nr = nrow(selected_compare)
nc = ncol(selected_compare)
par(mfrow = c(1,1), mai = c(1,1.8,0.8,0.8)+0.02)
plot(0, xaxt='n', yaxt='n',xlab = "",ylab = "", xlim = c(2.2,nr-1.2),ylim=c(1.1,nc-0.2),pch=NA)
for (i in which(row.names(selected_compare) %in% site_previous))
  polygon(c(i-0.5,i-0.5,i+0.5,i+0.5),c(0.5,(nr+0.5),(nr+0.5),0.5), col = "lightgrey", border = NA)
points(which(selected_compare==1,arr.ind = TRUE)[,1], nc+1-which(selected_compare==1,arr.ind = TRUE)[,2],
       pch = 4,lwd=1.5)
axis(1, at = 1:nr, selected_genes[ordered_genes][1:nr],las = 2, cex.axis=1, tick = F, line = -0.5,font = 2)
axis(2, at = nc:1, colnames(selected_compare),las = 2,cex.axis=1, tick = F, line = -0.5,font = 2)
for (i in 0:(nr-1)+0.5) abline(v = i, lty = 2, col = "gray50")
for (i in 1:nc+0.5) abline(h = i, lty = 2, col = "gray50")
percent = format(apply(selected_compare,2,function(a){
  b = a[a==1];round(sum(names(b)%in%site_previous)/length(b)*100,1)}), nsmall = 1)
# for(i in (nc:1)) mtext(paste(percent[nc+1-i],"%"),side = 4, line = 2,at = i,adj=1)
text(x = rep(nr+2), y = (nc:1), labels = paste(percent,"%"), xpd = NA,cex = 0.7,font=2)
dev2bitmap("../../../manuscript/Submission to BIB/revised figures/Figure5.jpeg", type = "jpeg", 
           res = 600, height = 5.48, width=13)

