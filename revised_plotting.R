setwd("~/Box Sync/knockoffs/code/results/1028/")

col = c("blue","mediumpurple","red","gray10","orange", "forestgreen", "deeppink","seagreen","navyblue","magenta")
statistic = c("AIC", "BIC", "path", "cv", "mboost", "ordinalForest", "Bayesian", "Jonckheere", "uni_cumulative", "uni_sratio")
ylab = c("FDR (%)", "Power (%)")
### 
### prior-to-control
method = c(rep("glmnetcr",3),"mboost")
stat_index = c(1,2,4,5)
ylim = c(100, 100)
m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.45,0.45,0.1))
for (r in 1:2){
  for (i in 1:2){
    par(mar = c(3,3,2,1) + 0.1, cex = 0.8, mgp = c(2.5, 1.5, 0), font.main = 2)
    plot(0, ann = FALSE, ylim = c(0,ylim[i]), 
         xlim = c(0.2,1), pch = NA, xaxt ="n", yaxt = "n")
    axis(1, at = seq(0.2,1,0.2), label = rep("", 5), tck = -0.01)
    axis(1, at = seq(0.2,1,0.2), line = -1, lwd = 0, cex.axis = 0.9, font = 2)
    axis(2, at = seq(0,100,20), label = rep("", 6), tck = -0.01)
    axis(2, at = seq(0,100,20), line = -1, lwd = 0, cex.axis = 0.9, font = 2)
    mtext(side = 1, text = "Signal amplitude (A)", line = 1.7, cex = 0.9, font = 2)
    mtext(side = 2, text = ylab[i], line = 1.7, cex = 0.9, font = 2)
    for(j in 1:length(method)){
      if (r==2) {load(paste("result1028_", method[j], statistic[stat_index][j], ".RData", sep = "_"))
      } else {load(paste("rho02/result1028__1234",method[j], statistic[stat_index][j], "0.2", ".RData", sep = "_"))}
      res = sapply(results, rowMeans)
      lines(seq(0.2,1,0.2)-0.015+0.005*j, res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
      sd = sapply(results, function(x) apply(x,MARGIN = 1, FUN = sd))
      arrows(seq(0.2,1,0.2)-0.015+0.005*j, (res[i,]-sd[i,])*100, seq(0.2,1,0.2)-0.015+0.005*j, (res[i,]+sd[i,])*100, code=3, 
             length=0.02, angle = 90, col = col[stat_index[j]])
    }
    if (r==2) {title(expression(bold(paste(rho, "=", 0.5))))
       } else {title(expression(bold(paste(rho, "=", 0.2))))}
  }
}
par(mar = c(0,0,0,0) + 0.1)
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = statistic[stat_index], text.font = 2,
       col=col[stat_index], lwd=2.5, cex=1.2, horiz = TRUE, lty=stat_index, bty = "n", x.intersp = 0.2)
dev2bitmap("../../../manuscript/Submission to BIB/second revision/second revised figures/Figure1.jpeg", 
           type = "jpeg", res = 750, height = 7, width=8)
### marginal
method = rep("marginal",3)
stat_index = 8:10
layout(mat = m,heights = c(0.45,0.45,0.1))
for (r in 1:2){
  for (i in 1:2){
    par(mar = c(3,3,2,1) + 0.1, cex = 0.8, mgp = c(2.5, 1.5, 0), font.main = 2)
    plot(0, ann = FALSE, ylim = c(0,ylim[i]), 
         xlim = c(0.2,1), pch = NA, xaxt ="n", yaxt = "n")
    axis(1, at = seq(0.2,1,0.2), label = rep("", 5), tck = -0.01)
    axis(1, at = seq(0.2,1,0.2), line = -1, lwd = 0, cex.axis = 0.9, font = 2)
    axis(2, at = seq(0,100,20), label = rep("", 6), tck = -0.01)
    axis(2, at = seq(0,100,20), line = -1, lwd = 0, cex.axis = 0.9, font = 2)
    mtext(side = 1, text = "Signal amplitude (A)", line = 1.7, cex = 0.9, font = 2)
    mtext(side = 2, text = ylab[i], line = 1.7, cex = 0.9, font = 2)
    for(j in 1:length(method)){
      if (r==2) {
        load(paste("result1028_", method[j], statistic[stat_index][j], ".RData", sep = "_"))
      } else {load(paste("rho02/result1028__1234",method[j], statistic[stat_index][j], "0.2", ".RData", sep = "_"))}
      res = sapply(results, rowMeans)
      lines(seq(0.2,1,0.2)-0.015+0.005*j, res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
      sd = sapply(results, function(x) apply(x,MARGIN = 1, FUN = sd))
      arrows(seq(0.2,1,0.2)-0.015+0.005*j, (res[i,]-sd[i,])*100, seq(0.2,1,0.2)-0.015+0.005*j, (res[i,]+sd[i,])*100, code=3, 
             length=0.02, angle = 90, col = col[stat_index[j]])
    }
    if (r==2) {title(expression(bold(paste(rho, "=", 0.5))))
    } else {title(expression(bold(paste(rho, "=", 0.2))))}
  }
}
par(mar = c(0,0,0,0) + 0.1)
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c("Jonckheere", "Univariate CL", "Univariate CR"), text.font = 2,
       col=col[stat_index], lwd=2.5, cex=1.2, horiz = TRUE, lty=stat_index, bty = "n", x.intersp = 0.2)
dev2bitmap("../../../manuscript/Submission to BIB/second revision/second revised figures/Figure2.jpeg", type = "jpeg", 
           res = 750, height = 7, width=8)

### RDVS
res = list()
for (i in 1:5){load(paste0("../1222/result1222_",i*2,"_1234_rdvs_mboost_0.2_.RData"));res[[i]]=results[[1]]}
results = res
save(results, file = "../1222/result1222__1234_rdvs_mboost_0.2_.RData")
res = list()
for (i in 1:5){load(paste0("../1222/result1222_",i*2,"_1234_rdvs_ordinalForest_0.2_.RData"));res[[i]]=results[[1]]}
results = res
save(results, file = "../1222/result1222__1234_rdvs_ordinalForest_0.2_.RData")
res = list()
for (i in 1:5){load(paste0("../1222/result1222_",i*2,"_1234_rdvs_mboost_0.5_.RData"));res[[i]]=results[[1]]}
results = res
save(results, file = "../1222/result1222__1234_rdvs_mboost_0.5_.RData")
res = list()
for (i in 1:5){load(paste0("../1222/result1222_",i*2,"_1234_rdvs_ordinalForest_0.5_.RData"));res[[i]]=results[[1]]}
results = res
save(results, file = "../1222/result1222__1234_rdvs_ordinalForest_0.5_.RData")

method = rep("rdvs",6)
stat_index = c(1:6)
layout(mat = m,heights = c(0.45,0.45,0.1))
for (r in 1:2){
  for (i in 1:2){
    par(mar = c(3,3,2,1) + 0.1, cex = 0.8, mgp = c(2.5, 1.5, 0), font.main = 2)
    plot(0, ann = FALSE, ylim = c(0,ylim[i]), 
         xlim = c(0.2,1), pch = NA, xaxt ="n", yaxt = "n")
    axis(1, at = seq(0.2,1,0.2), label = rep("", 5), tck = -0.01)
    axis(1, at = seq(0.2,1,0.2), line = -1, lwd = 0, cex.axis = 0.9, font = 2)
    axis(2, at = seq(0,100,20), label = rep("", 6), tck = -0.01)
    axis(2, at = seq(0,100,20), line = -1, lwd = 0, cex.axis = 0.9, font = 2)
    mtext(side = 1, text = "Signal amplitude (A)", line = 1.7, cex = 0.9, font = 2)
    mtext(side = 2, text = ylab[i], line = 1.7, cex = 0.9, font = 2)
    for(j in 1:length(method)){
      if (r==2) {
        load(paste("../1222/result1222__1234", method[j], statistic[stat_index][j], "0.5_.RData", sep = "_"))
      } else {load(paste("../1222/result1222__1234",method[j], statistic[stat_index][j], "0.2", ".RData", sep = "_"))}
      res = sapply(results, rowMeans)
      lines(seq(0.2,1,0.2)-0.015+0.005*j, res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
      sd = sapply(results, function(x) apply(x,MARGIN = 1, FUN = sd))
      arrows(seq(0.2,1,0.2)-0.015+0.005*j, (res[i,]-sd[i,])*100, seq(0.2,1,0.2)-0.015+0.005*j, (res[i,]+sd[i,])*100, code=3, 
             length=0.02, angle = 90, col = col[stat_index[j]])
    }
    if (r==2) {title(expression(bold(paste(rho, "=", 0.5))))
    } else {title(expression(bold(paste(rho, "=", 0.2))))}
  }
}
par(mar = c(0,0,0,0) + 0.1)
plot(1, type = "n", axes=FALSE, xlab="", ylab="", xlim = c(-2,2))
legend(x = "top",x.intersp=0.2, xjust=0, yjust=0, xpd = TRUE,
       text.width=c(0.2,0.2,0.2,0.2,0.15,0.25), horiz=TRUE, 
       legend = statistic[stat_index], text.font = 2,
       col=col[stat_index], lwd=2.5, cex=1.2, lty=stat_index, bty = "n")
dev2bitmap("../../../manuscript/Submission to BIB/second revision/second revised figures/Figure3.jpeg", type = "jpeg", 
           res = 750, height = 7, width=8)

### knockoff
statistic = c("AIC", "BIC", "path", "cv", "mboost", "randomForest", "Bayesian", "Jonckheere", "uni_cumulative", "uni_sratio")
method = rep("knockoff",6)
stat_index = c(1:6)
layout(mat = m,heights = c(0.45,0.45,0.1))
for (r in 1:2){
  for (i in 1:2){
    par(mar = c(3,3,2,1) + 0.1, cex = 0.8, mgp = c(2.5, 1.5, 0), font.main = 2)
    plot(0, ann = FALSE, ylim = c(0,ylim[i]), 
         xlim = c(0.2,1), pch = NA, xaxt ="n", yaxt = "n")
    axis(1, at = seq(0.2,1,0.2), label = rep("", 5), tck = -0.01)
    axis(1, at = seq(0.2,1,0.2), line = -1, lwd = 0, cex.axis = 0.9, font = 2)
    axis(2, at = seq(0,100,20), label = rep("", 6), tck = -0.01)
    axis(2, at = seq(0,100,20), line = -1, lwd = 0, cex.axis = 0.9, font = 2)
    mtext(side = 1, text = "Signal amplitude (A)", line = 1.7, cex = 0.9, font = 2)
    mtext(side = 2, text = ylab[i], line = 1.7, cex = 0.9, font = 2)
    for(j in 1:length(method)){
      if (r==2) {
        load(paste("result1028_", method[j], statistic[stat_index][j], ".RData", sep = "_"))
      } else {load(paste("rho02/result1028__1234",method[j], statistic[stat_index][j], "0.2", ".RData", sep = "_"))}
      res = sapply(results, rowMeans)
      lines(seq(0.2,1,0.2)-0.015+0.005*j, res[i,]*100, col = col[stat_index[j]], lty = stat_index[j], lwd = 2.5)
      sd = sapply(results, function(x) apply(x,MARGIN = 1, FUN = sd))
      arrows(seq(0.2,1,0.2)-0.015+0.005*j, (res[i,]-sd[i,])*100, seq(0.2,1,0.2)-0.015+0.005*j, (res[i,]+sd[i,])*100, code=3, 
             length=0.02, angle = 90, col = col[stat_index[j]])
    }
    if (r==2) {title(expression(bold(paste(rho, "=", 0.5))))
    } else {title(expression(bold(paste(rho, "=", 0.2))))}
  }
}
par(mar = c(0,0,0,0) + 0.1)
plot(1, type = "n", axes=FALSE, xlab="", ylab="", xlim = c(-2,2))
legend(x = "top",x.intersp=0.2, xjust=0, yjust=0, xpd = TRUE,
       text.width=c(0.2,0.2,0.2,0.2,0.15,0.25), horiz=TRUE, 
       legend = statistic[stat_index], text.font = 2,
       col=col[stat_index], lwd=2.5, cex=1.2, lty=stat_index, bty = "n")
dev2bitmap("../../../manuscript/Submission to BIB/second revision/second revised figures/Figure4.jpeg", type = "jpeg", 
           res = 750, height = 7, width=8)

######### real data #########
# site_inc = c("ALOX12","CD81","RARRES1","SOX17","BCR","GP1BB","ALOX12","DDIT3","ACVR1","KCNK4",
#                   "SNCG","HOXB2","EPHX1","ELK3","COL1A1","NEFL","PADI4","ABCC2","FLT3","HS3ST2",
#                   "BCR","HOXA5","ASCL2","TSC2","PLAT","EYA4","SERPINA5","PITX2","SERPINE1","HOXA9",
#                   "CFTR","LIG3","DBC1","IGF1","TAL1","FES","CALCA","CRIP1","APOA1","MMP14","SLC22A3",
#                   "SMARCB1","CRK","TNK1","HOXA5","DLK1","NAT2","TES","ASCL2","GRB10")
# site_dec = c("IL8","CREB1","HLA-DPB1","TIAM1","NOTCH4","ST6GAL1","TIAM1","ESR1","HLA-DPA1","GSTP1",
#              "HLA-DPA1","ESR1","HDAC9","PDGFRB","ESR1","NQO1","TNF","RASSF1","TIMP1","DAB2IP","DLC1",
#              "DLC1","MPO","ERN1","GSTM2","IGF2","TGFB2","DAB2IP","KLK10","CPA4","HTR2A","HLA-DRA",
#              "TNFSF8","IGF2","PDGFRB","PRKCDBP","SPP1","GSTM2","GPC3","S100A4","STAT5A","IL16","IL6",
#              "DNASE1L1","MYLK","DNASE1L1","SPARC","ZMYND10","CAPG","IGF2","MMP7","SH3BP2","IL18BP",
#              "SH3BP2","COMT","FABP3","S100A2","G6PD","BMP4","IFNG","IL10","TEK","PDGFRB","OSM","PTPRH",
#              "PYCARD","MFAP4","RIPK3","COL18A1","CD2","VAV1","SEPT9","HDAC1","MPL","HLA-DOA","EVI2A",
#              "ARHGDIB","IL16","CD9","PSCA","DLC1","LIF","BMP4","EFNB1","MST1R","AOC3","FGF2","GJB2",
#              "LTB4R","TNFRSF10A","ZAP70","CD34","PTK6","GABRA5"
# )
setwd("~/Box Sync/knockoffs/code/real_data/from_kellie/")
load("LiverSamples.RData")
library(clinfun)
# BiocManager::install("qvalue")
library(qvalue)
dec.pvalue<-inc.pvalue<-numeric()
for (i in 1:dim(exprs(data))[1]) {
  dec.pvalue[i]<-jonckheere.test(exprs(data)[i,], pData(data)$TissueType, alternative = "decreasing")$p.value
  inc.pvalue[i]<-jonckheere.test(exprs(data)[i,], pData(data)$TissueType, alternative = "increasing")$p.value
}
dec.q<-qvalue(dec.pvalue)
inc.q<-qvalue(inc.pvalue)
site_dec = featureNames(data)[dec.q$qvalues<0.05]
site_inc = featureNames(data)[inc.q$qvalues<0.05]
gene_dec = str_extract(site_dec, "[^_]+")
gene_inc = str_extract(site_inc, "[^_]+")

### real data
statistic = c("AIC", "BIC", "path", "cv", "mboost", "ordinalForest", "Bayesian", "Jonckheere", "uni_cumulative", "uni_sratio")
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
colnames(selected_compare) <- c(paste("L1 penalized CR",statistic[c(1,2,4)],sep = "+"),"mboost",
                                paste("Marginal",c("Jonckheere", "CL", "CR")),
                                paste("RDVS",statistic[1:6],sep = "+"),
                                paste("Knockoff",statistic[1:6],sep = "+"))
rownames(selected_compare) <- selected_genes
ordered_genes = order(rowSums(selected_compare), decreasing = TRUE)
selected_compare = selected_compare[ordered_genes,]
selected_compare_trim = selected_compare[rowSums(selected_compare)>8,colSums(selected_compare)>0]
nr = nrow(selected_compare_trim)
nc = ncol(selected_compare_trim)
percent = format(apply(selected_compare,2,function(a){
  b = a[a==1];round(sum(names(b)%in%c(gene_inc,gene_dec))/length(b)*100,1)}), nsmall = 1)
percent = percent[!is.na(as.numeric(percent))]
method_order = order(percent, decreasing = T)
selected_compare_trim = selected_compare_trim[,method_order]
par(mfrow = c(1,1), mai = c(1.2,2.1,0.8,0.8)+0.02)
plot(0, xaxt='n', yaxt='n',xlab = "",ylab = "", xlim = c(2.2,nr-1.2),ylim=c(1.1,nc-0.2),pch=NA)
for (i in which(row.names(selected_compare_trim) %in% gene_inc))
  polygon(c(i-0.5,i-0.5,i+0.5,i+0.5),c(0.5,(nr+0.5),(nr+0.5),0.5), col = "lightpink", border = NA)
for (i in which(row.names(selected_compare_trim) %in% gene_dec))
  polygon(c(i-0.5,i-0.5,i+0.5,i+0.5),c(0.5,(nr+0.5),(nr+0.5),0.5), col = "lightblue", border = NA)
points(which(selected_compare_trim==1,arr.ind = TRUE)[,1], nc+1-which(selected_compare_trim==1,arr.ind = TRUE)[,2],
       pch = 4,lwd=2, cex = 2)
axis(1, at = 1:nr, selected_genes[ordered_genes][1:nr],las = 2, cex.axis=1.2, tick = F, line = -0.5,font = 2)
axis(2, at = nc:1, colnames(selected_compare_trim),las = 2,cex.axis=1.2, tick = F, line = -0.5,font = 2)
for (i in 0:(nr-1)+0.5) abline(v = i, lty = 2, col = "gray50")
for (i in 1:nc+0.5) abline(h = i, lty = 2, col = "gray50")
# for(i in (nc:1)) mtext(paste(percent[nc+1-i],"%"),side = 4, line = 2,at = i,adj=1)
text(x = rep(nr+2), y = (nc:1), labels = paste(percent[method_order],"%"), xpd = NA,cex = 1.2,font=2)
dev2bitmap("../../../manuscript/Submission to BIB/second revision/second revised figures/Figure5.jpeg", type = "jpeg", 
           res = 750, height = 5.48, width=13)

