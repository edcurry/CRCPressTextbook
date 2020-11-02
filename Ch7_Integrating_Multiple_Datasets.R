rppa.data <- read.table("RPPA_RBN",sep="\t",head=TRUE,row.names=1)
dim(rppa.data)

rppa.data <- as.matrix(rppa.data)

plot(hclust(dist(rppa.data)))

plot(hclust(as.dist(1-cor(t(rppa.data)))))

rppa.hclust <- hclust(dist(t(rppa.data)))
rppa.clusters <- cutree(rppa.hclust,k=3)

library(gplots)
heatmap(rppa.data,col=bluered(100),scale="none")

rppa.cols <- c("yellow","red","green")[rppa.clusters]

heatmap(rppa.data,col=bluered(100),scale="none",ColSideColors=rppa.cols)
legend("topleft",legend=c("C1","C2","C3"),fill=c("yellow","red","green"))

library(limma)
design <- cbind(intercept=1,c2=as.numeric(rppa.clusters==2),c3=as.numeric(rppa.clusters==3))
rppa.cluster.fit <- lmFit(rppa.data,design=design)
rppa.cluster.fit <- eBayes(rppa.cluster.fit)
topTable(rppa.cluster.fit,coef=c(2,3))

rppa.cluster.proteins <- rownames(topTable(rppa.cluster.fit,coef=c(2,3),n=40,p.val=0.05))
heatmap(rppa.data[rppa.cluster.proteins,],col=bluered(100),scale="row",ColSideColors=rppa.cols)

library(NMF)
aheatmap(rppa.data[rppa.cluster.proteins,],scale="row",annCol=data.frame(clustr=factor(rppa.clusters)))

clin.data <- read.table("GBM_clinicalMatrix",sep="\t",head=TRUE,row.names=1)
sum(colnames(rppa.data) %in% rownames(clin.data))
rownames(clin.data) <- gsub(rownames(clin.data),pattern="-",replace=".")
sum(colnames(rppa.data) %in% rownames(clin.data))

sex.design <- cbind(intercept=1,female=as.numeric(clin.data[colnames(rppa.data),"gender"]=="FEMALE"))
sex.lm <- lmFit(rppa.data,design=sex.design)
sex.lm <- eBayes(sex.lm)
topTable(sex.lm,coef=2)

sex.cols <- c("yellow","green")[sex.design[,2]+1]
sex.proteins <- topTable(sex.lm,coef=2)$ID
heatmap(rppa.data[sex.proteins,],col=bluered(100),ColSideColors=sex.cols)
legend("topleft",legend=c("male","female"),fill=c("yellow","green"))

library(survival)
os.time <- clin.data[colnames(rppa.data),"days_to_last_followup"]
os.event <- as.numeric(clin.data[colnames(rppa.data),"vital_status"]=="DECEASED")
gbm.os <- Surv(os.time,os.event)
coxph(gbm.os ~ rppa.data[1,])

all.hrs <- rep(NA,nrow(rppa.data))
all.pvals <- rep(NA,nrow(rppa.data))
for(i in 1:nrow(rppa.data)){
	coxphmodel <- coxph(gbm.os ~ rppa.data[i,])
	all.hrs[i] <- summary(coxphmodel)$coef[1,2]
	all.pvals[i] <- summary(coxphmodel)$coef[1,5]
}
rppa.coxph.df <- data.frame(Protein=rownames(rppa.data),HR=all.hrs,p.value=all.pvals)
rppa.coxph.df <- rppa.coxph.df[order(rppa.coxph.df$p.value,decreasing=FALSE),]
rppa.coxph.df$adj.p.val <- p.adjust(rppa.coxph.df$p.value,method="fdr")
rppa.coxph.df[1:4,]

pai1.high <- as.numeric(rppa.data["PAI1",]>median(rppa.data["PAI1",]))
plot(survfit(gbm.os ~ pai1.high),col=c("black","red"),lwd=2)
legend("topright",legend=c("low-PAI1","high-PAI1"),fill=c("black","red"))

table(clin.data[,"chemo_therapy"])
had.chemo <- which(clin.data[colnames(rppa.data),"chemo_therapy"]=="YES")
coxph(gbm.os[had.chemo] ~ rppa.data["PAI1",had.chemo])

gbm.age <- clin.data[colnames(rppa.data)[had.chemo],"age_at_initial_pathologic_diagnosis"]
coxph(gbm.os[had.chemo] ~ gbm.age + rppa.data["PAI1",had.chemo])

gx.data <- as.matrix(read.table("HT_HG-U133A",sep="\t",head=TRUE,row.names=1))
shared.samples <- intersect(colnames(rppa.data),colnames(gx.data))
library(limma)
cor.design <- cbind(intercept=1,PAI1=rppa.data["PAI1",shared.samples])
cor.fit <- lmFit(gx.data[,shared.samples],design=cor.design)
cor.fit <- eBayes(cor.fit)
topTable(cor.fit,coef=2,number=2)

pai1.100cor.genes <- rownames(topTable(cor.fit,coef=2,number=100))
write.table(pai1.100cor.genes,file="GBM_PAI1_correlated100genes.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
