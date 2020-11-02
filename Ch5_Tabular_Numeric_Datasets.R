#Load processed siRNA screen data:
siRNA.screen <- read.table("siRNAscreen_data.txt",sep="\t",header=TRUE)

#View first rows of table:
head(siRNA.screen)

#Make matrix using numeric part of table:
siRNA.zscores <- as.matrix(siRNA.screen[,-c(1,2)])
rownames(siRNA.zscores) <- as.character(siRNA.screen$GENE)

# scatter plot
plot(x=siRNA.zscores[,1],y=siRNA.zscores[,2],xlab=colnames(siRNA.zscores)[1],ylab=colnames(siRNA.zscores)[2])

# box plot
boxplot(lapply(c(1:5),function(x)siRNA.zscores[,x]),names=colnames(siRNA.zscores))

# compute median and standard deviation for each column
apply(siRNA.zscores,MARGIN=2,median)
apply(siRNA.zscores,MARGIN=2,sd)

# bar plot
barplot(siRNA.zscores[1,])

# correlations
PIK3CA.cors <- apply(siRNA.zscores,MARGIN=1,function(x)cor(x,siRNA.zscores[which(rownames(siRNA.zscores)=="PIK3CA"),]))

PIK3CA.cors <- rep(NA,nrow(siRNA.zscores))
for(i in 1:nrow(siRNA.zscores)){
	PIK3CA.cors[i] <- cor(siRNA.zscores[i,],siRNA.zscores[which(rownames(siRNA.zscores)=="PIK3CA"),])
}

PIK3CA.corPvals <- apply(siRNA.zscores,MARGIN=1,function(x)cor.test(x,siRNA.zscores[which(rownames(siRNA.zscores)=="PIK3CA"),])$p.value)

PIK3CA.corPvals <- rep(NA,nrow(siRNA.zscores))
for(i in 1:nrow(siRNA.zscores)){
	PIK3CA.corPvals[i] <- cor.test(siRNA.zscores[i,],siRNA.zscores[which(rownames(siRNA.zscores)=="PIK3CA"),])$p.value
}

PIK3CA.df <- data.frame(Gene=rownames(siRNA.zscores),cor=PIK3CA.cors,p.value=PIK3CA.corPvals)
PIK3CA.df <- PIK3CA.df[order(PIK3CA.df$p.value,decreasing=FALSE),]
PIK3CA.df$adj.p.val <- p.adjust(PIK3CA.df$p.value,method="BH")
head(PIK3CA.df)

plot(siRNA.zscores[which(rownames(siRNA.zscores)=="PIK3CA"),],type="l",col="red",ylab="z-score",xlab="cell line")
points(siRNA.zscores[which(rownames(siRNA.zscores)=="DUSP22"),],type="l",col="blue")
legend("topright",legend=c("PIK3CA","DUSP22"),lty=c(1,1),col=c("red","blue"))

# clustering
plot(hclust(dist(t(siRNA.zscores))))

plot(hclust(as.dist(1-cor(siRNA.zscores))))

# heatmaps
# if necessary: install.packages("NMF")
library(NMF)

aheatmap(siRNA.zscores,Rowv=NA,scale="row")
aheatmap(siRNA.zscores,Rowv=NA,scale="none")

# linear models
library(limma)

colnames(siRNA.zscores)
design2 <- cbind(intercept=1,grp2=c(0,0,1,1))
grpfit <- lmFit(siRNA.zscores[,2:5],design=design2)
grpfit2 <- eBayes(grpfit)
topTable(grpfit2,coef=2)

aheatmap(siRNA.zscores[topTable(grpfit2)$ID,],scale="row")

design <- cbind(Intercept=1,MCF7=c(1,0,0,0,0))
mcf7.fit <- lmFit(siRNA.zscores,design=design)
mcf7.fit <- eBayes(mcf7.fit)
topTable(mcf7.fit,coef=2)

