# for brc-cancer:
setwd('~/RTutorials')
library(ShortRead)
reads <- readFastq("cancertest.fq")

quals <- array(NA,dim=c(length(reads),width(reads[1])))
for(i in 1:nrow(quals)){
	quals[i,] <- as.numeric(quality(reads)[[i]])-33
}

makefig('cancertestFastqScoreBoxplot.png')
boxplot(quals)
dev.off()

goodq.reads <- reads[apply(quals,MARGIN=1,function(x)sum(as.numeric(x<30))<=5)]

length(goodq.reads)/length(reads)

goodq.reads.noN <- goodq.reads[alphabetFrequency(sread(goodq.reads))[,"N"]==0]

writeFastq(goodq.reads.noN,file="cancertest_filtered.fq",compress=FALSE)
