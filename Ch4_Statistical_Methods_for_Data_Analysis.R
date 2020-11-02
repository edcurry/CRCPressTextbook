set.seed(10)
a <- rnorm(3,mean=1,sd=1)
b <- rnorm(3,mean=1,sd=1)
mean(a)
mean(b)
x <- rnorm(3,mean=1,sd=1)
y <- rnorm(3,mean=2,sd=1)
mean(x)
mean(y)

# a simple illustration of hypothesis testing
mean(x) - mean(y)

meanDiffs <- c()
for(i in 1:100){
	x <- rnorm(3,mean=1,sd=1)
	y <- rnorm(3,mean=1,sd=1)
	meanDiffs <- c(meanDiffs,abs(mean(x)-mean(y)))
}
sum(meanDiffs>=2.599836)

# generate random samples from uniform distribution, convert to True/False depending on whether or not they are greater than 0.5 (this gives samples from a Bernoulli distribution with probability of True=0.5)
set.seed(10)
x <- runif(10)>0.5
x
as.numeric(x)
# compute probabilities from Binomial distribution
dbinom(7,prob=0.5,size=10)
dbinom(3,prob=0.5,size=10)
sum(dbinom(c(0,1,2,3,7,8,9,10),prob=0.5,size=10))

# create matrices, use these to demonstrate matrix multiplication
A <- matrix(c(1,2,3,4,5,6),nrow=3,ncol=2)
B <- matrix(c(1,2,3,4,5,6),nrow=2,ncol=3)
A%*%B

# create variables to illustrate the effects of centering and scaling
set.seed(10)
x <- rnorm(10,mean=1,sd=2)
y <- rnorm(10,mean=2,sd=1)
boxplot(list(x=x,y=y))
xc <- x-median(x)
yc <- y-median(y)
boxplot(list(x=xc,y=yc))
boxplot(list(x=xs,y=ys))
xz <- (x-median(x))/sd(x)
yz <- (y-median(y))/sd(y)
boxplot(list(x=xz,y=yz))

# create independently sampled variables, and then a dependent variable, in order to illustrate correlation
set.seed(10)
x <- rnorm(20)
y <- rnorm(20)
cor(x,y)
plot(x,y)
cor.test(x,y)
z <- x+rnorm(20,sd=0.1)
cor(x,z)
cor.test(x,z)
plot(x,z)
set.seed(10)
x <- runif(20)
y <- runif(20)
cor(x,y)
z <- x+rnorm(20,sd=0.1)
cor(x,z)
cor.test(x,y)
cor.test(x,y,method='spearman')
cor.test(x,y)
cor.test(x,z,method='spearman')

# list pre-loaded datasets available in R
data()

# load the Swiss towns (1888) dataset to illustrate clustering
data(swiss)
dist(swiss)
hclust(dist(swiss))
plot(hclust(dist(swiss)))
hclust(dist(t(swiss)))

# use the Swiss towns dataset to illustrate linear model fitting and hypothesis testing
data(swiss)
lm(Fertility ~ Education, data=swiss)
data(swiss)
m1 <- lm(Fertility ~ Education, data=swiss)
summary(m1)
m2 <- lm(Fertility ~ Education + Infant.Mortality, data=swiss)
summary(m2)

# randomly sample p-values from a uniform distribution to demonstrate multiple hypothesis testing adjustment
p <- runif(1000)
p.adjust(p,method='fdr')

# use the Swiss towns dataset to illustrate PCA
data(swiss)
swisspc <- prcomp(swiss)
names(swisspc)
(swisspc$sdev^2)/sum(swisspc$sdev^2)
sum((swiss[1,]-swisspc$center)*swisspc$rotation[,1])
swisspc$x[1:4,1:3]
plot(x=swisspc$x[,1],y=swisspc$x[,2],type='n')
text(rownames(swisspc$x),x=swisspc$x[,1],y=swisspc$x[,2])
