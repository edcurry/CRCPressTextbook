# create a vector of 3 numbers
vec <- c(1,4,3)
# view the vector
vec

# create a vector of a sequence of numbers
vec2 <- c(2:11)
vec2

# use square brackets to index a vector, using either numbers or another vector
vec[1]
vec2[vec]
vec2[-1]
vec2[-vec]

# create, view and index an array
A <- array(c(1:6),dim=c(2,3))
A
A[1,2]
A[2,]
A[,c(1,3)]

# create and view a list
mylist <- list(1,2,3)
mylist

# set the value for a particular element of a list
mylist[[1]] <- c(1,2,3)
mylist
mylist[[2]] <- list(1,2,3)
mylist

# create a list with a named element
mylist <- list(1,2,foo=3)
mylist
mylist$foo

# create and view a dataframe
mydf <- data.frame(numbers=c(1:3),names=c("one","two","three"))
mydf

# write a data frame to file
write.table(mydf,file="mydf.txt",sep="\t",quote=FALSE,row.names=FALSE)

# read in a dataframe from file
B <- read.table(file="mydf.txt",sep="\t",header=TRUE)
B$names
as.character(B$names)

# create a character vector
x <- c("apple","orange","banana","mango","pineapple")
# create an empty vector in which to add the indices for which the vector x has the character "mango"
position <- c()
# run a for loop with an iterator variable i taking values from 1 to the length of the vector x, and if the corresponding element is "mango", concatenate the iterator index to the vector 'position'
for(i in 1:length(x)){
	if(x[i]=="mango"){
		position <- c(position,i)
	}
}
position

# as a quicker way of achieving this, use logical indexing
position <- c(1:length(x))[x=="mango"]
position

# or use the inbuilt function 'which'
position <- which(x=="mango")
position

# define a function that does nothing
foo1 <- function(arg1,arg2){}

# run this function specifying the 2 arguments
foo1(arg1="a",arg2=2)

# define another function which does nothing, and takes no arguments
foo1 <- function(){}
foo1()

# now define a function which will return the greater of the two supplied arguments
foo2 <- function(arg1,arg2){
  if (arg1>arg2){ 
     out <- arg1
  }
  else{ 
     out <- arg2 
  }
  out
}
# run this function with some different arguments
foo2(arg1=1,arg2=2)
foo2(arg1=4,arg2=2)

# create an array with which to illustrate the 'apply' function
A <- array(1:20,dim=c(4,5))
A

# apply the function 'sum' to the rows of the array 'A'
apply(A,MARGIN=1,sum)
# apply the function 'sum' to the columns of the array 'A'
apply(A,MARGIN=2,sum)

# define a new function, which take the largest value from the first 3 columns and adds it to the smaller value from the second 2 columns.
newfun <- function(x){
	max(x[1:3])+min(x[4:5])
}
# apply this function 'newfun' to the array 'A'
apply(A,MARGIN=1,newfun)

# create a character vector (again)
x <- c("apple","orange","banana","mango","pineapple")
# create a new character vector by pasting the text "_juice" to each element of 'x'
y <- paste(x,"juice",sep="_")
y

# use the function 'strsplit' to split each element of 'y' around the underscore
strsplit(y,split="_")

# define a function to return the first part of a character when split around the underscore
fruit <- function(x){
	strsplit(x,split="_")[[1]][1]
}

# apply this function 'fruit' to the vector 'y'
sapply(y,fruit)

# create a vector to illustrate R's graphical capabilities
vec <- c(1,4,3)
# plot these values as the y-coordinates, using their position in the vector as the x-coordinates
plot(vec)

# create another vector
vec2 <- c(2:11)

# now plot the 2 vectors on the same graph, using 'points'
plot(vec2,type="l",ylim=c(1,11))
points(vec,type="l",col="red")

# create a scatterplot of 2 vectors plotted against each other
plot(x=vec,y=vec^2)

# create a list for making a boxplot
mylist2 <- list(c(1:3),2,c(1:5))
boxplot(mylist2)

# create a graphical device on a file, to draw a graph to
png(file='MyBoxplot.png')
boxplot(mylist2)
# remember to close the graphical device with 'dev.off()'
dev.off()

# install the ggplot2 package for pretty graphs
install.packages('ggplot2')

# create a dataset (based on random numbers) to illustrate ggplot's capabilities
set.seed(10)
# make 1 vector of Normally-distributed random numbers
viability <- rnorm(40)
# make a vector of the labels 'control' or 'treated'
treatment <- rep(c('control','treated'),20)
# make another vector of the labels 'media1' or 'media2'
culture <- rep(c('media1','media2'),each=20)
# combine these 3 vectors into a data frame
plotdf <- data.frame(viability=viability,treatment=treatment,culture=culture)
head(plotdf)

# load the ggplot2 package
library(ggplot2)

# create stratified boxplot with jittered points overlaid
ggplot(plotdf,aes(x=culture,y=viability,fill=treatment)) + geom_boxplot() + geom_point(position=position_jitterdodge())
