
#setting wd
setwd("/Users/darafilyaskina/Desktop/hw1p2")
#reading files
x1<-read.csv("x1.csv",stringsAsFactors=FALSE)
x2<-read.csv("x2.csv",stringsAsFactors=FALSE)
x3<-read.csv("x3.csv",stringsAsFactors=FALSE)
x4<-read.csv("x4.csv",stringsAsFactors=FALSE)
x5<-read.csv("x5.csv",stringsAsFactors=FALSE)
x6<-read.csv("x6.csv",stringsAsFactors=FALSE)
x7<-read.csv("x7.csv",stringsAsFactors=FALSE)
x8<-read.csv("x8.csv",stringsAsFactors=FALSE)
y<-read.csv("y.csv",stringsAsFactors=FALSE)

library(Hmisc)
#merging things together
data<-Merge(x1,x2,x3,x4,x5,x6,x7,x8,y,id = ~ id)

##finding the appropriate model

#generating a vector of variable names
files_to_loop<-list.files()
library(stringr)
variables<-gsub("\\.csv","",files_to_loop)
variables<-variables[1:8]
#generating a vector of dependent variable in string
dep<-c("y")
#finding all combinations
combinations<-combn(variables,4,rep=FALSE)

#generating a list of R-squared for every model:
#first create a vector of r sqrt
#than using as.formula function regress y on all combinations of x onbtained
#finally, find r sqrt of every model and put it in the generated vector
r_sqr<-vector(length=ncol(combinations))
for (i in 1:ncol(combinations)){
  r_sqr[i]<-summary(lm(as.formula(paste(dep, paste(combinations[,i], collapse=" + "), sep=" ~ ")), data=data))$r.squared
  #> y ~ x1 + x2 + x3 + x4
}

#finding highest r sqrt number of model
number_of_model<-which(max(r_sqr) == r_sqr)
#finding this model
result<-combinations[,number_of_model]
result




