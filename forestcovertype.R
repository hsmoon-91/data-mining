rm(list=ls())

data = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/forest-cover-type-prediction/forest-cover-type-prediction/train.csv")

dim(data)
str(data)
# missing data
colSums(is.na(data)) 

df = data[,-1]

# correlation
library(corrplot)
corrplot(cor(df[,1:10]))

par(mfrow=c(2,5))
# histogram
for(i in 1:10){ hist(df[,i], main=paste("histogram of ",colnames(df)[i]),xlab=colnames(df)[i]) }

library(e1071)
# skewness / kurtosis
for(i in 1:10){ cat(skewness(df[,i]),kurtosis(df[,i]),"\n") }

# boxplot :5,10
par(mfrow=c(1,2))
boxplot(df[,5]); boxplot(df[,10])

plot(df[,5]); df[df[,5]>500,1:10]
plot(df[,10]); df[df[,10]>6800 & df[,10]<7000,1:10]
