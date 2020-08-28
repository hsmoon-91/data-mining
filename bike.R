rm(list=ls())
# install and load
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
pkg <- c("ggplot2","lubridate","scales","dplyr","MASS","car","caTools","glm2",
         "glmnet","ncvreg","ncpen","lmvar")
ipak(pkg)

b.train = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/bike-sharing-demand/train.csv",header=T)
b.test = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/bike-sharing-demand/test.csv",header=T)

## remove "casual, registered"
b.train = b.train[,-c(10,11)]

## Converting integer to factor on training set
b.train$season = as.factor(b.train$season)
b.train$holiday = as.factor(b.train$holiday)
b.train$workingday = as.factor(b.train$workingday)
b.train$weather = as.factor(b.train$weather)

b.test$season = as.factor(b.test$season)
b.test$holiday = as.factor(b.test$holiday)
b.test$workingday = as.factor(b.test$workingday)
b.test$weather = as.factor(b.test$weather)

## Deriving day, hour from datetime field Train & Test
b.train$datetime = ymd_hms(b.train$datetime)
b.train$hour = hour(b.train$date)
b.train$day = wday(b.train$date)
# b.train$month = month(b.train$date, label=T)
b.train$month = month(b.train$date)
b.train[,11:13] = lapply(b.train[,11:13], factor) #converting derived variables into factors

b.test$datetime = ymd_hms(b.test$datetime)
b.test$hour = hour(b.test$date)
b.test$day = wday(b.test$date)
# b.test$month = month(b.test$date, label=T)
b.test$month = month(b.test$date)
b.test[,10:12] = lapply(b.test[,10:12], factor) #converting derived variables into factors

## Removing datetime field 
b.train$datetime = NULL

## catagorical variable vectorization
str(b.train); colnames(b.train)
str(b.test); colnames(b.test)

dim(b.train[,c(1:8,10:12)])
dim(b.test[,c(2:12)])
df = rbind(b.train[,c(1:8,10:12)],b.test[,c(2:12)])

x.fac = df[,c(1,2,3,4,9,10,11)]
x.num = df[,-c(1,2,3,4,9,10,11)]
x.dummies = as.data.frame(model.matrix(~.-1, x.fac))
x.mat = cbind(x.num,x.dummies); dim(x.mat)

tx.mat = x.mat[1:dim(b.train[,c(1:8,10:12)])[1],]
nx.mat = x.mat[1:dim(b.test[,c(2:12)])[1],]
ty.vec = b.train[,9]

# x.fac = b.train[,c(1,2,3,4,10,12)] 
# x.num = b.train[,-c(1,2,3,4,9,10,12)]
# x.dummies = as.data.frame(model.matrix(~.-1, x.fac))
# tx.mat = cbind(x.num,x.dummies)
# 
tx.mat = matrix(as.numeric(unlist(tx.mat)),nrow=nrow(tx.mat))
ty.vec = as.vector(as.numeric(ty.vec))

# x.fac = b.test[,c(2,3,4,5,10,11,12)] 
# x.num = b.test[,-c(1,2,3,4,5,10,11,12)]
# x.dummies = as.data.frame(model.matrix(~.-1, x.fac))
# nx.mat = cbind(x.num,x.dummies)
# 
# nx.mat = matrix(as.numeric(unlist(nx.mat)),nrow=nrow(nx.mat))

# Splitting the Train dataset -> train / validation
# set.seed(123)
# df = cbind(tx.mat,ty.vec)
# split = sample.split(df[,dim(df)[2]], SplitRatio = 0.70)
# tdf = subset(df, split == TRUE)
# vdf = subset(df, split == FALSE)
# 
# ty.vec = tdf[,dim(tdf)[2]]; vy.vec = vdf[,dim(vdf)[2]]
# tx.mat = tdf[,-dim(tdf)[2]]; vx.mat = vdf[,-dim(vdf)[2]]
# tx.mat = matrix(as.numeric(unlist(tx.mat)),nrow=nrow(tx.mat))
# vx.mat = matrix(as.numeric(unlist(vx.mat)),nrow=nrow(vx.mat))
# ty.vec = as.vector(as.numeric(ty.vec))
# v.vec = as.vector(as.numeric(vy.vec))


## modeling
# fit = cv.glmnet(tx.mat,ty.vec,family="gaussian",type.measure="mse",alpha=0,nfolds=5) # ridge 
fit = cv.glmnet(tx.mat,ty.vec,family="poisson",type.measure="mse",alpha=0) # ridge
# fit = cv.glmnet(tx.mat,ty.vec,family="poisson",type.measure="mse",alpha=0.5) # elastic net
# fit = cv.glmnet(tx.mat,ty.vec,family="poisson",type.measure="mse",alpha=1) # lasso
# fit = cv.glmnet(tx.mat,ty.vec,family="Gamma",type.measure = "mse")
# fit = cv.ncvreg(tx.mat,ty.vec,family="poisson",penalty="SCAD",gamma=2.1,nfolds=5)
coef(fit)
## validation
# pre = predict(fit,s=fit$lambda.min,newx=vx.mat) # glmnet
# pre = predict(fit,X=vx.mat) # ncvreg
# rmsle(vy.vec,exp(pre))

## prediction
nx.mat = matrix(as.numeric(unlist(nx.mat)),nrow=nrow(nx.mat))
pre = predict(fit,s=fit$lambda.min,newx=nx.mat)
exp(pre)

b.test = read.csv("C:/Users/HSMOON/Desktop/[2020-1]datamining/numerical data/bike-sharing-demand/test.csv",header=T)
dt = b.test$datetime
submission_ridge = data.frame(datetime=dt, count=drop(exp(pre)))
write.csv(submission_ridge,'submission_ridge.csv',row.names=FALSE)

# library(readr)
# # install.packages("xgboost")
# library(xgboost)
# is.matrix(tdf)
# 
# k = as.data.frame(tdf)
# cv <- xgb.cv(data = k, label = k$ty.vec, nfold = 10)


# write.table(final,file="C:/Users/HSMOON/Desktop/[2020-1]datamining/numerical data/bike-sharing-demand/submission.csv")

