rm(list=ls())
Data = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/house-prices/house-prices-advanced-regression-techniques/train.csv",header=T)
## missing data
sum(is.na(Data)); colSums(is.na(Data))
dim(Data);summary(Data)
# remove "Id"
df = Data[,-1]
colSums(is.na(df))
# replace NA->none
## Alley
replace(df$Alley,is.na(df$Alley),'None')->df$Alley
## Basement
replace(df$BsmtQual,is.na(df$BsmtQual),'None')->df$BsmtQual
replace(df$BsmtCond,is.na(df$BsmtCond),'None')->df$BsmtCond
cbind(df$BsmtCond[949],df$BsmtExposure[949])
df$BsmtExposure[949] = "No"
# cbind(which(is.na(df$BsmtFinType1)),which(is.na(df$BsmtFinType2)))
# cbind(df$BsmtFinType1[333],df$BsmtFinType2[333])
replace(df$BsmtExposure,is.na(df$BsmtExposure),'None')->df$BsmtExposure
replace(df$BsmtFinType1,is.na(df$BsmtFinType1),'None')->df$BsmtFinType1
replace(df$BsmtFinType2,is.na(df$BsmtFinType2),'None')->df$BsmtFinType2
## fire
replace(df$FireplaceQu,is.na(df$FireplaceQu),'None')->df$FireplaceQu
## garage
# cbind(which(is.na(df$GarageType)),which(is.na(df$GarageYrBlt)),which(is.na(df$GarageFinish)),which(is.na(df$GarageQual)),which(is.na(df$GarageCond)))
replace(df$GarageType,is.na(df$GarageType),'None')->df$GarageType
replace(df$GarageYrBlt,is.na(df$GarageYrBlt),0)->df$GarageYrBlt
replace(df$GarageFinish,is.na(df$GarageFinish),'None')->df$GarageFinish
replace(df$GarageQual,is.na(df$GarageQual),'None')->df$GarageQual
replace(df$GarageCond,is.na(df$GarageCond),'None')->df$GarageCond
## pool
replace(df$PoolQC,is.na(df$PoolQC),'None')->df$PoolQC
## fence
replace(df$Fence,is.na(df$Fence),'None')->df$Fence
## miscfeature
replace(df$MiscFeature,is.na(df$MiscFeature),'None')->df$MiscFeature
## LotFrontage / MasVnrType / MasVnrArea / Electrical
### MasVnrType : None / MasVnrArea : 0
replace(df$MasVnrType,is.na(df$MasVnrType),'None')->df$MasVnrType
replace(df$MasVnrArea,is.na(df$MasVnrArea),0)->df$MasVnrArea
### Electrical
table(df$Electrical)
df = df[-which(is.na(df$Electrical)),] # remove NA
dim(df)
### LotFrontage
df[is.na(df$LotFrontage),]
plot(density(log(df$LotFrontage),na.rm=T))
summary(log(df$LotFrontage))
replace(df$LotFrontage,is.na(df$LotFrontage),mean(log(df$LotFrontage),na.rm=T))->df$LotFrontage # mean value

# chr : MSSubClass / OverallQual / OverallCond
# int : 
# date : YearBuilt / YearRemodAdd / GarageYrBlt / MoSold / YrSold
strDate = cbind(as.numeric(df$YearRemodAdd),as.numeric(df$YearBuilt))
df$year_gap = drop(strDate[,1])-drop(strDate[,2]) 
df = df[,-c(19,20)]# remove YearBuilt / YearRemodAdd

df = within(df,{
  GarageYrBlt_f = character(0)
  GarageYrBlt_f[ GarageYrBlt >= 1900 & GarageYrBlt < 1920 ] = "1" 
  GarageYrBlt_f[ GarageYrBlt >= 1921 & GarageYrBlt < 1940 ] = "2" 
  GarageYrBlt_f[ GarageYrBlt >= 1941 & GarageYrBlt < 1960 ] = "3" 
  GarageYrBlt_f[ GarageYrBlt >= 1961 & GarageYrBlt < 1980 ] = "4" 
  GarageYrBlt_f[ GarageYrBlt >= 1981 & GarageYrBlt < 2000 ] = "5" 
  GarageYrBlt_f[ GarageYrBlt >= 2001] = "6"
  
  GarageYrBlt_f = factor(GarageYrBlt_f, level = c("1","2","3","4","5","6","None"))})
replace(df$GarageYrBlt_f,is.na(df$GarageYrBlt_f),"None")->df$GarageYrBlt_f # add GarageYrBlt_f
df = df[,-57] # remove GarageYrBlt

# install.packages("tidyr")
# library(tidyr) # merge variable
df = within(df,{
  season = character(0)
  season[ MoSold >= 1 & MoSold < 4 ] = "sp" 
  season[ MoSold >= 4 & MoSold < 7 ] = "sm" 
  season[ MoSold >= 7 & MoSold < 10 ] = "fl" 
  season[ MoSold >= 10 ] = "wn" 
  
  season = factor(season, level = c("sp","sm","fl","wn"))})  
df = df[,-73] # remove MoSold8

df$MSSubClass = as.character(df$MSSubClass)
df$OverallQual = as.character(df$OverallQual)
df$OverallCond = as.character(df$OverallCond)

str(df); sum(is.na(df))

c.var = NULL
for(i in 1:ncol(df)){
  if(is.character(df[,i])==T){df[,i] = as.factor(df[,i])}
  if(is.numeric(df[,i]) | is.integer(df[,i])){c.var = c(c.var, colnames(df)[i])}
}

## dummy 
d.mat = model.matrix(SalePrice~.,data=df)[,-1]
dim(d.mat)
is.data.frame(d.mat)
## test data
TData = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/house-prices/house-prices-advanced-regression-techniques/test.csv",header=T)
dim(TData); dim(df)
colSums(is.na(TData))

dat = as.data.frame(cbind(SalePrice=df$SalePrice,d.mat))
str(dat)
# install.packages("operators")
# library(operators)
# for(i in 1:ncol(dat)){
#   if(colnames(dat)[i] %!in% c.var){dat[,i] = as.factor(dat[,i])}
# }


# Method  -----------------------------------------------------------------


# 1. svd - regression
# install.packages("svd")
library(svd)
svd.mat = svd(d.mat)
D = diag(svd.mat$d)

plot(svd.mat$d^2/sum(svd.mat$d^2), type="l", xlab="Singualar vector",ylab = "variance explained")
plot(cumsum(svd.mat$d^2/sum(svd.mat$d^2)), type="l",xlab = "Singular vector",ylab = "Cumulative percent of variance explained")
U = svd.mat$u[1:nrow(svd.mat$u),1:30]
D = diag(svd.mat$d[1:30])
V = svd.mat$v[1:nrow(svd.mat$v),1:30]

dd = U%*%D%*%t(V)
dim(dd)
# regression 
reg_model = lm(SalePrice~., data=dat)
# regression with LASSO
library(glmnet)
# intrain = createDataPartition(dat$SalePrice,p=0.7,list=F)
# tr.x = d.mat[intrain,]; tr.y = df$SalePrice[intrain]
# ts.x = dat[-intrain,]; ts.y = df$SalePrice[-intrain]
lsso_model = cv.glmnet(d.mat,df$SalePrice,alpha=1,nfolds=5)
# regression with SCAD
library(ncvreg)
scad_model = cv.ncvreg(d.mat,df$SalePrice,penalty="SCAD",nfolds=5)
# decision tree : dummy x?
library(caret)
library(tree)
# intrain = createDataPartition(dat$SalePrice,p=0.7,list=F)
# tr = dat[intrain,];ts = dat[-intrain,]

idx = sample(1:nrow(dat),size=nrow(dat)*0.7)
tr = dat[idx,]; ts = dat[-idx,];
set.seed(2020)
str(df) ; str(dat)
tree_model = tree(SalePrice~., data=df)
tree_model = tree(SalePrice~., data=dat)



# Random Forest 
library(randomForest)

## tuning paramter : ntree, mtry
set.seed(2020)
nt.vec = seq(100,500,by=100)
m.mat = NULL; fold = 1:5
for(fid in fold){
  print(fid)
  idx = sample(1:nrow(dat),size=nrow(dat)*0.7)
  tr = dat[idx,]; ts = dat[-idx,];
  attach(tr)
  m.vec = NULL
  for(n in nt.vec){
    fm = randomForest(SalePrice~.,data=tr,ntree=n)
   # print(fm)
    pval = predict(fm,newdata=ts,type="response")
    m.vec = c(m.vec,mean((pval-ts$SalePrice)^2))
  }
  m.mat = rbind(m.mat,m.vec)
}
opt = which.min(colMeans(m.mat))
nt = nt.vec[opt]
attach(dat)
rf_model = randomForest(SalePrice~.,ntree=nt,data=dat)
importance(rf_model)
getTree(rf_model, 1, labelVar=T)

# svm
library(e1071)
library(kernlab)

idx = sample(1:nrow(df),size=nrow(df)*0.7)
tr.mat = df[idx,]; ts.mat = df[-idx,]
svm_tune = tune()