rm(list=ls())
Data = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/house-prices/house-prices-advanced-regression-techniques/train.csv",header=T)
## missing data
sum(is.na(Data)); colSums(is.na(Data))
dim(Data);summary(Data)
# remove "Id"
df = Data[,-1]
# colSums(is.na(df))

#  EDA --------------------------------------------------------------------
# missmap(df,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)


# discrete variable -------------------------------------------------------
## Alley
replace(df$Alley,is.na(df$Alley),'None')->df$Alley
table(df$Alley)
boxplot(df$SalePrice~df$Alley);boxplot(log(df$SalePrice)~df$Alley)

## Basement
replace(df$BsmtQual,is.na(df$BsmtQual),'None')->df$BsmtQualm
replace(df$BsmtCond,is.na(df$BsmtCond),'None')->df$BsmtCond
replace(df$BsmtExposure,is.na(df$BsmtExposure),'None')->df$BsmtExposure
# cbind(which(is.na(df$BsmtFinType1)),which(is.na(df$BsmtFinType2)))
# cbind(df$BsmtFinType1[333],df$BsmtFinType2[333])
replace(df$BsmtFinType1,is.na(df$BsmtFinType1),'None')->df$BsmtFinType1
replace(df$BsmtFinType2,is.na(df$BsmtFinType2),'None')->df$BsmtFinType2
## fire
replace(df$FireplaceQu,is.na(df$FireplaceQu),'None')->df$FireplaceQu
## garage
# cbind(which(is.na(df$GarageType)),which(is.na(df$GarageYrBlt)),which(is.na(df$GarageFinish)),which(is.na(df$GarageQual)),which(is.na(df$GarageCond)))
replace(df$GarageYrBlt,is.na(df$GarageYrBlt),"None")->df$GarageYrBlt
replace(df$GarageType,is.na(df$GarageType),'None')->df$GarageType
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
### Electrical
table(df$Electrical)
df = df[-which(is.na(df$Electrical)),] # remove NA
# remove Utilities
df = df[,-(which(colnames(df)=="Utilities"))]

# continuous variable -----------------------------------------------------
### LotFrontage\
# plot(density(log(df$LotFrontage),na.rm=T))
# summary(log(df$LotFrontage))
# replace(df$LotFrontage,is.na(df$LotFrontage),mean(df$LotFrontage,na.rm=T))->df$LotFrontage # mean value
df = df[,-which(colnames(df)=="LotFrontage")] # remove NA
replace(df$MasVnrArea,is.na(df$MasVnrArea),median(df$MasVnrArea,na.rm=T))->df$MasVnrArea

# feature engineering -----------------------------------------------------

# chr : MSSubClass / OverallQual / OverallCond
# int : 
# date : YearBuilt / YearRemodAdd / GarageYrBlt / MoSold / YrSold
# strDate = cbind(as.numeric(df$YearRemodAdd),as.numeric(df$YearBuilt))
# df$year_gap = drop(strDate[,1])-drop(strDate[,2]) 
# df = df[,-c(which(colnames(df)=="YearBuilt"),which(colnames(df)=="YearRemodAdd"))] # remove YearBuilt / YearRemodAdd

# df = within(df,{
#   GarageYrBlt_f = character(0)
#   GarageYrBlt_f[ GarageYrBlt >= 1900 & GarageYrBlt < 1920 ] = "1" 
#   GarageYrBlt_f[ GarageYrBlt >= 1921 & GarageYrBlt < 1940 ] = "2" 
#   GarageYrBlt_f[ GarageYrBlt >= 1941 & GarageYrBlt < 1960 ] = "3" 
#   GarageYrBlt_f[ GarageYrBlt >= 1961 & GarageYrBlt < 1980 ] = "4" 
#   GarageYrBlt_f[ GarageYrBlt >= 1981 & GarageYrBlt < 2000 ] = "5" 
#   GarageYrBlt_f[ GarageYrBlt >= 2001] = "6"
#   
#   GarageYrBlt_f = factor(GarageYrBlt_f, level = c("1","2","3","4","5","6","None"))})
# replace(df$GarageYrBlt_f,is.na(df$GarageYrBlt_f),"None")->df$GarageYrBlt_f # add GarageYrBlt_f
# df = df[,-c(which(colnames(df)=="GarageYrBlt"))] # remove GarageYrBlt
# 
# # install.packages("tidyr")
# # library(tidyr) # merge variable
# df = within(df,{
#   season_sold = character(0)
#   season_sold[ MoSold >= 1 & MoSold < 4 ] = "sp" 
#   season_sold[ MoSold >= 4 & MoSold < 7 ] = "sm" 
#   season_sold[ MoSold >= 7 & MoSold < 10 ] = "fl" 
#   season_sold[ MoSold >= 10 ] = "wn" 
#   
#   season_sold = factor(season_sold, level = c("sp","sm","fl","wn"))})  
# df = df[,-c(which(colnames(df)=="MoSold"))] # remove MoSold8

df$GarageYrBlt = as.character(df$GarageYrBlt)
df$MoSold = as.character(df$MoSold)
df$MSSubClass = as.character(df$MSSubClass)
df$OverallQual = as.character(df$OverallQual)
df$OverallCond = as.character(df$OverallCond)
df$YrSold = as.factor(df$YrSold)
df$YearBuilt = as.factor(df$YearBuilt)
df$YearRemodAdd = as.factor(df$YearRemodAdd)
str(df); sum(is.na(df))


# -----------------------------------------------------------------------------------------
c.var = NULL
for(i in 1:ncol(df)){
  if(is.character(df[,i])==T){df[,i] = as.factor(df[,i])}
  # if(is.numeric(df[,i])==T | is.integer(df[,i])==T){c.var = c(c.var, colnames(df)[i])}
}

c.name = NULL
for(i in 1:ncol(df)){ if(class(df[,i])=="numeric" | class(df[,i])=="integer"){c.name=c(c.name,colnames(df)[i]) }}


str(df)
# for(i in 1:length(c.name)){
#   # print(i)
#   val.vec = as.vector(summary(df[,c.name[i]]))
#   # print(val.vec)
#   if(val.vec[1]!=0){
#     df[,c.name[i]] = log(df[,c.name[i]]+0.001)
#   }
#   if(val.vec[1]==0){
#     df[,c.name[i]] = log(df[,c.name[i]]+1)
#   }
# 
#   # if(val.vec[1]<0){
#   #   df[,c.name[i]] = log(df[,c.name[i]]+1)
#   # }
#   # if(val.vec[1]==0){
#   #   df[,c.name[i]] = log(df[,c.name[i]]+0.001)
#   # }
# 
#   # if(i %in% c(1,2,8,11,18,29,30)){
#   # # print(summary(log(df[,c.name[i]]+0.001))) # 1,2,8,11,18,29
#   #   df[,c.name[i]] = log(df[,c.name[i]]+0.001)
#   # }
#   # if(i %in% c(3,4,5,6,7,9,10,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,28)){
#   #   df[,c.name[i]] = log(df[,c.name[i]]+1)
#   # }
# }

## dummy 
f.name = NULL
for(i in 1:ncol(df)){
  if(class(df[,i])=="factor"){f.name = c(f.name,colnames(df)[i])}
}

# dummies = model.matrix(~.-1, data=df[,f.name])
# d.mat = cbind(dummies, df[,c.name])

# install.packages("operators")
# library(operators)
# for(i in 1:ncol(dat)){
#   if(colnames(dat)[i] %!in% c.var){dat[,i] = as.factor(dat[,i])}
# }


## test data
TData = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/house-prices/house-prices-advanced-regression-techniques/test.csv",header=T)
dim(TData); dim(df)
colSums(is.na(TData))
tdf = TData[,-1]

# discrete variable -------------------------------------------------------
## Alley
replace(tdf$Alley,is.na(tdf$Alley),labels(which.max(table(df$Alley))))->tdf$Alley
## Basement
replace(tdf$BsmtQual,is.na(tdf$BsmtQual),labels(which.max(table(df$BsmtQual))))->tdf$BsmtQual
replace(tdf$BsmtCond,is.na(tdf$BsmtCond),labels(which.max(table(df$BsmtCond))))->tdf$BsmtCond
replace(tdf$BsmtExposure,is.na(tdf$BsmtExposure),labels(which.max(table(df$BsmtExposure))))->tdf$BsmtExposure
replace(tdf$BsmtFinType1,is.na(tdf$BsmtFinType1),labels(which.max(table(df$BsmtFinType1))))->tdf$BsmtFinType1
replace(tdf$BsmtFinType2,is.na(tdf$BsmtFinType2),labels(which.max(table(df$BsmtFinType2))))->tdf$BsmtFinType2
## garage
replace(tdf$GarageType,is.na(tdf$GarageType),labels(which.max(table(df$GarageType))))->tdf$GarageType
# replace(tdf$GarageYrBlt,is.na(tdf$GarageYrBlt),0)->tdf$GarageYrBlt
replace(tdf$GarageFinish,is.na(tdf$GarageFinish),labels(which.max(table(df$GarageFinish))))->tdf$GarageFinish
replace(tdf$GarageQual,is.na(tdf$GarageQual),labels(which.max(table(df$GarageQual))))->tdf$GarageQual
replace(tdf$GarageCond,is.na(tdf$GarageCond),labels(which.max(table(df$GarageCond))))->tdf$GarageCond
## fire
replace(tdf$FireplaceQu,is.na(tdf$FireplaceQu),labels(which.max(table(df$FireplaceQu))))->tdf$FireplaceQu
## pool
replace(tdf$PoolQC,is.na(tdf$PoolQC),labels(which.max(table(df$PoolQC))))->tdf$PoolQC
## fence
replace(tdf$Fence,is.na(tdf$Fence),labels(which.max(table(df$Fence))))->tdf$Fence
## miscfeature
replace(tdf$MiscFeature,is.na(tdf$MiscFeature),labels(which.max(table(df$MiscFeature))))->tdf$MiscFeature
replace(tdf$GarageYrBlt,is.na(tdf$GarageYrBlt),labels(which.max(table(df$GarageYrBlt))))->tdf$GarageYrBlt
## LotFrontage / MasVnrType / MasVnrArea / Electrical
### MasVnrType : None / MasVnrArea : 0
replace(tdf$MasVnrType,is.na(tdf$MasVnrType),labels(which.max(table(df$MasVnrType))))->tdf$MasVnrType
mat = cbind(tdf[,"MasVnrType"],tdf[,"MasVnrArea"]); mat = as.data.frame(mat); mat[,1] = as.factor(mat[,1])
val = which(is.na(mat[,2]))
for(i in 1:length(val)){
  if(mat[val[i],1]=="none"){replace(tdf$MasVnrArea,is.na(tdf$MasVnrArea),0)->tdf$MasVnrArea}
  else{replace(tdf$MasVnrArea,is.na(tdf$MasVnrArea),mean(df$MasVnrArea,na.rm=T))->tdf$MasVnrArea}
}
# MSZoning
replace(tdf$MSZoning,is.na(tdf$MSZoning),labels(which.max(table(df$MSZoning))))->tdf$MSZoning # 4 
# replace(tdf$Utilities,is.na(tdf$Utilities),labels(which.max(table(df$Utilities))))->tdf$Utilities # 2 
replace(tdf$Exterior1st,is.na(tdf$Exterior1st),labels(which.max(table(df$Exterior1st))))->tdf$Exterior1st # 1
replace(tdf$Exterior2nd,is.na(tdf$Exterior2nd),labels(which.max(table(df$Exterior2nd))))->tdf$Exterior2nd # 1
replace(tdf$KitchenQual,is.na(tdf$KitchenQual),labels(which.max(table(df$KitchenQual))))->tdf$KitchenQual # 1
replace(tdf$Functional,is.na(tdf$Functional),labels(which.max(table(df$Functional))))->tdf$Functional # 2
replace(tdf$SaleType,is.na(tdf$SaleType),labels(which.max(table(df$SaleType))))->tdf$SaleType # 1
# remove Utilities
tdf = tdf[,-(which(colnames(tdf)=="Utilities"))]

# continuous variable -----------------------------------------------------
# LotFrontage
# replace(tdf$LotFrontage,is.na(tdf$LotFrontage),mean(df$LotFrontage,na.rm=T))->tdf$LotFrontage # mean value
tdf = tdf[,-which(colnames(tdf)=="LotFrontage")] # remove LotFrontage

# Bsmt
replace(tdf$BsmtFinSF1,is.na(tdf$BsmtFinSF1),median(df$BsmtFinSF1,na.rm=T))->tdf$BsmtFinSF1 # median value # 1
replace(tdf$BsmtFinSF2,is.na(tdf$BsmtFinSF2),median(df$BsmtFinSF2,na.rm=T))->tdf$BsmtFinSF2 # 1
replace(tdf$BsmtUnfSF,is.na(tdf$BsmtUnfSF),median(df$BsmtUnfSF,na.rm=T))->tdf$BsmtUnfSF  # 1
replace(tdf$TotalBsmtSF,is.na(tdf$TotalBsmtSF),median(df$TotalBsmtSF,na.rm=T))->tdf$TotalBsmtSF # 1
replace(tdf$BsmtFullBath,is.na(tdf$BsmtFullBath),median(df$BsmtFullBath,na.rm=T))->tdf$BsmtFullBath # 2
replace(tdf$BsmtHalfBath,is.na(tdf$BsmtHalfBath),median(df$BsmtHalfBath,na.rm=T))->tdf$BsmtHalfBath # 2
replace(tdf$GarageCars,is.na(tdf$GarageCars),median(df$GarageCars,na.rm=T))->tdf$GarageCars # 1
replace(tdf$GarageArea,is.na(tdf$GarageArea),median(df$GarageArea,na.rm=T))->tdf$GarageArea  # 1
sum(is.na(tdf))
# feature engineering -----------------------------------------------------


# strDate = cbind(as.numeric(tdf$YearRemodAdd),as.numeric(tdf$YearBuilt))
# tdf$year_gap = drop(strDate[,1])-drop(strDate[,2])
# tdf = tdf[,-c(which(colnames(tdf)=="YearRemodAdd"),which(colnames(tdf)=="YearBuilt"))] # remove YearBuilt / YearRemodAdd
# tdf = tdf[-which(tdf$year_gap==-1),] # year_gap == -1 remove
# 
# tdf = within(tdf,{
#   GarageYrBlt_f = character(0)
#   GarageYrBlt_f[ GarageYrBlt >= 1900 & GarageYrBlt < 1920 ] = "1" 
#   GarageYrBlt_f[ GarageYrBlt >= 1921 & GarageYrBlt < 1940 ] = "2" 
#   GarageYrBlt_f[ GarageYrBlt >= 1941 & GarageYrBlt < 1960 ] = "3" 
#   GarageYrBlt_f[ GarageYrBlt >= 1961 & GarageYrBlt < 1980 ] = "4" 
#   GarageYrBlt_f[ GarageYrBlt >= 1981 & GarageYrBlt < 2000 ] = "5" 
#   GarageYrBlt_f[ GarageYrBlt >= 2001] = "6"
#   
#   GarageYrBlt_f = factor(GarageYrBlt_f, level = c("1","2","3","4","5","6","None"))})
# replace(tdf$GarageYrBlt_f,is.na(tdf$GarageYrBlt_f),labels(which.max(table(df$GarageYrBlt_f))))->tdf$GarageYrBlt_f # add GarageYrBlt_f
# tdf = tdf[,-which(colnames(tdf)=="GarageYrBlt")] # remove GarageYrBlt
# 
# # install.packages("tidyr")
# # library(tidyr) # merge variable
# tdf = within(tdf,{
#   season_sold = character(0)
#   season_sold[ MoSold >= 1 & MoSold < 4 ] = "sp" 
#   season_sold[ MoSold >= 4 & MoSold < 7 ] = "sm" 
#   season_sold[ MoSold >= 7 & MoSold < 10 ] = "fl" 
#   season_sold[ MoSold >= 10 ] = "wn" 
#   
#   season_sold = factor(season_sold, level = c("sp","sm","fl","wn"))})  
# tdf = tdf[,-which(colnames(tdf)=="MoSold")] # remove MoSold8

tdf$GarageYrBlt = as.character(tdf$GarageYrBlt)
tdf$MoSold = as.character(tdf$MoSold)
tdf$MSSubClass = as.character(tdf$MSSubClass)
tdf$OverallQual = as.character(tdf$OverallQual)
tdf$OverallCond = as.character(tdf$OverallCond)
tdf$YrSold = as.factor(tdf$YrSold)
tdf$YearBuilt = as.factor(tdf$YearBuilt)
tdf$YearRemodAdd = as.factor(tdf$YearRemodAdd)

c.var = NULL
for(i in 1:ncol(tdf)){
  if(is.character(tdf[,i])==T){tdf[,i] = as.factor(tdf[,i])}
  # if(is.numeric(tdf[,i])==T | is.integer(tdf[,i])==T){c.var = c(c.var, colnames(tdf)[i])}
}

## dummy /(SalesPrice X)
# td.mat = model.matrix(SalePrice~.,data=tdf)[,-1]
# dim(td.mat);is.data.frame(td.mat)
# 
# tdat = as.data.frame(cbind(SalePrice=tdf$SalePrice,td.mat))
# str(tdat)

tc.name = NULL
for(i in 1:ncol(tdf)){ if(class(tdf[,i])=="numeric" | class(tdf[,i])=="integer"){tc.name=c(tc.name,colnames(tdf)[i]) }}
# for(i in 1:length(tc.name)){
#   # print(i)
#   val.vec = as.vector(summary(tdf[,tc.name[i]]))
#   # print(val.vec)
#   if(val.vec[1]!=0){
#     tdf[,tc.name[i]] = log(tdf[,tc.name[i]]+0.001)
#   }
#   if(val.vec[1]==0){
#     tdf[,tc.name[i]] = log(tdf[,tc.name[i]]+1)
#   }
# }
# 
# for(i in 1:length(c.name)){
#   if(i %in% c(1,2,8,11,18)){
#     # print(summary(log(tdf[,c.name[i]]+0.001))) # 1,2,8,11,16
#     tdf[,c.name[i]] = log(tdf[,c.name[i]]+0.001)
#   }
#   if(i %in% c(3,4,5,6,7,9,10,12,13,14,15,17,19,20,21,22,23,24,25)){
#     # print(summary(log(tdf[,c.name[i]]+1))) # 3,4,5,6,7,9,10,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27
#     tdf[,c.name[i]] = log(tdf[,c.name[i]]+1)
#   }
# }
tf.name = NULL
for(i in 1:ncol(tdf)){
  if(class(tdf[,i])=="factor"){tf.name = c(tf.name,colnames(tdf)[i])}
}

# for(name in tf.name){
#   cat(name,":", length(levels(tdf[,name])),"\n") 
# }

length(f.name) ; length(tf.name)
length(c.name) ; length(tc.name)


### levels
# ff.name = NULL
# for(name in tf.name){
#   if (length(levels(tdf[,name])) != length(levels(df[,name]))){
#     ff.name = c(ff.name, name)
#   }
#   # cat(name, ":", levels(tdf[,name]), "\n")
#   # cat(name, ":", levels(df[,name]), "\n")
# }

# final dataset case 1 ----------------------------------------------------------
# install.packages("spatstat")
# library(spatstat)
# # dummies = model.matrix(~.-1,tdf[,ff.name])
# dummies = dummify(tdf[,ff.name])
# td.mat = cbind(dummies,tdf[,tc.name])
# # dummies = model.matrix(~.-1,df[,ff.name])
# dummies = dummify(df[,ff.name])
# d.mat = cbind(dummies,df[,c.name])
# 
# ddf = cbind(df[,ff.name],df[,c.name])
# tddf = cbind(tdf[,ff.name],tdf[,tc.name])
# 
# dim(d.mat);dim(td.mat)
# head(d.mat)
# dim(ddf); dim(tddf)


# final data  case 2 --------------------------------------------------------------
library(spatstat)
sum(f.name==tf.name)
mat = rbind(df[,f.name],tdf[,tf.name])
dummies = dummify(mat)
d.mat = as.data.frame(cbind(dummies[1:dim(df)[1],],df[,c.name]))
td.mat = as.data.frame(cbind(dummies[(dim(df)[1]+1):dim(mat)[1],],tdf[,tc.name]))
library(corrplot)
corrplot(cor(df[,c.name]),method="shade",tl.col='black', tl.srt=45,pch=1,rect.lwd = 0.5)

# length(c.name)
# par(mfrow=c(4,7))
# for(i in 1:length(c.name)){
#   hist(df[,c.name[i]], main=paste("Histogram of ",c.name[i]),xlab=c.name[i]) 
# }
par(mfrow = c(2,2))
boxplot(log(df$SalePrice)~df$Condition2,xlab=f.name[colnames(df)=="Condition2"],ylab="SalePrice",main="boxplot of Condition2")
boxplot(log(df$SalePrice)~df$OverallQual,xlab=f.name[colnames(df)=="OverallQual"],ylab="SalePrice",main="boxplot of OverallQual")
boxplot(log(df$SalePrice)~df$ExterQual,xlab=f.name[colnames(df)=="ExterQual"],ylab="SalePrice",main="boxplot of ExterQual")
boxplot(log(df$SalePrice)~df$ExterCond,xlab=f.name[colnames(df)=="ExterCond"],ylab="SalePrice",main="boxplot of ExterCond")

for(i in 1:length(f.name)){
boxplot(log(df$SalePrice)~df[,f.name[i]],xlab=f.name[i],ylab="SalePrice",main=paste("boxplot of ",f.name[i]))
}

boxplot(log(df$SalePrice)~df[,f.name[1]])
### correlation
# library(corrplot)
# corrplot(cor(df[,c.name]),type="upper",method="number")
# Method  -------------------------------------------------------------------------------------------------


# # 1. svd - regression
# install.packages("svd")
# library(svd)
# svd.mat = svd(d.mat[,-which(colnames(d.mat)=="SalePrice")])
# # svd.mat = svd(ddf)
# D = diag(svd.mat$d)
# plot(svd.mat$d^2/sum(svd.mat$d^2), type="l", xlab="Singualar vector",ylab = "variance explained",xlim=c(0, 5))
# plot(cumsum(svd.mat$d^2/sum(svd.mat$d^2)), type="l",xlab = "Singular vector",ylab = "Cumulative percent of variance explained",
#      xlim=c(0, 100))
# U = svd.mat$u[1:nrow(svd.mat$u),1:20]
# D = diag(svd.mat$d[1:20])
# # V = svd.mat$v[1:nrow(svd.mat$v),1:20]
# nsvd.mat = U%*%D
# head(nsvd.mat)
# svd.mat = U%*%D%*%t(V)
# 
# # colnames(dim(svd.mat)) = colnames(d.mat)
# # svd.mat = as.data.frame(svd.mat)
# nsvd.mat = as.data.frame(cbind(nsvd.mat,SalePrice=d.mat[,which(colnames(d.mat)=="SalePrice")]))
# 
# tsvd.mat = svd(td.mat)
# tU = tsvd.mat$u[1:nrow(tsvd.mat$u),1:20]
# tD = diag(tsvd.mat$d[1:20])
# ntsvd.mat = tU%*%tD
# # tV = tsvd.mat$v[1:nrow(tsvd.mat$v),1:20]
# # tsvd.mat = tU%*%tD%*%t(tV)
# # colnames(tsvd.mat) = colnames(td.mat)
# ntsvd.mat = as.data.frame(ntsvd.mat)
# 
# class(svd.mat); class(d.mat)
# dim(svd.mat)
# dim(tsvd.mat)

# dim(nsvd.mat)
# dim(ntsvd.mat)
# dim(d.mat)
# dim(td.mat)

# ddf = d.mat = nsvd.mat
# tddf = td.mat = ntsvd.mat

# 1-1. truncated SVD 
# install.packages("irlba")
# library(irlba)
# mat = irlba(as.matrix(d.mat),nv=30)
# mat$v

# 2. pca - regression
# mat1 = df[,c.name]; mat1 = mat1[,-which(colnames(d.mat)=="SalePrice")]; mat2 = df[,ff.name]
# tmat1 = tdf[,tc.name]; tmat2 = dummify(tdf[,tf.name])
# nd.mat = d.mat[,-which(colnames(d.mat)=="SalePrice")]
# pca_dt = prcomp(nd.mat,scale=F)
# tpca_dt = prcomp(td.mat,scale=F)
# summary(pca_dt)
# # screeplot(pca_dt,type="l",npcs=140)
# # screeplot(tpca_dt,type="l",npcs=140)
# nd.mat = as.data.frame(cbind(pca_dt$x[,1:18],SalePrice=d.mat[,which(colnames(d.mat)=="SalePrice")]))
# ntd.mat = as.data.frame(tpca_dt$x[,1:18])
# ddf = d.mat = nd.mat
# tddf = td.mat = ntd.mat
# install.packages("steps")
# library(steps)
# 3. original - regression
f.mat = NULL
# regression 
reg_model = lm(SalePrice~.,data=d.mat)
vif(reg_model)
# f_reg_model = step(reg_model,direction = "both")
# plot(reg_model)
# as.matrix(tddf)%*%drop(coef(reg_model)[-1])  # data frame / matrix ? 
f.mat = cbind(f.mat, exp(predict(reg_model,newdata=td.mat)))
# regression with LASSO
library(glmnet)
# intrain = createDataPartition(dat$SalePrice,p=0.7,list=F)
# tr.x = d.mat[intrain,]; tr.y = df$SalePrice[intrain]
# ts.x = dat[-intrain,]; ts.y = df$SalePrice[-intrain]

lsso_model = cv.glmnet(x=as.matrix(d.mat[,-which(colnames(d.mat)=="SalePrice")]),y=drop(d.mat$SalePrice),alpha=1,nfolds=5)
f.mat = cbind(f.mat, exp(predict(lsso_model,as.matrix(td.mat),lambda=lsso_model$lambda.min)))

ridge_model = cv.glmnet(x=as.matrix(d.mat[,-which(colnames(d.mat)=="SalePrice")]),y=drop(d.mat$SalePrice),alpha=0.2,nfolds=5)
f.mat = cbind(f.mat, exp(predict(ridge_model,as.matrix(td.mat),lambda=ridge_model$lambda.min)))

# install.packages("grpreg")
# library(grpreg)
# head(d.mat)
# g.vec = c(rep(1,5),rep(2,2),rep(3,3),rep(4,4),rep(5,4),rep(6,5),rep(7,3),rep(8,25),rep(9,9),rep(10,5))
# glsso_model = cv.grpreg(X=as.matrix(d.mat[,-which(colnames(d.mat)=="SalePrice")]),y=drop(d.mat$SalePrice),group=, penalty="grLasso",nfolds=5)

# lsso_model = cv.glmnet(x=as.matrix(svd.mat[,-which(colnames(svd.mat)=="SalePrice")]),y=drop(svd.mat$SalePrice),alpha=1,nfolds=5)
# f.mat = cbind(f.mat, exp(predict(lsso_model,as.matrix(td.mat),lambda=lsso_model$lambda.min)))

# regression with SCAD 
# lamdba sequence : max-> min
library(ncvreg)
scad_model = cv.ncvreg(X=as.matrix(d.mat[,-which(colnames(d.mat)=="SalePrice")]),y=drop(d.mat$SalePrice),penalty="SCAD",nfolds=5)
f.mat = cbind(f.mat, exp(predict(scad_model,as.matrix(td.mat),lambda=scad_model$lambda.min)))
# # decision tree : dummy x?
# library(caret)
# library(tree)
# # intrain = createDataPartition(dat$SalePrice,p=0.7,list=F)
# # tr = dat[intrain,];ts = dat[-intrain,]
# set.seed(2020)
# idx = sample(1:nrow(ddf),size=nrow(ddf)*0.7)
# tr = ddf[idx,]; ts = ddf[-idx,];
# tc = tree.control(tr, mincut = 5, minsize = 10, mindev = 0.01) 
# tree_model = tree(SalePrice~., data=tr, control = tc)
# # tree_model = tree(SalePrice~., data=d.mat) # tree categorical vraiabl x ?


# Random Forest 
library(randomForest)
## tuning paramter : ntree, mtry
set.seed(2020)
nt.vec = seq(100,500,by=100)
m.mat = NULL; fold = 1:5
for(fid in fold){
  print(fid)
  idx = sample(1:nrow(d.mat),size=nrow(d.mat)*0.7)
  tr = d.mat[idx,]; ts = d.mat[-idx,];
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
attach(d.mat)
rf_model = randomForest(SalePrice~.,ntree=nt,data=d.mat)
# plot(rf_model)
# importance(rf_model)
# getTree(rf_model, 1, labelVar=T)

f.mat = cbind(f.mat, exp(predict(rf_model,newdata=td.mat)))
# svm
library(e1071)
library(kernlab)

svm_tune = tune.svm(SalePrice~.,data=d.mat, kernel="linear", cost=10^(-3:0),
                    tunecontrol = tune.control(cross=5,best.model=T))
svm_model = svm_tune$best.model
f.mat = cbind(f.mat, exp(predict(svm_model,newdata=td.mat)))

# svm_tune = tune.svm(SalePrice~.,data=svd.mat, kernel="linear", cost=10^(-3:0),
#                     tunecontrol = tune.control(cross=5,best.model=T))
# svm_model = svm_tune$best.model
# f.mat = cbind(f.mat, exp(predict(svm_model,newdata=tsvd.mat)))
# gradient Boosting : n.trees
library(gbm)

idx = sample(1:nrow(d.mat),size=nrow(d.mat)*0.7)
tr.mat = d.mat[idx,]; ts.mat = d.mat[-idx,]; err=NULL
# tr.mat = svd.mat[idx,]; ts.mat = svd.mat[-idx,]; err=NULL
# n = nt.vec[5]
nt.vec = seq(100,1000,by=100)
for(n in nt.vec){
  model = gbm(SalePrice~.,data=tr.mat,distribution="gaussian",cv.folds=5,n.trees=n,train.fraction=0.5)
  pval = predict.gbm(model,newdata = ts.mat)
  err = c(err,mean((pval-ts.mat$SalePrice)^2))
  plot(err)
}
opt.nt = nt.vec[which.min(err)]
gbm_model = gbm(SalePrice~.,data=d.mat,distribution="gaussian",n.trees=opt.nt,train.fraction=0.5)
f.mat = cbind(f.mat,exp(predict.gbm(gbm_model,newdata = td.mat)))



#### save
loglm_submission = data.frame(Id=TData$Id, SalePrice=f.mat[,1])
loglsso_submission = data.frame(Id=TData$Id, SalePrice=f.mat[,2])
logrid_submission = data.frame(Id=TData$Id, SalePrice=f.mat[,3])
logscad_submission = data.frame(Id=TData$Id, SalePrice=f.mat[,4])
logrf_submission = data.frame(Id=TData$Id, SalePrice=f.mat[,5])
logsvm_submission = data.frame(Id=TData$Id, SalePrice=f.mat[,6])
loggbm_submission = data.frame(Id=TData$Id, SalePrice=f.mat[,7])
# ridge_submission = data.frame(Id=TData$Id, SalePrice=f.mat[,1])
logesm_submission = data.frame(Id=TData$Id, SalePrice=(f.mat[,6]+f.mat[,7])/2)

setwd('C:/Users/HSMOON/Desktop/mygit/data-mining/house-prices/house-prices-advanced-regression-techniques/sumbmission/')
getwd()
write.csv(loglm_submission,'loglm_submission.csv',row.names=FALSE)
write.csv(loglsso_submission,'loglsso_submission.csv',row.names=FALSE)
write.csv(logrid_submission,'logrid_submission.csv',row.names=FALSE)
write.csv(logscad_submission,'logscad_submission.csv',row.names=FALSE)
write.csv(logrf_submission,'logrf_submission.csv',row.names=FALSE)
write.csv(logsvm_submission,'logsvm_submission.csv',row.names=FALSE)
write.csv(loggbm_submission,'loggbm_submission.csv',row.names=FALSE)
write.csv(logesm_submission,'logesm_submission.csv',row.names=FALSE)


# write.csv(f.mat,file="C:/Users/HSMOON/Desktop/mygit/data-mining/houseprice.csv")
# save.image(file = "C:/Users/HSMOON/Desktop/mygit/data-mining/houseprice.RData")
