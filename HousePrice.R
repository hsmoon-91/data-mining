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
# cbind(df$BsmtCond[949],df$BsmtExposure[949])
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
df = df[,-73] # remove MoSold

df$MSSubClass = as.character(df$MSSubClass)
df$OverallQual = as.character(df$OverallQual)
df$OverallCond = as.character(df$OverallCond)

str(df); sum(is.na(df))
## dummy 
n.mat = model.matrix(SalePrice~.,data=df)[,-1]

# Method : svd - regression
# install.packages("svd")
library(svd)

