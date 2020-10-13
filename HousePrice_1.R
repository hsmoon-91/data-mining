rm(list=ls())
train = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/house-prices/house-prices-advanced-regression-techniques/train.csv",header=T)
test = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/house-prices/house-prices-advanced-regression-techniques/test.csv",header=T)
`%notin%` <- Negate(`%in%`)

# combine train and test data for preprocessing
all_data <- rbind(select(train,MSSubClass:SaleCondition),select(test,MSSubClass:SaleCondition))
colSums(is.na(all_data))
str(all_data)
# discrete var
fac.name= c("MSSubClass","MSZoning","Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope",
           "Neighborhood","Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond",
           "YearBuilt","YearRemodAdd","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","ExterQual",
           "ExterCond","Foundation","BsmtCond","BsmtQual","BsmtExposure","BsmtFinType1","BsmtFinType2","Heating",
           "HeatingQC","CentralAir","Electrical","KitchenQual","Functional","FireplaceQu","GarageType",
           "GarageYrBlt","GarageFinish","GarageQual","GarageCond","PavedDrive","Fence","PoolQC","MiscFeature","MoSold",
           "YrSold","SaleType","SaleCondition")
# continuous var
con.name = NULL
for(i in 1:dim(all_data)[2]){
  if(colnames(all_data)[i] %notin% fac.name){ con.name= c(con.name,colnames(all_data)[i]) }
}
