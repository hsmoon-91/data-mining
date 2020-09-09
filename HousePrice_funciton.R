## Data cleansing
## missing data
clsn.fun = function(Data){
  # remove "Id"
  df = Data[,-1]
  # replace NA->none
  ## Alley
  replace(df$Alley,is.na(df$Alley),'None')->df$Alley
  ## Basement
  replace(df$BsmtQual,is.na(df$BsmtQual),'None')->df$BsmtQual
  replace(df$BsmtCond,is.na(df$BsmtCond),'None')->df$BsmtCond
  # cbind(df$BsmtCond[949],df$BsmtExposure[949])
  df$BsmtExposure[949] = "No"
  replace(df$BsmtExposure,is.na(df$BsmtExposure),'None')->df$BsmtExposure
  replace(df$BsmtFinType1,is.na(df$BsmtFinType1),'None')->df$BsmtFinType1
  replace(df$BsmtFinType2,is.na(df$BsmtFinType2),'None')->df$BsmtFinType2
  ## fire
  replace(df$FireplaceQu,is.na(df$FireplaceQu),'None')->df$FireplaceQu
  ## garage
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
  ### LotFrontage
  replace(df$LotFrontage,is.na(df$LotFrontage),mean(log(df$LotFrontage),na.rm=T))->df$LotFrontage # mean value
  print(list(dim(Data),str(Data),colSums(Data))
}
