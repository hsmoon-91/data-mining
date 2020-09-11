## TData cleansing
## missing TData
clsn.fun = function(TData){
  # remove "Id"
  tdf = TData[,-1]
  # replace NA->none
  ## Alley
  replace(tdf$Alley,is.na(tdf$Alley),'None')->tdf$Alley
  ## Basement
  cbind(which(is.na(tdf$BsmtQual)),which(is.na(tdf$BsmtCond)),which(is.na(tdf$BsmtExposure)))
  replace(tdf$BsmtQual,is.na(tdf$BsmtQual),'None')->tdf$BsmtQual
  replace(tdf$BsmtCond,is.na(tdf$BsmtCond),'None')->tdf$BsmtCond

  tdf$BsmtExposure[949] = "No"
  replace(tdf$BsmtExposure,is.na(tdf$BsmtExposure),'None')->tdf$BsmtExposure
  replace(tdf$BsmtFinType1,is.na(tdf$BsmtFinType1),'None')->tdf$BsmtFinType1
  replace(tdf$BsmtFinType2,is.na(tdf$BsmtFinType2),'None')->tdf$BsmtFinType2
  ## fire
  replace(tdf$FireplaceQu,is.na(tdf$FireplaceQu),'None')->tdf$FireplaceQu
  ## garage
  replace(tdf$GarageType,is.na(tdf$GarageType),'None')->tdf$GarageType
  replace(tdf$GarageYrBlt,is.na(tdf$GarageYrBlt),0)->tdf$GarageYrBlt
  replace(tdf$GarageFinish,is.na(tdf$GarageFinish),'None')->tdf$GarageFinish
  replace(tdf$GarageQual,is.na(tdf$GarageQual),'None')->tdf$GarageQual
  replace(tdf$GarageCond,is.na(tdf$GarageCond),'None')->tdf$GarageCond
  ## pool
  replace(tdf$PoolQC,is.na(tdf$PoolQC),'None')->tdf$PoolQC
  ## fence
  replace(tdf$Fence,is.na(tdf$Fence),'None')->tdf$Fence
  ## miscfeature
  replace(tdf$MiscFeature,is.na(tdf$MiscFeature),'None')->tdf$MiscFeature
  ## LotFrontage / MasVnrType / MasVnrArea / Electrical
  ### MasVnrType : None / MasVnrArea : 0
  replace(tdf$MasVnrType,is.na(tdf$MasVnrType),'None')->tdf$MasVnrType
  replace(tdf$MasVnrArea,is.na(tdf$MasVnrArea),0)->tdf$MasVnrArea
  ### Electrical
  table(tdf$Electrical)
  tdf = tdf[-which(is.na(tdf$Electrical)),] # remove NA
  ### LotFrontage
  replace(tdf$LotFrontage,is.na(tdf$LotFrontage),mean(log(tdf$LotFrontage),na.rm=T))->tdf$LotFrontage # mean value
  print(list(dim(TData),str(TData),colSums(TData))
}
