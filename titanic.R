rm(list=ls())
Data = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/titanic/titan.tr.csv",header=T)
# data = read.csv("C:/Users/HSMOON/Desktop/seminar/analysis/Titanic/kaggle data/train.csv",header=T)
data = Data
dim(data); names(data); summary(data); str(data)
# install.packages("data.table")
# library(data.table)
# data.table(data)
## missing data
data = data[,c(-1,-11)]
mis.mat = NULL
for(j in 1:dim(data)[2]){ print(j); mis.mat = rbind(mis.mat,data[data[,j]=="?",]) }
dim(mis.mat);
## ? -> NA

for(j in 1:dim(data)[2]){
  replace(data[,j],data[,j]=="?",NA) -> data[,j]
}

## imputation
# "age" : mean
# mdata =  data[data[,"age"]=="?",]
# dim(mdata)
# mat = cbind(as.numeric(data[data[,"age"]!="?","age"]),as.factor(data[data[,"age"]!="?","sex"]),
#             as.factor(data[data[,"age"]!="?","pclass"]))
# str(mat)
# nmat = cbind(as.numeric(data[data[,"age"]=="?","age"]),data[data[,"age"]=="?","sex"],
#              data[data[,"age"]=="?","pclass"])
# names(data)


## data cleansing
dim(data)
str(data)
for(i in 1:dim(data)[2]){
  if(i %in%c(5,6,7,9)) { data[,i] = as.numeric(data[,i]) }
  if(i %in%c(1,2,4,8,10)) { data[,i] = as.factor(data[,i])}
}
str(data)
# levels(data$ticket) = c(1:703)
# levels(data$sex) = c(1:2)
# levels(data$embarked) = c(1:3)
head(data)

# data$ticket[856] ## 475
# sum(data$ticket == 856)

str(data)
sum(is.na(data))
sum(is.na(data$age))
data$cabin
length(sort(data$fare))
length(data$fare)
str(data)

tf.mat = NULL
names(data)
tf.mat = data[,c(8,9,2,6,7,10)]; names(tf.mat)
str(tf.mat); head(tf.mat)

vmat = tf.mat[tf.mat[,3]==3 & tf.mat[,4]==0 & tf.mat[,5]==0,] # pclass, sibsp, parch : 3,0.0
data[is.na(data[,"fare"]),"fare"] = mean(vmat[,2],na.rm=T)
names(data)

tf.mat[is.na(tf.mat[,6]),]
tf.mat[tf.mat[,3]==1 & tf.mat[,4]==0 & tf.mat[,5]==0,]
# maybe 'embarked' 3 frequency
# install.packages("frequency")
# library(frequency)
names(data)
data[is.na(data[,10]),]
table(data$pclass, data$embarked)
table(data$sex, data$embarked)
table(data$sibsp, data$embarked)
names(tf.mat)
vmat = tf.mat[,c(3,4,5)]
tab = table(vmat)
data[is.na(data[,"embarked"]),"embarked"] = c("S","S")

# k = "Soholt, Mr. Peter Andreas Lauritz Andersen"
# gsub('(.*, )|(\\..*)', '',k)

data$title = gsub('(.*, )|(\\..*)', '', data$name)
table(data$sex,data$title)
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
data$title[data$title == 'Mlle']        = 'Miss'
data$title[data$title == 'Ms']          = 'Miss'
data$title[data$title == 'Mme']         = 'Mrs' 
data$title[data$title %in% rare_title]  = 'Rare Title'
data$title = as.factor(data$title)
# levels(data$title) = c(1:5)

data$fsize = data$sibsp+data$parch+1 # family size
dim(data);names(data)


## train / test data for 'age' variable
  train = data[is.na(data$age)==F,-3];test = data[is.na(data$age)==T,-3]
  train_age = train$age;
  test$age = 0
  dim(train); dim(test)

n = dim(train)[1]
idx = sample(1:n,size=0.7*n,replace=T)
tmat = train[idx,]; nmat = train[-idx,]
tmat_age = train[idx,"age"]; nmat_age = train[-idx,"age"]

## missing data : age
# install.packages("class")
## knn : discrete & continous
# install.packages("FNN")
library(FNN)
library(class)
# result = numeric()
# k = 5:25
# for(i in k){
#   pre = knn(tmat, nmat, tmat_age, k=i)
#   tab = table(pre, nmat_age)
#   result[i-4] = (tab[1,1]+tab[2,2])/sum(tab)
# }
# result
# sort(result, decreasing = T)
# which(result==max(result)) 

names(train)
xmat.train = model.matrix(age ~ ., data=tmat)[,-1]
#검증데이터 행렬
xmat.test = model.matrix(age ~ ., data=nmat)[,-1]
k = 5:25; rst = NULL
for(i in k){
  pre = knn.reg(xmat.train,xmat.test,tmat_age,k=i)
  mse = mean((nmat_age-pre$pred)^2)
  rst = c(rst,mse)
}
opt.k = k[which.min(rst)]


train.model = model.matrix(age~.,data=train)[,-1]
test.model = model.matrix(age~.,data=test)[,-1]
pre.val = knn.reg(train.model,test.model,train_age,k=opt.k)
test$age = pre.val$pred
dim(data); names(data)
ndata = rbind(train,test)
sum(is.na(ndata))

ndata$child[ndata$age >= 19] = "adult"
ndata$child[ndata$age < 19] = "child"
ndata$child = as.factor(ndata$child)

ndata = ndata[,-7]
str(ndata)
dim(ndata)

new = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/titanic/titan.ts.csv",header=T)
new = new[,c(-1,-11)]
dim(new)
mis.mat = NULL
for(j in 1:dim(new)[2]){ print(j); mis.mat = rbind(mis.mat,new[new[,j]=="?",]) }
dim(mis.mat);
## ? -> NA
for(j in 1:dim(new)[2]){
  replace(new[,j],new[,j]=="?",NA) -> new[,j]
}
str(new)
for(i in 1:dim(new)[2]){
  if(i %in%c(5,6,7)) { new[,i] = as.numeric(new[,i]) }
  if(i %in%c(1,2,4,8,10)) { new[,i] = as.factor(new[,i])}
}

dim(new)
new$title = gsub('(.*, )|(\\..*)', '', new$name)
table(new$sex,new$title)
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
new$title[new$title == 'Mlle']        = 'Miss'
new$title[new$title == 'Ms']          = 'Miss'
new$title[new$title == 'Mme']         = 'Mrs' 
new$title[new$title %in% rare_title]  = 'Rare Title'
new$title = as.factor(new$title)
# levels(new$title) = c(1:5)

new$fsize = new$sibsp+new$parch+1 # family size

new = new[,-3]
nrow(new[is.na(new$age),]) == sum(is.na(new))

train = new[is.na(new$age)==F,];test = new[is.na(new$age)==T,]
train_age = train$age;
test$age = 0
dim(train); dim(test)

n = dim(train)[1]
idx = sample(1:n,size=0.7*n,replace=T)
tmat = train[idx,]; nmat = train[-idx,]
tmat_age = train[idx,"age"]; nmat_age = train[-idx,"age"]
dim(tmat);dim(nmat)
names(train)
xmat.train = model.matrix(age ~ ., data=tmat)[,-1]
#검증데이터 행렬
xmat.test = model.matrix(age ~ ., data=nmat)[,-1]
dim(xmat.test); dim(xmat.train)
k = 5:25; rst = NULL
for(i in k){
  pre = knn.reg(xmat.train,xmat.test,tmat_age,k=i)
  mse = mean((nmat_age-pre$pred)^2)
  rst = c(rst,mse)
}
opt.k = k[which.min(rst)]


train.model = model.matrix(age~.,data=train)[,-1]
test.model = model.matrix(age~.,data=test)[,-1]
dim(train.model);dim(test.model)
pre.val = knn.reg(train.model,test.model,train_age,k=opt.k)
test$age = pre.val$pred

nnew = rbind(train,test)
sum(is.na(nnew))

nnew$child[nnew$age >= 19] = "adult"
nnew$child[nnew$age < 19] = "child"
nnew$child = as.factor(nnew$child)
nnew_sur = nnew$survived

str(ndata)
str(nnew)
nnew = nnew[,-7]
dim(nnew);dim(ndata)

ndata.df = model.matrix(survived~.,data=ndata)[,-1]
ndata.df = as.data.frame(ndata.df)
ndata.df = cbind(ndata$survived,ndata.df); colnames(ndata.df)[1] = "survived"

nnew.df = model.matrix(survived~.,data=nnew)[,-1]
nnew.df = as.data.frame(nnew.df)
nnew.df = cbind(nnew$survived,nnew.df); colnames(nnew.df)[1] = "survived"
dim(ndata.df); dim(nnew.df)

# 1. logistic
# model = model.matrix(survived~.,data=ndata)[,-1]

fit = glm(survived~., family="binomial",data=ndata.df)
summary(fit)
pre = predict(fit,newdata=nnew.df)
pre[pre<0.5]=0; pre[pre>=0.5]=1
nnew.df$survived
tab = table(pre,nnew.df$survived)
acc = (tab[1,1]+tab[2,2])/sum(tab)

# 2. penalty : LASSO, ridge, SCAD, MCP
library(glmnet)
library(ncvreg)
# train / valid
x.mat = as.matrix(ndata.df[,-1]); y.vec = drop(ndata.df[,1])
n.mat = as.matrix(nnew.df[,-1])
# idx = sample(dim(ndata.df)[1],size=0.7*dim(ndata.df)[1],replace=T)
# tx.mat = as.matrix(ndata.df[idx,-1]); vx.mat = as.matrix(ndata.df[-idx,-1])
# ty.vec = drop(ndata.df[idx,1]); vy.vec = drop(ndata.df[-idx,1])
# LASSO
fit = cv.glmnet(x.mat,y.vec,family="binomial")
val = as.factor(predict(fit,newx=n.mat,type="class",lambda=fit$lambda.min))
tab = table(val,nnew.df$survived)
acc = (tab[1,1]+tab[2,2])/sum(tab)
             
# ridge 
fit = cv.glmnet(x.mat,y.vec,family="binomial",alpha=0) 
val = as.factor(predict(fit,newx=n.mat,type="class",lambda=fit$lambda.min))
tab = table(val,nnew.df$survived)
acc = (tab[1,1]+tab[2,2])/sum(tab)

# SCAD
fit = cv.ncvreg(x.mat,y.vec,family="binomial",penalty="SCAD")
val = as.factor(predict(fit,X=n.mat,type="class",lambda=fit$lambda.min))
tab = table(val,nnew.df$survived)
acc = (tab[1,1]+tab[2,2])/sum(tab)

# MCP
fit = cv.ncvreg(x.mat,y.vec,family="binomial",penalty="MCP")
val = as.factor(predict(fit,X=n.mat,type="class",lambda=fit$lambda.min))
tab = table(val,nnew.df$survived)
acc = (tab[1,1]+tab[2,2])/sum(tab)

library(MASS)
library(e1071)
library(rpart)
library(randomForest)
library(adabag)
library(xgboost)
library(kknn)
library(caret)


fitControl = trainControl(method="cv",number=5,classProbs=TRUE,summaryFunction=twoClassSummary)
# 3. random forest
model_rf = train(make.names(survived)~.,data=ndata.df,method="rf",metric="ROC",trControl=fitControl)
pred = predict(model_rf,newdata=nnew.df)
levels(pred) = levels(ndata.df$survived)
tab = table(pred,nnew.df$survived)
acc = (tab[1,1]+tab[2,2])/sum(tab)

# 4. svm
# 5. XGBoosting
tdata = data.matrix(ndata.df); ndata = data.matrix(nnew.df)
train_data = tdata[,-1]; train_labels = tdata[,1]-1
test_data = ndata[,-1]; test_labels = ndata[,1]-1
dtrain = xgb.DMatrix(data=train_data,label=train_labels)
dtest = xgb.DMatrix(data=test_data,label=test_labels)
# default parameter
params = list(booster="gbtree",objective="binary:logistic",eta=0.3,gamma=0,max_depth=6)
xgbcv = xgb.cv(data = dtrain,nrounds=100,nfold=5,metrics=list("rmse","auc"),objective="binary:logistic")
which.min(as.matrix(xgbcv$evaluation_log)[,6])
model_xgb = xgb.train(params=params,data=dtrain,which.min(as.matrix(xgbcv$evaluation_log)[,6]),watchlist=list(val=dtest,train=dtrain),eval_metric="error")
xbgpred = predict(model_xgb,dtest)
pred = as.factor(ifelse (xbgpred >= 0.5,1,0))
tab = table(pred,nnew.df$survived)
acc = (tab[1,1]+tab[2,2])/sum(tab)
acc

# "svmRadial" "rf", "C5.0", "xgbDART"
model <- train(
  survived ~., data = ndata.df, method = "svmRadial",
  trControl = trainControl("cv", number = 5)
)
# Best tuning parameter
model$bestTune
library(dplyr)
predicted.classes <- model %>% predict(nnew.df)
tab = table(predicted.classes,nnew.df$survived)
acc = (tab[1,1]+tab[2,2])/sum(tab)
acc
library(gbm)
library(randomForest)
rf_model = randomForest(as.data.frame(ndata.df[,-1]), y = drop(ndata.df[,1]),ntree=500,importance=TRUE)
pre = predict(rf_model, newdata=nnew.df, type="class")
length(pre)
tab = table(pre,nnew.df$survived)
acc = (tab[1,1]+tab[2,2])/sum(tab)

rf_model = rfcv(as.data.frame(ndata.df[,-1]),drop(ndata.df[,1]),cv.fold=5)



b_model = gbm(survived~.,data=ndata.df,distribution="bernoulli",n.trees=100,cv.folds=5)