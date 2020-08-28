rm(list=ls())
data = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/titan.tr.csv",header=T)
# data = read.csv("C:/Users/HSMOON/Desktop/seminar/analysis/Titanic/kaggle data/train.csv",header=T)
dim(data); names(data); summary(data); str(data)

# install.packages("data.table")
library(data.table)
data.table(data)
## missing data
mis.mat = NULL
for(j in 1:dim(data)[2]){ if(j==11) break; mis.mat = rbind(mis.mat,data[data[,j]=="?",]) }
dim(mis.mat);

## imputation
# "age" : mean
mdata =  data[data[,"age"]=="?",]
dim(mdata)
mat = cbind(as.numeric(data[data[,"age"]!="?","age"]),as.factor(data[data[,"age"]!="?","sex"]),
            as.factor(data[data[,"age"]!="?","pclass"]))
str(mat)
nmat = cbind(as.numeric(data[data[,"age"]=="?","age"]),data[data[,"age"]=="?","sex"],
             data[data[,"age"]=="?","pclass"])

names(data)
cbind(data$name,data$sibsp,data$parch)
data$name
## data cleansing
dim(data)
str(data)
data$ticket = as.factor(data$ticket)
levels(data$ticket) = c(1:703)
data$pclass = as.factor(data$pclass)
data$sex = as.factor(data$sex)
levels(data$sex) = 1:2
data$age = as.numeric(data$age)
data$sibsp = as.factor(data$sibsp)
data$parch = as.factor(data$parch)
data$fare = as.numeric(data$fare)
data[data[,"fare"]=="?",]
data$ticket[c(850:900)]
data$ticket[856] ## 475
sum(data$ticket == 856)

str(data)
sum(is.na(data))
sum(is.na(data$age))
data$cabin
length(sort(data$fare))
length(data$fare)

data$ticket
tf.mat = cbind(data$ticket,data$fare,data$pclass,data$sibsp,data$parch)
colnames(tf.mat) = c("ticket","fare","pclass","sibsp","parch")
tf.mat
tf.mat[tf.mat[,1]%in%c(1:100),]
tf.mat[tf.mat[,1]%in%c(101:200),]
tf.mat[tf.mat[,1]%in%c(201:300),]
tf.mat[tf.mat[,1]%in%c(301:400),]
tf.mat[tf.mat[,1]%in%c(401:500),]
tf.mat[tf.mat[,1]%in%c(501:600),]
tf.mat[tf.mat[,1]%in%c(601:700),]
tf.mat[tf.mat[,1]%in%c(701:800),]
tf.mat[tf.mat[,1]%in%c(801:900),]

tf.mat[tf.mat[,3]==3&tf.mat[,4]==1&tf.mat[,5]==1,]

## train / test data for 'age' variable
train = data[is.na(data$age)==F,]
n = dim(train)[1]
idx = sample(1:n,size=0.7*n,replace=T)
tmat = train[idx,]; nmat = train[-idx,]
tmat_age = train[idx,"age"]; nmat_age = train[-idx,"age"]
test = data[is.na(data$age)==T,]
dim(train); dim(test)
train_age = train$age
test_age = test$age

## missing data : age
# install.packages("class")
library(class)
result = numeric()
k = 5:25
for(i in k){
  pre = knn(train, test, train_age, k=i)
  tab = table(pre, test_age)
  result[i-4] = (tab[1,1]+tab[2,2])/sum(tab)
}
result
sort(result, decreasing = T)
which(result==max(result)) 

