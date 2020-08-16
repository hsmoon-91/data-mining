rm(list=ls())
data = read.csv("C:/Users/HSMOON/Desktop/seminar/analysis/Titanic/titan.tr.csv",header=T)
# data = read.csv("C:/Users/HSMOON/Desktop/seminar/analysis/Titanic/kaggle data/train.csv",header=T)
dim(data); names(data); summary(data); str(data)

library(data.table)
data.table(data)
## missing data
mis.mat = NULL
for(j in 1:dim(data)[2]){ if(j==11) break; mis.mat = rbind(mis.mat,data[data[,j]=="?",]) }
dim(mis.mat); sum(data=="?")

## imputation
# "age" : mean
data[data[,"age"]=="?",]
mat = cbind(as.numeric(data[data[,"age"]!="?","age"]),data[data[,"age"]!="?","sex"],
            data[data[,"age"]!="?","pclass"])

nmat = cbind(as.numeric(data[data[,"age"]=="?","age"]),data[data[,"age"]=="?","sex"],
             data[data[,"age"]=="?","pclass"])

mat[,2]