rm(list=ls())
train = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/house-prices/house-prices-advanced-regression-techniques/train.csv",header=T)
test = read.csv("C:/Users/HSMOON/Desktop/mygit/data-mining/house-prices/house-prices-advanced-regression-techniques/test.csv",header=T)
cat("train :", colSums(is.na(train)), "test :", colSums(is.na(test)))


