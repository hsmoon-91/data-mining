
# install.packages("installr") 
# library(installr)
# updateR()
rm(list=ls())
# update.packages(checkBuilt=TRUE)
# version
# packageStatus()
# install.packages("tree")
library(tree)
library(rpart)
library(rpart.plot)
library(C50)
library(party)
library(partykit)
library(readxl)

###########################################################################################
path = "C:\\Users\\HSMOON\\Desktop\\mygit\\data-mining\\3.아파트_주소기반_주택지역특성시세정보_DB.xlsx"
sheet.names = excel_sheets(path)
df1 = read_excel(path,sheet.names[1])
df2 = read_excel(path,sheet.names[2])
df3 = read_excel(path,sheet.names[3])
df4 = read_excel(path,sheet.names[4])
data = Reduce(function(x,y) merge(x,y,all=F),list(df1,df2,df3,df4))
names(data)
dim(data)
data$LAND_ACCESS = as.numeric(as.character(data$LAND_ACCESS)) 
data$UNIT_NUM = as.numeric(as.character(data$UNIT_NUM)) 

data = data[complete.cases(data), c("SGG","PRICE_GEN", "FLOOR", "PRIV_AREA", "PUB_AREA",
                                    "SUM_AREA","UNIT_NUM", "PARK_NUM", "LAND_ACCESS", "SUB_DIST")]  

head(data, 5)
###########################################################################################
tree_data = subset(data,!grepl("광진구", SGG))
tree_data$SGG = ifelse(tree_data$SGG == "성동구", "A",ifelse(tree_data$SGG == "용산구", "B", "C"))
table(tree_data$SGG)

tree_data$SGG = as.factor(as.character(tree_data$SGG))
head(tree_data)

###########################################################################################
set.seed(10)
sample.num = sample(1:nrow(tree_data),0.7*nrow(tree_data))
train = tree_data[sample.num,]
test = tree_data[-sample.num,]
dim(test); dim(train)
###########################################################################################
tree_ml = tree(SGG~.,data=train,split="deviance")
summary(tree_ml)
plot(tree_ml)
text(tree_ml, cex = 0.7)
###########################################################################################
tree_p = snip.tree(tree_ml,nodes=c(6))
plot(tree_p)
text(tree_p, all=T)
###########################################################################################
tree_p2 = prune.misclass(tree_ml)
plot(tree_p2)
###########################################################################################
fin.tr = prune.misclass(tree_ml, best=15)
plot(fin.tr)
text(fin.tr,cex=0.7)
###########################################################################################
par(mfrow=c(1,3))
#가지치기 안한 Tree
plot(tree_ml)
title(main="Full Tree", cex.main = 1)
text(tree_ml,cex=0.7)
#node수로 가지치기한 Tree
plot(tree_p)
title(main="Pruned Tree \n by  nodes", cex.main = 1)
text(tree_p, all=T, cex=0.7)
#끝마디수로 가지치기한 Tree
plot(fin.tr)
title(main="Pruned Tree \n by Terminal nodes", cex.main = 1)
text(fin.tr, cex=0.7)
###########################################################################################
yhat = predict(fin.tr,newdata=test,type="class")
ytest = test$SGG
table(yhat,ytest)
cat("오분류율 = ", mean(yhat!=ytest)*100,"%")

###########################################################################################
tree_plot = tree(SGG ~ LAND_ACCESS + UNIT_NUM, data = train,
                 control = tree.control(nobs = nrow(train), minsize = 50))
par(pty="s")
plot(train$LAND_ACCESS, train$UNIT_NUM, xlab="LAND_ACCESS", ylab="UNIT_NUM", type = "n")
text(train$LAND_ACCESS, train$UNIT_NUM, col = c(2:(length(levels(train$SGG)) + 1))[train$SGG], train$SGG)
partition.tree(tree_plot, add=TRUE, cex=1.5)

# cart #########################################################################################
cart_ml = rpart(SGG ~.,train)
cart_ml
rpart.plot(cart_ml, main = "Classification using CART")
yhat                  <- predict(cart_ml, newdata=test, type="class")
ytest                 <- test$SGG
table(yhat,ytest)
cat("오분류율 = ", mean(yhat!=ytest)*100,"%")

# C5.0 #########################################################################################
c5_ml = C5.0(as.factor(SGG) ~.,train)
summary(c5_ml)
# plot(c5_ml, width = 1194, height = 746)
plot(c5_ml, gp = gpar(fontsize = 7))
yhat = predict(c5_ml, newdata=test, type="class")
ytest = test$SGG
table(yhat,ytest)
cat("오분류율 = ",mean(yhat!=ytest)*100,"%")
# QUESET #########################################################################################
queset_ml = ctree(SGG ~., train, control = ctree_control(testtype=c("MonteCarlo")))
summary(queset_ml)
plot(queset_ml)
yhat = predict(queset_ml, newdata=test, type="response")
ytest = test$SGG
table(yhat,ytest)
cat("오분류율 = ",mean(yhat!=ytest)*100,"%")