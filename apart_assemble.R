rm(list=ls())
# install.packages("randomForest")
library(ipred)
library(gbm)
library(randomForest)
library(readxl)
###########################################################################################
find_M <- function(m){
  fit.boost         <- gbm(as.factor(SGG)~.,data=train,distribution="multinomial",
                           n.trees=m)
  pred.prob         <- predict(fit.boost,x_test,type="response",n.trees=m)  #확률로 나옴
  pred.prob         <- matrix(pred.prob, ncol = length(fit.boost$classes))
  pred              <- fit.boost$classes[apply(pred.prob,1,which.max)]
  err_rate          <- mean(pred!=y_test)
  return(err_rate)
}
path = "C:\\Users\\HSMOON\\Desktop\\mygit\\data-mining\\3.아파트_주소기반_주택지역특성시세정보_DB.xlsx"
sheet.names = excel_sheets(path)
df1 = read_excel(path,sheet.names[1])
df2 = read_excel(path,sheet.names[2])
df3 = read_excel(path,sheet.names[3])
df4 = read_excel(path,sheet.names[4])
data = Reduce(function(x,y) merge(x,y,all=F),list(df1,df2,df3,df4))

data$LAND_ACCESS = as.numeric(as.character(data$LAND_ACCESS)) 
data$UNIT_NUM = as.numeric(as.character(data$UNIT_NUM)) 
data = data[complete.cases(data), c("SGG","PRICE_GEN", "FLOOR", "PRIV_AREA", "PUB_AREA",
                                    "SUM_AREA","UNIT_NUM", "PARK_NUM", "LAND_ACCESS", "SUB_DIST")]  

head(data, 5)
###########################################################################################
en_data               <- subset(data,!grepl("광진구", SGG))
en_data$SGG           <- ifelse(en_data$SGG == "성동구", "A",
                                ifelse(en_data$SGG == "용산구", "B", "C"))
table(en_data$SGG)
# column type 지정
en_data$SGG           <- as.factor(as.character(en_data$SGG))

head(en_data)
###########################################################################################
set.seed(10)
sample_num            <- sample(1:nrow(en_data), 0.7*nrow(en_data))
train                 <- en_data[sample_num,]
test                  <- en_data[-sample_num,]
x_train               <- train[,-1]
x_test                <- test[,-1]
y_train               <- train[,1]
y_test                <- test[,1]
# bagging ########################################################################################
fit.bagg              <- ipredbagg(as.factor(y_train), x_train, data=train, nbagg=1000)  
fit.bagg
pred                  <-predict(fit.bagg, newdata = x_test)
table(pred,y_test)
cat("오분류율 = ", mean(pred!=y_test)*100,"%")
pred2                <-predict(fit.bagg,x_test,type="prob")
head(pred2)
# Boosting ########################################################################################
fit.boost             <- gbm(SGG~.,data=train,distribution="multinomial",n.trees=500)
summary(fit.boost)
pred.prob             <- predict(fit.boost,x_test,type="response",n.trees=500)  
pred.prob             <- matrix(pred.prob,ncol=3)                               
pred.prob
# 각 범주에 속할 확률 산출
colnames(pred.prob)   <- levels(y_train)
head(pred.prob)
pred                  <-apply(pred.prob,1,which.max)
pred                  <-ifelse(pred==1,"A",ifelse(pred==2,"B","C"))
table(pred,y_test)
cat("오분류율 = ", mean(pred!=y_test)*100, "%")


find_M(50)
find_M(100)
find_M(250)
find_M(500)
find_M(750)
find_M(1000)
find_M(1500)
find_M(2000)
# random forest ########################################################################################
set.seed(100)
fit.rf                <- randomForest(as.factor(SGG)~.,data=train, ntree=1000,  mtry=3)
pred                  <-predict(fit.rf,x_test)
table(pred,y_test)
cat("오분류율 = ", mean(pred!=y_test)*100, "%")
MAPE                  <- NULL
for(i in 1:ncol(x_train)){
  temp_rf           <- randomForest(as.factor(SGG)~.,data=train, ntree=1000, mtry=i)  
  pred              <- predict(temp_rf,x_test)
  MAPE[i]           <- mean(pred!=y_test)
}
round(MAPE,2)
plot(x=seq(1,ncol(x_train)),y=MAPE,type="l",xlab="변수개수")


which.min(MAPE)
fit = randomForest(as.factor(SGG)~.,data=train,ntree=1000,mtry=which.min(MAPE))
pred                  <-predict(fit,x_test)
table(pred,y_test)
