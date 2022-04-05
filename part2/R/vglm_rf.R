library(VGAM)
library(randomForest)
m1 <- vglm(OverallCond~SalePrice,multinomial,data=house_data_v1_cleaned)
m1
summary(m1)



house_data_v1_cleaned$OverallCond <- factor(house_data_v1_cleaned$OverallCond)
house_data_v1_cleaned <- house_data_v1_cleaned[,-1]
house_data_v1_cleaned <- house_data_v1_cleaned[,-16]
# data partition 80/20 split with respect to the response variable
trainIndex <- createDataPartition(house_data_v1_cleaned$OverallCond, p=0.8, list = F, times =1)
Xtrain <- house_data_v1_cleaned[trainIndex, -11]
ytrain <- house_data_v1_cleaned[trainIndex,11]
Xtest <- house_data_v1_cleaned[-trainIndex, -11]
ytest <- house_data_v1_cleaned[-trainIndex,11]
ytrain <- unlist(ytrain)
ytest <- unlist(ytest)
m2 <- randomForest(x=Xtrain, y = ytrain)


