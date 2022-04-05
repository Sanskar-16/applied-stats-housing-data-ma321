source('part-2.R')

library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(broom)

summary(house.data)

# partitioning the model for training and testing set (80/20 split)
samp <- createDataPartition(house.data$SalePrice, p = 0.8, list = FALSE)
training <- house.data[samp,]
testing <- house.data[-samp,]
x_test <- testing[,1:43]
y_test <- testing[,44]

set.seed(16)

# linear regression model using all the variables (regression based)
lm_full_model <- train(SalePrice~., data = training, method = "lm")

# random forest model using all the variables (tree based)
rf_full_model <- train(SalePrice~., data = training, method = "rf")

# checking most important variables for the linear regression model
lm_imp<- varImp(lm_full_model, xlim = c(50, 100))
ggplot(lm_imp)
plot(lm_imp, top = 10)
lm_imp

# checking most important variables for the random forest model
rf_imp <- varImp(rf_full_model, xlim = c(20,100))
ggplot(rf_imp)
plot(rf_imp, top = 10)
rf_imp

# Now that we know what the most important variables are with respect to the dataset,
# we can model accordingly to get the most accurate model using both, a refression 
# model and a tree based model.

# linear regression model using the most important variables out of all of them (regression based)
lm_selected_model <- train(SalePrice~OverallQual+X2ndFlrSF+OverallCond+X1stFlrSF+Fireplaces+BsmtQual+KitchenQual+Neighborhood+BldgType, 
                           data = training, method = "lm")

# random forest model using the most important variables out of all of them (tree based)
rf_selected_model <- train(SalePrice~OverallQual+X2ndFlrSF+OverallCond+X1stFlrSF+Fireplaces+BsmtQual+KitchenQual+Neighborhood,
                           data = training, method = "rf")

# getting the insight about the model
lm_full_model$finalModel
lm_selected_model$finalModel
rf_full_model$finalModel
rf_selected_model$finalModel

# evaluating the model based just on the metrics
lm_selected_model$results
rf_selected_model$results

# listing models to compare them agaisnt each other
model_list <- list(lm = lm_selected_model, rf = rf_selected_model)
res <- resamples(model_list)
summary(res)

compare_models(lm_selected_model, rf_selected_model)

# making predictions for SalePrice based on the train/test data
pred_lm <- as.data.frame(predict(lm_selected_model, x_test))
names(pred_lm)[1] = "fit"
pred_lm$real = y_test
pred_lm$error = pred_lm$fit - pred_lm$real
mean(pred_lm$error^2)
# [1] 0.1344096

pred_rf <- as.data.frame(predict(rf_selected_model, x_test))
names(pred_rf)[1] = "fit"
pred_rf$real = y_test
pred_rf$error = pred_rf$fit - pred_rf$real
mean(pred_rf$error^2)
# [1] 0.1318999

print("Section 3b")

#Leave-One-Out Cross-Validation

#defines the cross-validation method
cvmethod <- trainControl(method = 'LOOCV')

#creates a linear regression model and performs the LOOCV method
modelloocv1 <- train(SalePrice ~ Fireplaces+TotRmsAbvGrd+Foundation+X1stFlrSF, data=house.data, method= 'lm', trControl=cvmethod, na.action=na.exclude)
modelloocv2 <- train(SalePrice ~., data=house.data, method= 'lm', trControl=cvmethod, na.action=na.exclude)

#shows the results of the LOOCV
print(modelloocv1)
print(modelloocv2)

#K-Fold Cross Validation

#defines the cross-validation method
cvmethod <- trainControl(method = 'cv', number=10)

#creates a linear regression model and performs the k-fold classification method
modelkfold1 <- train(SalePrice ~Fireplaces+TotRmsAbvGrd+Foundation+X1stFlrSF, data=house.data, method= 'lm', trControl=cvmethod, na.action=na.exclude)
modelkfold2 <- train(SalePrice ~., data=house.data, method= 'lm', trControl=cvmethod, na.action=na.exclude)

#shows the predictions made across each fold from 1 to 10
modelkfold1$resample
modelkfold2$resample

#shows the results of the k-fold classification method
print(modelkfold1)
print(modelkfold2)

# cleaning the environment
rm(lm_imp,
   model_list,
   predictions_rf,
   res,
   rf_imp,
   samp,
   testing,
   training,
   x_test,
   y_test,
   pred_lm
   )