source('part-1.R')
library(readr)
library(dplyr)
library(nnet) # multinational logistic regression
library(caret)
library(randomForest)
library(yardstick)
library(MASS)


# Class counts in the OverallCond
# setting the seed before the partition

table(house.data$OverallCond)

set.seed(10)
index <- createDataPartition(house.data$OverallCond, p=0.8, times = 1, list=F)
ytrain <-unlist(house.data[index,11])
ytest <- unlist(house.data[-index,11])
xtest <- house.data[-index,-11]
xtrain <- house.data[index,-11]

table(ytest)
table(ytrain)
# FILIPS garden ================================================================
summary(house.data)
# https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/

# making a copy of the data not to disturb later processes
mlr_data <- house.data
sapply(mlr_data %>% select_if(is.factor), function(x){length(unique(x))})
glimpse(mlr_data)
table(mlr_data$OverallQual)
# OverallQual has 8 unique values and natural order, good enough to use as
# numerical variable
mlr_data <- mlr_data %>%
  mutate(OverallQual = as.numeric(OverallQual))

# pick a reference level, Average has the highest frequency
mlr_data$OverallCond <- relevel(mlr_data$OverallCond, ref = "Average")

# test/train split
mlr_train <- mlr_data[index,]
mlr_test <- mlr_data[-index,]

# source https://www.youtube.com/watch?v=QvnsTXfPenU
# Logistic regression predicts a binary variable, a natural extension is
# multinomial logistic regression, which essentially does the same but ensembles
# more than two classes, both are used for nominal target variables. Simply
# said, it asks the question whether a predictor contributes towards the final
# outcome or not, and how much to each class of the outcome.

# it would be reasonable to expect that neighborhood would contribute towards
# successfull prediction of overall condition, but many of it's categories
# ended up being insignificant and were polluting the model, which was causing
# issues given the small pool of data we have available w.r.t. the expanded
# predictors

# full model including all predictors
model_mlr_f <- multinom(OverallCond ~ . -Fireplaces -Heating -GarageType 
                         -LotConfig -Alley -LotArea -LotFrontage -PavedDrive
                         -Neighborhood -Exterior1st -HouseStyle,
                         data = mlr_train, trace = F)
# null model including only the intercept
model_mlr_n <- multinom( OverallCond ~ 1,
                         data = mlr_train, trace = F)

# output is muted, this takes about 30s to run
model_mlr_aic_b <- stepAIC(model_mlr_f, direction = "backward", trace=FALSE)
model_mlr_aic_f <- stepAIC(model_mlr_n,direction="forward", scope=list(upper=model_mlr_f,lower=model_mlr_n), trace=FALSE)

summary(model_mlr_aic_b)
summary(model_mlr_aic_f)
data.frame(model_mlr_aic_b = c(model_mlr_aic_b$AIC, model_mlr_aic_b$deviance),
           model_mlr_aic_f = c(model_mlr_aic_f$AIC, model_mlr_aic_f$deviance),
           row.names = c("AIC", "DEVIANCE"))
# by working on excluding non-significant predictors, it ended up being the case
# where forward and backward wrapper methods produced models with the same
# metrics

evaluate_model <- function(model, data){
  predicted <-
    model %>%
    predict(newdata = data, "class")
  
  correct <- data %>%
    bind_cols(., pred_oc = predicted) %>%
    dplyr::select(OverallCond, pred_oc) %>%
    mutate(correct = OverallCond == pred_oc) %>%
    group_by(correct) %>%
    summarise(count = n())
  
  print(correct)
  
  conf_mat_res <- data %>%
    bind_cols(., pred_oc = predicted) %>%
    dplyr::select(OverallCond, pred_oc) %>%
    conf_mat(OverallCond, pred_oc)
  
  average_tp <- sum(conf_mat_res$table[1,1])
  good_tp <- sum(conf_mat_res$table[2,2])
  poor_tp <- sum(conf_mat_res$table[3,3])
  
  average_tn <- sum(conf_mat_res$table[-1, -1])
  good_tn <- sum(conf_mat_res$table[-2,-2])
  poor_tn <- sum(conf_mat_res$table[-3,03])
  
  average_fp <- sum(conf_mat_res$table[1, -1])
  good_fp <- sum(conf_mat_res$table[2,-2])
  poor_fp <- sum(conf_mat_res$table[3,-3])
  
  average_fn <- sum(conf_mat_res$table[-1,1])
  good_fn <- sum(conf_mat_res$table[-2,2])
  poor_fn <- sum(conf_mat_res$table[-3,3])
  
  #source: https://towardsdatascience.com/confusion-matrix-for-your-multi-class-machine-learning-model-ff9aa3bf7826
  precision <- function(tp, fp){tp/(tp+fp)}
  recall <- function(tp, fn){tp/(tp+fn)}
  f1 <- function(tp, fp, fn){2*tp/(2*tp+fp+fn)}
  
  class_metrics <- data.frame(precision = c(precision(average_tp, average_fp), precision(good_tp, good_fp), precision(poor_tp, poor_fp)),
             recall = c(recall(average_tp, average_fn), recall(good_tp, good_fn), recall(poor_tp, poor_fn)),
             f1 = c(f1(average_tp, average_fp, average_fn), f1(good_tp, good_fp, good_fn), f1(poor_tp, poor_fp, poor_fn)),
             row.names = c("average", "good", "poor"))
  
  print(class_metrics)

  # source: https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/
  # calculating significance of individual variables
  z <- summary(model)$coefficients/summary(model)$standard.errors
  print('z values:')
  print(z)
  # 2-tailed z test
  print('p values:')
  p <- (1 - pnorm(abs(z), 0, 1))*2
  print(p)
  
  # ratio of P(choosing one categoty)/P(choosing baseline category) - relative risk
  # source https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
  relative_risk <- exp(coef(model))
  print("relative risk")
  print(relative_risk)
  
  data %>%
    bind_cols(., pred_oc = predicted) %>%
    dplyr::select(OverallCond, pred_oc) %>%
    conf_mat(OverallCond, pred_oc) %>%
    autoplot(type = "heatmap")
}

evaluate_model(model_mlr_aic_f, mlr_train)
evaluate_model(model_mlr_aic_f, mlr_test)


# From the output we can see the relation of predictors and the individual output
# classes C, |C|-1 in total because we have picked the most numerous one
# "Average" as being the reference class.
# For example, the log odds of having Average vs Good house condition seems to decrease
# by (-)3.4116987 when Foundation becomes CBlock as opposed of BrkTill

# WERONIKA masterplan ==========================================================


# Choice of model: random forest
# training the model using 80% of original data
set.seed(123)
table(ytest)
table(ytrain)
rf_train <- randomForest(xtrain, ytrain, method='class', importance = T)
print(rf_train)

layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rf_train, log="y", main="Random Forest error rate")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf_train$err.rate),col=1:4,cex=0.8,fill=1:4)

# Plotting the most significant variables for the accuracy of the model
varImpPlot(rf_train, main="Feature importance")
# Prediction using 20% of original data
prediction_rf <- predict(rf_train, xtest)


# Evaluating the model using the accuracy metric
confusion_matrix_rf <- confusionMatrix(prediction_rf, ytest)
confusion_matrix_rf

#before part3, we clean up the environment
rm(confusion_matrix_rf,
   index,
   mlr_data,
   mlr_test,
   mlr_train,
   model_mlr_all,
   model_mlr1,
   p,
   rf_train,
   xtest,
   xtrain,
   z,
   mlr_tab_test_2,
   mlr_tab_train_2,
   prediction_rf,
   ytest,
   ytrain)
