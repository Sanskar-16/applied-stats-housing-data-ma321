
install.packages("tidyverse")
library(tidyr)
library(caret)
library("mlogit")
# library(dplyr)
library(readr)

house_data <- read_csv("../data/house_data_v1_cleaned.csv")

house_data <- house_data %>%
  mutate(OverallQual = gsub('(\\d')) %>%
  mutate(OverallCond=case_when(
    OverallQual %in% 1:5 ~ "PoorEnough",
    OverallQual %in% 6:10 ~ "GoodEnough"
  ))


summary(house_data$OverallCond)
str(house_data$OverallCond)

house_data$OverallCond <- factor(house_data$OverallCond)

# data partition 80/20 split with respect to the response variable
trainIndex <- createDataPartition(house_data$OverallCond, p=0.8, list = F, times =1)

predictor_index <- 12
Xtrain <- house_data[trainIndex, -predictor_index]
ytrain <- house_data[trainIndex,predictor_index]
Xtest <- house_data[-trainIndex, -predictor_index]
ytest <- house_data[-trainIndex,predictor_index]

# mlogit_data <- mlogit.data(house_data,)
# m1 <- mlogit(house_data$OverallCond~house.data$SalePrice, data = house.data)

fitted_glm  <- glm(predictor_index ~ . -predictor_index , data = Xtrain, family = binomial)

house_da
