
install.packages("tidyverse")
library(tidyr)
library(caret)
library("mlogit")
library(dplyr)
library(readr)

house_data <- read_csv("../data/house-data.csv")
house_data <- read_csv("../data/house_data_v1_cleaned.csv")

house_data <- house_data %>%
  mutate(OverallCond=case_when(
    OverallCond %in% 1:3 ~ "Poor",
    OverallCond %in% 4:6 ~ "Average",
    OverallCond %in% 7:10 ~ "Good"
  ))


summary(house_data$OverallCond)
str(house_data$OverallCond)

house_data$OverallCond <- factor(house_data$OverallCond)

# data partition 80/20 split with respect to the response variable
trainIndex <- createDataPartition(house_data$OverallCond, p=0.8, list = F, times =1)
Xtrain <- house_data[trainIndex, -14]
ytrain <- house_data[trainIndex,14]
Xtest <- house_data[-trainIndex, -14]
ytest <- house_data[-trainIndex,14]
