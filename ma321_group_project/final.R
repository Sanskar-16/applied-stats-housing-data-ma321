library(funModeling) #used to plot categorical variables an their frequencies
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(broom)
library(ggcorrplot)
library(nnet) # multinational logistic regression
library(yardstick)
library(MASS)
library(HSAUR2)
library(ISLR)
library(tictoc)

#the libraries used

#read in data
house.data = read.csv("../data/house-data.csv")

#initial statistics on dataset
attributes(house.data)
dim(house.data)

#having a look at the variables
str(house.data)
summary(house.data)

#having a breif look at the number of levels per variable
sapply(house.data,function(x) length(unique(x)))

#Going though the variables for cleaning----

#firstly, we have alot of character columns which we need to convert to factors for analysis
#we also have some categorical variables which we would want to make a table with (to view obs per level)
#and we want to check some of the categorical variables for correct representation

#We should remove the Id column since this won't provide us with any analytical insight outside of the data set 
#(ID is unique per row whilst not being a continuous variable)
house.data$Id = NULL

#this is a continuous variable which we need to check since it has missing values
summary(house.data$LotFrontage) #we have NA's which in this context, can be treated as 0
house.data$LotFrontage[is.na(house.data$LotFrontage)] = 0

house.data$Street = as.factor(house.data$Street)
summary(house.data$Street)
freq(house.data$Street) #Nearly all 'Pave', the variable isn't useful for modelling, should be removed
house.data$Street = NULL

house.data$Alley[is.na(house.data$Alley)] = 'NoAlley'
house.data$Alley = as.factor(house.data$Alley) #NA in this column actually means no alley access
summary(house.data$Alley)
freq(house.data$Alley)

house.data$Utilities = as.factor(house.data$Utilities)
summary(house.data$Utilities) #only 1 NoSewa observation, not useful for modelling, we remove
freq(house.data$Utilities)
house.data$Utilities = NULL

house.data$LotConfig = as.factor(house.data$LotConfig)
summary(house.data$LotConfig) #FR3 has low observation, we should cobine with FR2
levels(house.data$LotConfig) = c("Corner","CulDSac","FR2&3","FR2&3","Inside")
summary(house.data$LotConfig)
freq(house.data$LotConfig)

house.data$Neighborhood = as.factor(house.data$Neighborhood)
summary(house.data$Neighborhood) #We should merge Blueste and NPkVill into an 'Other' column
levels(house.data$Neighborhood) = c("Blmngtn","Other","BrDale","BrkSide","ClearCr","CollgCr","Crawfor","Edwards","Gilbert","IDOTRR","MeadowV","Mitchel","NAmes","NoRidge","Other","NridgHt","NWAmes","OldTown","Sawyer","SawyerW","Somerst","StoneBr","SWISU","Timber","Veenker")
summary(house.data$Neighborhood)
freq(house.data$Neighborhood)

house.data$Condition1 = as.factor(house.data$Condition1)
summary(house.data$Condition1) #some levels have less than 10 observations, we should merge in the following way
#PosA & PosN; Near of adjacent to positive off-site feature--park; PosC
#RRAe & RRNe; within 200' or adjecent to East-West Rail; RRCe
#RRAn & RRNn; within 200' or adjecent to North-South Rail; RRCn
levels(house.data$Condition1) = c("Artery","Feedr","Norm","PosC","PosC","RRCe","RRCn","RRCe","RRCn")
summary(house.data$Condition1)
freq(house.data$Condition1)

house.data$Condition2 = as.factor(house.data$Condition2)
summary(house.data$Condition2) #all levels except Norm have low obs, turn into binary column around 'Norm'
levels(house.data$Condition2) = c("Not-Norm","Not-Norm","Norm","Not-Norm","Not-Norm","Not-Norm","Not-Norm","Not-Norm","Not-Norm")
summary(house.data$Condition2)
freq(house.data$Condition2)

house.data$BldgType = as.factor(house.data$BldgType)
summary(house.data$BldgType) #a useful variable! No NA's and good representation per level
freq(house.data$BldgType)

house.data$HouseStyle = as.factor(house.data$HouseStyle)
summary(house.data$HouseStyle) #we merge '2.5Fin' and '2.5Unf' into one level for '2.5All'
levels(house.data$HouseStyle) = c("1.5Fin","1.5Unf","1Story","2.5All","2.5All","2Story","SFoyer","SLvl")
summary(house.data$HouseStyle)
freq(house.data$HouseStyle)

###Make this continous###
house.data$OverallQual = as.factor(house.data$OverallQual)
summary(house.data$OverallQual) #We should merge 1,2 into 3 and make new level 3 or lower
levels(house.data$OverallQual) = c("3<=","3<=","3<=","4","5","6","7","8","9","10")
summary(house.data$OverallQual)
freq(house.data$OverallQual)

#Here we have OverallCond, we can convert in prep for question 2
house.data$OverallCond = as.factor(ifelse(house.data$OverallCond >= 1 & house.data$OverallCond <= 3, 'Poor',
                                          ifelse(house.data$OverallCond >= 4 & house.data$OverallCond <= 6, 'Average', 'Good')))
summary(house.data$OverallCond)
freq(house.data$OverallCond)

house.data$RoofStyle = as.factor(house.data$RoofStyle)
summary(house.data$RoofStyle) #some levels have less than 10 observations, we should make a category of 'other'
levels(house.data$RoofStyle) = c("Flat","Gable","Other","Hip","Other","Other")
summary(house.data$RoofStyle)
freq(house.data$RoofStyle)

house.data$RoofMatl = as.factor(house.data$RoofMatl)
summary(house.data$RoofMatl) #need to consider merging, turn into CompShg & not CompShg variable
levels(house.data$RoofMatl) = c("Not-CompShg","CompShg","Not-CompShg","Not-CompShg","Not-CompShg","Not-CompShg","Not-CompShg","Not-CompShg")
summary(house.data$RoofMatl)
freq(house.data$RoofMatl)

house.data$Exterior1st = as.factor(house.data$Exterior1st)
summary(house.data$Exterior1st) #some levels have less than 10 observations, need to merge in the following way
#AsbShng & AsphShn; Asbestos or Asphalt Shingles; Asb&AsphShn
#BrkComm, BrkFace, CBlock & Stone; Brick, Cinder or Stone; BrkCinStone
#ImStucc & Stucco; Imitation Stucco and Stucco: AllStucc
levels(house.data$Exterior1st) = c("Asb&AsphShn","Asb&AsphShn","BrkCinStone","BrkCinStone","BrkCinStone","CemntBd","HdBoard","AllStucc","MetalSd","Plywood","BrkCinStone","AllStucc","VinylSd","Wd Sdng","WdShing")
summary(house.data$Exterior1st)
freq(house.data$Exterior1st)

house.data$ExterQual = as.factor(house.data$ExterQual)
summary(house.data$ExterQual) #a useful variable! No NA's and decent representation per level
freq(house.data$ExterQual)

house.data$ExterCond = as.factor(house.data$ExterCond)
summary(house.data$ExterCond) #some levels have less than 10 observations, merge in the following way (maybe don't werge?)
#Ex & Gd; Good and Above; Ex&Gd
#Po & Fa; Fair and worse; Fa&Po
levels(house.data$ExterCond) = c("Ex&Gd","Fa&Po","Ex&Gd","Fa&Po","TA")
summary(house.data$ExterCond)
freq(house.data$ExterCond)

house.data$Foundation = as.factor(house.data$Foundation)
summary(house.data$Foundation) #some levels have less than 10 observations, use the following merge
#Slab, Stone & Wood; Slab, Stone Or Wood Foundation; SlaStnWood
levels(house.data$Foundation) = c("BrkTil","CBlock","PConc","SlaStnWood","SlaStnWood","SlaStnWood")
summary(house.data$Foundation)
freq(house.data$Foundation)

house.data$BsmtQual[is.na(house.data$BsmtQual)] = 'NoBsmt' #NA values actually mean no basement, not missing data
house.data$BsmtQual = as.factor(house.data$BsmtQual)
summary(house.data$BsmtQual) #a useful variable! good representation per level
freq(house.data$BsmtQual)

house.data$BsmtCond[is.na(house.data$BsmtCond)] = 'NoBsmt' #NA values actually mean no basement, not missing data
house.data$BsmtCond = as.factor(house.data$BsmtCond)
summary(house.data$BsmtCond) #'Po' has low observation, merge like the following:
#Fa & Po; Fair and Poor BsmtCond; Fa&Po
levels(house.data$BsmtCond) = c("Fa&Po","Gd","NoBsmt","Fa&Po","TA" )
summary(house.data$BsmtCond)
freq(house.data$BsmtCond)

house.data$Heating = as.factor(house.data$Heating)
summary(house.data$Heating) #we should put all other levels in 'Other'
levels(house.data$Heating) = c("Other","GasA","GasW","Other","Other","Other")
summary(house.data$Heating)
freq(house.data$Heating)

table(house.data$LowQualFinSF)

table(house.data$FullBath) #Since 0 and 1 are very different, we should keep separate

###Make this continous###
house.data$BedroomAbvGr = as.factor(house.data$BedroomAbvGr)
summary(house.data$BedroomAbvGr) #merge some values together (5,6 7 as 5 or above), 0 is very different to 1, we should keep separate
levels(house.data$BedroomAbvGr) = c("0","1","2","3","4",">=5",">=5",">=5")
summary(house.data$BedroomAbvGr)
freq(house.data$BedroomAbvGr)

table(house.data$KitchenAbvGr) #should consider removing 0 and 3 from dataset, too little observation for data

house.data$KitchenQual = as.factor(house.data$KitchenQual)
summary(house.data$KitchenQual) #a useful variable! No NA's and good representation per level
freq(house.data$KitchenQual)

#We decided to keep TotRmsAbvGrd as continuous since we are interested in keeping as much observations as possible
summary(house.data$TotRmsAbvGrd)
freq(house.data$TotRmsAbvGrd)

house.data$Functional = as.factor(house.data$Functional)
summary(house.data$Functional) #we can merge the levels like the following:
#Maj1, Maj2 & Sev; 1 or more major dedunctions or severley damaged; Maj1-2+Sev
levels(house.data$Functional) = c("Maj1-2+Sev","Maj1-2+Sev","Min1","Min2","Mod","Maj1-2+Sev","Typ")
summary(house.data$Functional)
freq(house.data$Functional) #consider a 'Typ' not 'Typ' column?

###Make this continous###
house.data$Fireplaces = as.factor(house.data$Fireplaces)
summary(house.data$Fireplaces)
levels(house.data$Fireplaces) = c("0","1",">=2",">=2")
summary(house.data$Fireplaces)
freq(house.data$Fireplaces)

house.data$GarageType[is.na(house.data$GarageType)] = 'NoGarage'
house.data$GarageType = as.factor(house.data$GarageType)
summary(house.data$GarageType) #some levels have less than 10 observations, we should make a merge such as
#2Types & CarPort; 2 Types of garage or a cart port; 2TypCarP
levels(house.data$GarageType) = c("2TypCarP","Attchd","Basment","BuiltIn","2TypCarP","Detchd","NoGarage")
summary(house.data$GarageType)
freq(house.data$GarageType)

house.data$GarageCond[is.na(house.data$GarageCond)] = 'NoGarage'
house.data$GarageCond = as.factor(house.data$GarageCond)
summary(house.data$GarageCond) #We merge into Ex&Gd and Fa&Po
levels(house.data$GarageCond) = c("Ex&Gd","Fa&Po","Ex&Gd","NoGarage","Fa&Po","TA")
summary(house.data$GarageCond)
freq(house.data$GarageCond)

house.data$PavedDrive = as.factor(house.data$PavedDrive)
summary(house.data$PavedDrive) #a clean variable! good representation per level
freq(house.data$PavedDrive)

table(house.data$PoolArea) #due to low amount of info, this variable isn't good for modelling
house.data$PoolArea = NULL

house.data$PoolQC = as.factor(house.data$PoolQC)
summary(house.data$PoolQC) #poor variable, consider removing for modelling
house.data$PoolQC = NULL

summary(house.data$MasVnrArea) #this variable has some NA, it's safe to assume we can recode as 0 since NA in this context means no MasVnrArea
house.data$MasVnrArea[is.na(house.data$MasVnrArea)] = 0

house.data$Fence[is.na(house.data$Fence)] = 'NoFence'
house.data$Fence = as.factor(house.data$Fence)
summary(house.data$Fence) #good variable, decent amount of observations per level
freq(house.data$Fence)

house.data$MiscFeature[is.na(house.data$MiscFeature)] = 'NoMiscF'
house.data$MiscFeature = as.factor(house.data$MiscFeature)
summary(house.data$MiscFeature) #turn this into a MiscFeature, no-MiscFeature variable
levels(house.data$MiscFeature) = c("MiscF","NoMiscF","MiscF","MiscF","MiscF")
summary(house.data$MiscFeature)
freq(house.data$MiscFeature)

table(house.data$MoSold) #decent spread, worth keeping varible!

house.data$SaleType = as.factor(house.data$SaleType)
summary(house.data$SaleType) #Since we have an other category already, we can just merge all other low levels into the other
levels(house.data$SaleType) = c("COD","Oth","Oth","Oth","Oth","Oth","New","Oth","WD")
summary(house.data$SaleType)
freq(house.data$SaleType)

house.data$SaleCondition = as.factor(house.data$SaleCondition)
summary(house.data$SaleCondition) #AdjLand low obs, we can merge:
#AdjLand & Alloca; Adjoing Land and Two linked properties; AdjL&Alloca
levels(house.data$SaleCondition) = c("Abnorml","AdjL&Alloca","AdjL&Alloca","Family","Normal","Partial")
summary(house.data$SaleCondition)
freq(house.data$SaleCondition)
#----

#creating a histogram of SalePrice ----

#removal of scientific notation
options(scipen=999)

#making histogram
hist(house.data$SalePrice,
     main = "Histogram of SalePrice",
     xlab = "SalePrice",
     labels = T
)

#a tabled version of the histogram with 10 bins instead of 16 in the histogram
table(cut(house.data$SalePrice,breaks=10))

#----

#now data is cleaned, we need to make a dataset which can be used for correlation checks
#this means tuning all our factors into purely numeric columns
house.data.corr = house.data

#this loops turns all numeric columns into factors
for (i in names(house.data.corr)) {
  if (is.factor(house.data.corr[[i]])) {
    house.data.corr[[i]] = as.numeric(house.data.corr[[i]])
  }
}

#Now lets do some correlation checks on the data

#creating the correlation matrix
summary(house.data.corr)
corr = cor(house.data.corr, use = "complete.obs")
corr[upper.tri(corr)] = 0
diag(corr) = 0

#a plot of the full matrix
ggcorrplot(corr,
           type = "lower",
           lab = F,
           hc.order = T,
           title = "Data Correlation Matrix")

#it's a bit hard to read the full matrix, so lets start by removing some variables
#which clearly show no signs of correlation

#sub-setting the data to remove those vars which visibly no correlation
house.data.corr.view  = subset(house.data.corr, select = -c(LotFrontage, 
                                                            LotArea, 
                                                            Alley,
                                                            LotConfig,
                                                            Neighborhood,
                                                            Condition1,
                                                            Condition2,
                                                            BldgType,
                                                            HouseStyle,
                                                            RoofStyle,
                                                            RoofMatl,
                                                            Exterior1st,
                                                            BsmtCond,
                                                            Heating,
                                                            LowQualFinSF,
                                                            Fence,
                                                            MoSold,
                                                            YrSold,
                                                            SaleType,
                                                            SaleCondition,
                                                            GarageType,
                                                            MiscVal,
                                                            KitchenAbvGr,
                                                            ExterCond,
                                                            PavedDrive,
                                                            Functional,
                                                            GarageCond))

#move the dependent to the end
#create the new selected correlation matrix
corr = cor(house.data.corr.view, use = "complete.obs")
corr[upper.tri(corr)] = 0
diag(corr) = 0

#a plot of the selected matrix
ggcorrplot(corr,
           type = "lower",
           lab = T,
           hc.order = T,
           title = "Data Correlation Matrix")

#From the above graph, we can now start to remove highly correlated vars from the data-set, 
#lets mainly those above 0.8;
#GrLivArea & TotRmsAbvGrd; 0.83
#TotalBsmtSF & X1stFlrSF; 0.82

#lets begin our removal process:
#first we will remove GrLivArea since it has the highest correlation coef (with TotRmsA)
house.data$GrLivArea = NULL

#next we will remove TotalBsmtSF since it highly correlates with X1stFlrSF and some houses don't have basements
#when they do have 1st floors
house.data$TotalBsmtSF = NULL

#there is no missing data in the dataset since a reason has been given fo when a value is NA in a column
#therefore our final dataset is:
summary(house.data)
dim(house.data)

#lastly, before modelling , we need to scale the continuous variables, can achieve this using a for loop
for (i in names(house.data)) {
  if (is.numeric(house.data[[i]])) {
    house.data[[i]] = as.vector(scale(house.data[[i]]))
  }
}

#cleaning up the environment for team members going forward
rm(house.data.corr)
rm(house.data.corr.view)
rm(i)
rm(corr)


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
tic("fitting mlr model")
model_mlr_aic_b <- stepAIC(model_mlr_f, direction = "backward", trace=FALSE)
model_mlr_aic_f <- stepAIC(model_mlr_n,direction="forward", scope=list(upper=model_mlr_f,lower=model_mlr_n), trace=FALSE)
toc()
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
   rf_train,
   xtest,
   xtrain,
   prediction_rf,
   ytest,
   ytrain)



summary(house.data)

# partitioning the model for training and testing set (80/20 split)
set.seed(16)
samp <- createDataPartition(house.data$SalePrice, p = 0.8, list = FALSE)
training <- house.data[samp,]
testing <- house.data[-samp,]
x_test <- testing[,1:43]
y_test <- testing[,44]

set.seed(16)

# linear regression model using all the variables (regression based)
lm_full_model <- train(SalePrice~., data = training, method = "lm")

# random forest model using all the variables (tree based)
tic("randomForest full model")
rf_full_model <- train(SalePrice~., data = training, method = "rf")
toc()

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
tic("randomForest reduced model")
rf_selected_model <- train(SalePrice~OverallQual+X2ndFlrSF+OverallCond+X1stFlrSF+Fireplaces+BsmtQual+KitchenQual+Neighborhood,
                           data = training, method = "rf")
toc()
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
print(mean(pred_lm$error^2))
# [1] 0.1344096

pred_rf <- as.data.frame(predict(rf_selected_model, x_test))
names(pred_rf)[1] = "fit"
pred_rf$real = y_test
pred_rf$error = pred_rf$fit - pred_rf$real
print(mean(pred_rf$error^2))
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



#Our question is on if the data naturally falls into clusters based on neighborhoods
#We will be comparing the cluster results to the clustered form by OverallCond

#first, a numeric version of the data
house.data.num = house.data

#this loops turns all numeric columns into factors
for (i in names(house.data.num)) {
  if (is.factor(house.data.num[[i]])) {
    house.data.num[[i]] = as.numeric(house.data.num[[i]])
  }
}

#We can employ K-Means to see if there are any inherit groups within the data

#lets start by doing a PCA transformation using the correlation matrix over the covariance
x.pca = princomp(house.data.num, cor = TRUE)
x.pca.test = prcomp(house.data.num)
#summaries of the transformation
s = summary(x.pca)
s
s$sdev

# plot the Cumulative  contribution of components 
plot(cumsum(s$sdev^2 / sum(s$sdev^2)), type="l", xlab="number of components", ylab="cumulative varince")

#biplot of the first two components 
biplot(x.pca, scale = 0) # no scaling in the plot 

#looking at the rotations/loadings for the 1st and 2nd PCA's
x.pca$loadings[,1]
x.pca$loadings[,2]

#now we can plot the data across the 1st 2 PC's and colour by OverCond and then neighbourhood
proj = as.data.frame(x.pca$scores)
proj$OverallCond = house.data$OverallCond
proj$Neighborhood = house.data$Neighborhood

#the OverallCond Plot
dev.off()
ggplot(proj) + 
  geom_point(aes(x=proj[,1], y=proj[,2], color=OverallCond)) +
  labs(color = "Overall Condition", x = "PC1", y = "PC2", title = "PCA Plot: By Overall Condition")

#The Neighbourhood Plot
ggplot(proj) + 
  geom_point(aes(x=proj[,1], y=proj[,2], color=Neighborhood)) +
  labs(color = "Neighborhood", x = "PC1", y = "PC2", title = "PCA Plot: By Neighborhood")

#We will use the PC's since it explains our data in better dimessions
#now we can have a look at these groups though a k-means clustering algorithm, k = the number of levels in OverallCond
#to make results reproducible, we set the random seed so the center points picked are always the same
set.seed(1)
km.OverallCond = kmeans(x.pca$scores, centers= length(unique(house.data$OverallCond)) ,1000)
km.OverallCond$cluster

#we can table the results with the OverallCond variable
table(km.OverallCond$cluster,as.matrix(house.data$OverallCond))

#observations per cluster
margin.table(table(km.OverallCond$cluster,as.matrix(house.data$OverallCond)), margin=1)

#we can plot the boxplots of the first 3 clusters across the SalePrice and OverallCond (SalePrice is scaled)
par(mfrow = c(3,1))
plot(house.data[km.OverallCond$cluster==1,c(11,44)],pch="x", ylim=c(-3,3))
plot(house.data[km.OverallCond$cluster==2,c(11,44)],col="firebrick", ylim=c(-3,3))
plot(house.data[km.OverallCond$cluster==3,c(11,44)],col="skyblue", ylim=c(-3,3))
par(mfrow = c(1,1))

#now we can have a look at these groups though a k-means clustering algorithm, k = the number of levels in Neighbourhood
set.seed(1)
km.Neighborhood = kmeans(x.pca$scores, centers=length(unique(house.data$Neighborhood)) ,1000)
km.Neighborhood$cluster

#we can table the results with the OverallCond variable
table(km.Neighborhood$cluster,as.matrix(house.data$Neighborhood))

#observations per cluster
margin.table(table(km.Neighborhood$cluster,as.matrix(house.data$Neighborhood)), margin=1)

#we can plot the boxplots of the first 3 clusters across the SalePrice and Neighborhood (SalePrice is scaled)
plot(house.data[km.Neighborhood$cluster==1,c(5,44)], ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==2,c(5,44)],col="firebrick", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==3,c(5,44)],col="skyblue", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==4,c(5,44)],col="khaki", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==5,c(5,44)],col="green", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==6,c(5,44)],col="magenta", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==7,c(5,44)],col="coral4", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==8,c(5,44)],col="purple", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==9,c(5,44)],col="salmon", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==10,c(5,44)],col="pink", ylim=c(-3,3), xaxt = "n", xlab = "")

#adding in better x-axis
text(x = 1:length(unique(house.data$Neighborhood)),
     y = par("usr")[3] - 0.1,
     labels = unique(house.data$Neighborhood),
     xpd = NA,
     adj = 1,
     srt = 55
)

#adding in rect to highlight a how the SalePrice varies per a neighbourhood per cluster
rect(xleft = 7.5, xright = 8.5, ybottom = -2, ytop = 2, border = "red", lwd = 3)

#as a final experiment, lets look at how the OverallCond K-Means cluster model looks over neighborhood
#(basically K-Means with 3 centers)
plot(house.data[km.OverallCond$cluster==1,c(5,44)], ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.OverallCond$cluster==2,c(5,44)],col="firebrick", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.OverallCond$cluster==3,c(5,44)],col="skyblue", ylim=c(-3,3), xaxt = "n", xlab = "")

#adding in better x-axis
text(x = 1:length(unique(house.data$Neighborhood)),
     y = par("usr")[3] - 0.1,
     labels = unique(house.data$Neighborhood),
     xpd = NA,
     adj = 1,
     srt = 55
)

#adding in rect to highlight clusters
rect(xleft = 0, xright = 25, ybottom = -0.5, ytop = 1.2, border = "black")
rect(xleft = 4, xright = 24, ybottom = 0, ytop = 3, border = "red")
rect(xleft = 2, xright = 24, ybottom = -1.5, ytop = 0.2, border = "blue")

#the above shows us that we seem to have 3 recognizable clusters for Neighborhoods, nice!

#now we can have a look at hierarchical clustering, first the libraries


#fitting the hierarchical cluster model
set.seed(1)
hclust.house = hclust(dist(x.pca$scores), method="complete") # aggomorative hierarchical clustering based on complete linkage 

#ploting with the OverallCond labels as the labels
plot(hclust.house,labels=(as.character(house.data$OverallCond)), main="",xlab="complete-linkage",ylab="level")

#ploting squares around the desired clusters (we want the amount of Conditions as the amount of clusters)
rect.hclust(hclust.house,k=length(unique(house.data$OverallCond)), border = "red")

#we can get a table of the results as well
qualClus = cutree(hclust.house, length(unique(house.data$OverallCond)))
table(qualClus, house.data$OverallCond)

#observations per cluster
margin.table(table(qualClus, house.data$OverallCond), margin=1)

#ploting with the neighborhood as the labels
plot(hclust.house,labels=(as.character(house.data$Neighborhood)), main="",xlab="complete-linkage",ylab="level")

#ploting squares around the desired clusters (we want the amount of neighbors as the amount of clusters)
rect.hclust(hclust.house,k=length(unique(house.data$Neighborhood)), border = "red")

#we can get a table of the results aswell
neighClus = cutree(hclust.house, length(unique(house.data$Neighborhood)))
table(neighClus, house.data$Neighborhood)

#observations per cluster
margin.table(table(neighClus, house.data$Neighborhood), margin=1)

#as a final experiment, let's loop though 3 - 24 to see what best clusters we get for neighborhood
####UNCOMMENT THIS SETTING TO SEE ALL THE GRAPHS####
#par(ask = T)
for(i in 3:24){
  
  #plot graph
  plot(hclust.house,labels=(as.character(house.data$Neighborhood)), main="",xlab="complete-linkage",ylab="level")
  
  #plot the labelled clusters
  rect.hclust(hclust.house,k=i, border = "red")
  
  #cut the tree at this cluster amount and table the results
  neighClus = cutree(hclust.house, i)
  table(neighClus, house.data$OverallCond)
  
  #observations per cluster
  margin.table(table(neighClus, house.data$Neighborhood), margin=1)
  
}

