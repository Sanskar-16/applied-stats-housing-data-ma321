install.packages("dsEssex")
install.packages("funModeling")


library(MASS)
library(ipred)
library(tree)
library(rpart)
library(randomForest)
library(ada)
library(dsEssex)
library(funModeling)

#read in data
setwd("C:/Users/robbi/OneDrive/Documents/uniWork/MA321")
house.data = read.csv("house-data.csv")

#initial statistics
attributes(house.data)
str(house.data)
head(house.data)
tail(house.data)
summary(house.data)
dim(house.data)

#some plots on all the data
#boxfreq(house.data) #placeholder

#setting a copy of the data for the correlation matrix used later
house.data.corr = house.data

#firstly, we have alot of character columns which we need to convert to factors for analysis
#we also have some categorical variables which we would want to make a table with (to view obs per level)
#and we want to check some of the categorical variables for correct representation

#We should remove the Id column since this won't provide us with any analytical insight outside of the data set 
#(ID is unique per row whilst not being a continuous variable)
house.data$Id = NULL
house.data.corr$Id = NULL

summary(house.data$LotFrontage) #we have NA's which in this context, can be treated as 0
house.data$LotFrontage[is.na(house.data$LotFrontage)] = 0
house.data$LotFrontage = house.data$LotFrontage

house.data$Street = as.factor(house.data$Street)
summary(house.data$Street)
freq(house.data$Street) #Nearly all 'Pave', the variable isn't useful for modelling, should be removed
house.data$Street = NULL
house.data.corr$Street = NULL

house.data$Alley[is.na(house.data$Alley)] = 'NoAlley'
house.data$Alley = as.factor(house.data$Alley) #NA in this column actually means no alley access
summary(house.data$Alley)
freq(house.data$Alley)
house.data.corr$Alley = as.numeric(house.data$Alley)


#more in-depth analysis between variables
#table(house.data$Alley,house.data$BldgType)
#table(house.data$Alley,house.data$HouseStyle)

house.data$Utilities = as.factor(house.data$Utilities)
summary(house.data$Utilities) #only 1 NoSewa observation, not useful for modelling, we remove
freq(house.data$Utilities)
house.data$Utilities = NULL
house.data.corr$Utilities = NULL

house.data$LotConfig = as.factor(house.data$LotConfig)
summary(house.data$LotConfig) #FR3 has low observation, we should cobine with FR2
levels(house.data$LotConfig) = c("Corner","CulDSac","FR2&3","FR2&3","Inside")
summary(house.data$LotConfig)
freq(house.data$LotConfig)
house.data.corr$LotConfig = as.numeric(house.data$LotConfig)

house.data$Neighborhood = as.factor(house.data$Neighborhood)
summary(house.data$Neighborhood) #We should merge Blueste and NPkVill into an 'Other' column
levels(house.data$Neighborhood) = c("Blmngtn","Other","BrDale","BrkSide","ClearCr","CollgCr","Crawfor","Edwards","Gilbert","IDOTRR","MeadowV","Mitchel","NAmes","NoRidge","Other","NridgHt","NWAmes","OldTown","Sawyer","SawyerW","Somerst","StoneBr","SWISU","Timber","Veenker")
summary(house.data$Neighborhood)
freq(house.data$Neighborhood)
house.data.corr$Neighborhood = as.numeric(house.data$Neighborhood)

house.data$Condition1 = as.factor(house.data$Condition1)
summary(house.data$Condition1) #some levels have less than 10 observations, we should merge in the following way
#PosA & PosN; Near of adjacent to positive off-site feature--park; PosC
#RRAe & RRNe; within 200' or adjecent to East-West Rail; RRCe
#RRAn & RRNn; within 200' or adjecent to North-South Rail; RRCn
levels(house.data$Condition1) = c("Artery","Feedr","Norm","PosC","PosC","RRCe","RRCn","RRCe","RRCn")
summary(house.data$Condition1)
freq(house.data$Condition1)
house.data.corr$Condition1 = as.numeric(house.data$Condition1)

house.data$Condition2 = as.factor(house.data$Condition2)
summary(house.data$Condition2) #all levels except Norm have low obs, turn into binary column around 'Norm'
levels(house.data$Condition2) = c("Not-Norm","Not-Norm","Norm","Not-Norm","Not-Norm","Not-Norm","Not-Norm","Not-Norm","Not-Norm")
summary(house.data$Condition2)
freq(house.data$Condition2)
house.data.corr$Condition2 = as.numeric(house.data$Condition2)

house.data$BldgType = as.factor(house.data$BldgType)
summary(house.data$BldgType) #a useful variable! No NA's and good representation per level
freq(house.data$BldgType)
house.data.corr$BldgType = as.numeric(house.data$BldgType)

house.data$HouseStyle = as.factor(house.data$HouseStyle)
summary(house.data$HouseStyle) #we merge '2.5Fin' and '2.5Unf' into one level for '2.5All'
levels(house.data$HouseStyle) = c("1.5Fin","1.5Unf","1Story","2.5All","2.5All","2Story","SFoyer","SLvl")
summary(house.data$HouseStyle)
freq(house.data$HouseStyle)
house.data.corr$HouseStyle = as.numeric(house.data$HouseStyle)

house.data$OverallQual = as.factor(house.data$OverallQual)
summary(house.data$OverallQual) #We should merge 1,2 into 3 and make new level 3 or lower
levels(house.data$OverallQual) = c("3<=","3<=","3<=","4","5","6","7","8","9","10")
summary(house.data$OverallQual)
freq(house.data$OverallQual)
house.data.corr$OverallQual = as.numeric(house.data$OverallQual)

#Here we have OverallCond, we can convert in prep for question 2
house.data$OverallCond = as.factor(ifelse(house.data$OverallCond >= 1 & house.data$OverallCond <= 3, 'Poor',
                        ifelse(house.data$OverallCond >= 4 & house.data$OverallCond <= 6, 'Average', 'Good')))
summary(house.data$OverallCond)
freq(house.data$OverallCond)
house.data.corr$OverallCond = as.numeric(house.data$OverallCond)

house.data$RoofStyle = as.factor(house.data$RoofStyle)
summary(house.data$RoofStyle) #some levels have less than 10 observations, we should make a category of 'other'
levels(house.data$RoofStyle) = c("Flat","Gable","Other","Hip","Other","Other")
summary(house.data$RoofStyle)
freq(house.data$RoofStyle)
house.data.corr$RoofStyle = as.numeric(house.data$RoofStyle)

house.data$RoofMatl = as.factor(house.data$RoofMatl)
summary(house.data$RoofMatl) #need to consider merging, turn into CompShg & not CompShg variable
levels(house.data$RoofMatl) = c("Not-CompShg","CompShg","Not-CompShg","Not-CompShg","Not-CompShg","Not-CompShg","Not-CompShg","Not-CompShg")
summary(house.data$RoofMatl)
freq(house.data$RoofMatl)
house.data.corr$RoofMatl = as.numeric(house.data$RoofMatl)

house.data$Exterior1st = as.factor(house.data$Exterior1st)
summary(house.data$Exterior1st) #some levels have less than 10 observations, need to merge in the following way
#AsbShng & AsphShn; Asbestos or Asphalt Shingles; Asb&AsphShn
#BrkComm, BrkFace, CBlock & Stone; Brick, Cinder or Stone; BrkCinStone
#ImStucc & Stucco; Imitation Stucco and Stucco: AllStucc
levels(house.data$Exterior1st) = c("Asb&AsphShn","Asb&AsphShn","BrkCinStone","BrkCinStone","BrkCinStone","CemntBd","HdBoard","AllStucc","MetalSd","Plywood","BrkCinStone","AllStucc","VinylSd","Wd Sdng","WdShing")
summary(house.data$Exterior1st)
freq(house.data$Exterior1st)
house.data.corr$Exterior1st = as.numeric(house.data$Exterior1st)

house.data$ExterQual = as.factor(house.data$ExterQual)
summary(house.data$ExterQual) #a useful variable! No NA's and decent representation per level
freq(house.data$ExterQual)
house.data.corr$ExterQual = as.numeric(house.data$ExterQual)

house.data$ExterCond = as.factor(house.data$ExterCond)
summary(house.data$ExterCond) #some levels have less than 10 observations, merge in the following way (maybe don't werge?)
#Ex & Gd; Good and Above; Ex&Gd
#Po & Fa; Fair and worse; Fa&Po
levels(house.data$ExterCond) = c("Ex&Gd","Fa&Po","Ex&Gd","Fa&Po","TA")
summary(house.data$ExterCond)
freq(house.data$ExterCond)
house.data.corr$ExterCond = as.numeric(house.data$ExterCond)

house.data$Foundation = as.factor(house.data$Foundation)
summary(house.data$Foundation) #some levels have less than 10 observations, use the following merge
#Slab, Stone & Wood; Slab, Stone Or Wood Foundation; SlaStnWood
levels(house.data$Foundation) = c("BrkTil","CBlock","PConc","SlaStnWood","SlaStnWood","SlaStnWood")
summary(house.data$Foundation)
freq(house.data$Foundation)
house.data.corr$Foundation = as.numeric(house.data$Foundation)

house.data$BsmtQual[is.na(house.data$BsmtQual)] = 'NoBsmt' #NA values actually mean no basement, not missing data
house.data$BsmtQual = as.factor(house.data$BsmtQual)
summary(house.data$BsmtQual) #a useful variable! good representation per level
freq(house.data$BsmtQual)
house.data.corr$BsmtQual = as.numeric(house.data$BsmtQual)

house.data$BsmtCond[is.na(house.data$BsmtCond)] = 'NoBsmt' #NA values actually mean no basement, not missing data
house.data$BsmtCond = as.factor(house.data$BsmtCond)
summary(house.data$BsmtCond) #'Po' has low observation, merge like the following:
#Fa & Po; Fair and Poor BsmtCond; Fa&Po
levels(house.data$BsmtCond) = c("Fa&Po","Gd","NoBsmt","Fa&Po","TA" )
summary(house.data$BsmtCond)
freq(house.data$BsmtCond)
house.data.corr$BsmtCond = as.numeric(house.data$BsmtCond)

house.data$Heating = as.factor(house.data$Heating)
summary(house.data$Heating) #we should put all other levels in 'Other'
levels(house.data$Heating) = c("Other","GasA","GasW","Other","Other","Other")
summary(house.data$Heating)
freq(house.data$Heating)
house.data.corr$Heating = as.numeric(house.data$Heating)

table(house.data$LowQualFinSF)

table(house.data$FullBath) #Since 0 and 1 are very different, we should keep separate

house.data$BedroomAbvGr = as.factor(house.data$BedroomAbvGr)
summary(house.data$BedroomAbvGr) #merge some values together (5,6 7 as 5 or above), 0 is very different to 1, we should keep separate
levels(house.data$BedroomAbvGr) = c("0","1","2","3","4",">=5",">=5",">=5")
summary(house.data$BedroomAbvGr)
freq(house.data$BedroomAbvGr)
house.data.corr$BedroomAbvGr = as.numeric(house.data$BedroomAbvGr)

table(house.data$KitchenAbvGr) #should consider removing 0 and 3 from dataset, too little observation for data

house.data$KitchenQual = as.factor(house.data$KitchenQual)
summary(house.data$KitchenQual) #a useful variable! No NA's and good representation per level
freq(house.data$KitchenQual)
house.data.corr$KitchenQual = as.numeric(house.data$KitchenQual)

house.data$TotRmsAbvGrd = as.factor(house.data$TotRmsAbvGrd)
summary(house.data$TotRmsAbvGrd) #merge the lower and higher tails in
levels(house.data$TotRmsAbvGrd) = c("<=3","<=3","4","5","6","7","8","9","10","11",">=12",">=12")
summary(house.data$TotRmsAbvGrd)
freq(house.data$TotRmsAbvGrd)
house.data.corr$TotRmsAbvGrd = as.numeric(house.data$TotRmsAbvGrd)

house.data$Functional = as.factor(house.data$Functional)
summary(house.data$Functional) #we can merge the levels like the following:
#Maj1, Maj2 & Sev; 1 or more major dedunctions or severley damaged; Maj1+Sev
levels(house.data$Functional) = c("Maj1+Sev","Maj1+Sev","Min1","Min2","Mod","Maj1+Sev","Typ")
summary(house.data$Functional)
freq(house.data$Functional) #consider a 'Typ' not 'Typ' column?
house.data.corr$Functional = as.numeric(house.data$Functional)

house.data$Fireplaces = as.factor(house.data$Fireplaces)
summary(house.data$Fireplaces)
levels(house.data$Fireplaces) = c("0","1",">=2",">=2")
summary(house.data$Fireplaces)
freq(house.data$Fireplaces)
house.data.corr$Fireplaces = as.numeric(house.data$Fireplaces)

house.data$GarageType[is.na(house.data$GarageType)] = 'NoGarage'
house.data$GarageType = as.factor(house.data$GarageType)
summary(house.data$GarageType) #some levels have less than 10 observations, we should make a merge such as
#2Types & CarPort; 2 Types of garage or a cart port; 2TypCarP
levels(house.data$GarageType) = c("2TypCarP","Attchd","Basment","BuiltIn","2TypCarP","Detchd","NoGarage")
summary(house.data$GarageType)
freq(house.data$GarageType)
house.data.corr$GarageType = as.numeric(house.data$GarageType)

house.data$GarageCond[is.na(house.data$GarageCond)] = 'NoGarage'
house.data$GarageCond = as.factor(house.data$GarageCond)
summary(house.data$GarageCond) #consider changing into a 'TA' not 'TA' variable, with a NoGarage Variable
levels(house.data$GarageCond) = c("Not-TA","Not-TA","Not-TA","NoGarage","Not-TA","TA")
summary(house.data$GarageCond)
freq(house.data$GarageCond)
house.data.corr$GarageCond = as.numeric(house.data$GarageCond)

house.data$PavedDrive = as.factor(house.data$PavedDrive)
summary(house.data$PavedDrive) #a clean variable! good representation per level
freq(house.data$PavedDrive)
house.data.corr$PavedDrive = as.numeric(house.data$PavedDrive)

table(house.data$PoolArea) #due to low amount of info, this variable isn't good for modelling
house.data$PoolArea = NULL
house.data.corr$PoolArea = NULL

house.data$PoolQC = as.factor(house.data$PoolQC)
summary(house.data$PoolQC) #poor variable, consider removing for modelling
house.data$PoolQC = NULL
house.data.corr$PoolQC = NULL

house.data$Fence[is.na(house.data$Fence)] = 'NoFence'
house.data$Fence = as.factor(house.data$Fence)
summary(house.data$Fence) #good variable, decent amount of observations per level
freq(house.data$Fence)
house.data.corr$Fence = as.numeric(house.data$Fence)

house.data$MiscFeature[is.na(house.data$MiscFeature)] = 'NoMiscF'
house.data$MiscFeature = as.factor(house.data$MiscFeature)
summary(house.data$MiscFeature) #turn this into a MiscFeature, no-MiscFeature variable
levels(house.data$MiscFeature) = c("MiscF","NoMiscF","MiscF","MiscF","MiscF")
summary(house.data$MiscFeature)
freq(house.data$MiscFeature)
house.data.corr$MiscFeature = as.numeric(house.data$MiscFeature)

table(house.data$MoSold) #decent spread, worth keeping varible!

house.data$SaleType = as.factor(house.data$SaleType)
summary(house.data$SaleType) #Since we have an other category already, we can just merge all other low levels into the other
levels(house.data$SaleType) = c("COD","Oth","Oth","Oth","Oth","Oth","New","Oth","WD")
summary(house.data$SaleType)
freq(house.data$SaleType)
house.data.corr$SaleType = as.numeric(house.data$SaleType)

house.data$SaleCondition = as.factor(house.data$SaleCondition)
summary(house.data$SaleCondition) #AdjLand low obs, we can merge:
#AdjLand & Alloca; Adjoing Land and Two linked properties; AdjL&Alloca
levels(house.data$SaleCondition) = c("Abnorml","AdjL&Alloca","AdjL&Alloca","Family","Normal","Partial")
summary(house.data$SaleCondition)
freq(house.data$SaleCondition)
house.data.corr$SaleCondition = as.numeric(as.factor(house.data$SaleCondition))

hist(SalePrice)

#Now lets do some correlation checks on the data
library(ggcorrplot)

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

#it's a bit hard to read the full matrix, so lets start by removing some variables we which cleary show no signs of correlation
house.data.corr.view = house.data.corr
house.data.corr.view$LotFrontage = NULL
house.data.corr.view$LotArea = NULL
house.data.corr.view$Alley = NULL
house.data.corr.view$LotConfig = NULL
house.data.corr.view$Neighborhood = NULL
house.data.corr.view$Condition1 = NULL
house.data.corr.view$Condition2 = NULL
house.data.corr.view$BldgType = NULL
house.data.corr.view$HouseStyle = NULL
house.data.corr.view$RoofStyle = NULL
house.data.corr.view$RoofMatl = NULL
house.data.corr.view$Exterior1st = NULL
house.data.corr.view$BsmtCond = NULL
house.data.corr.view$Heating = NULL
house.data.corr.view$LowQualFinSF = NULL
house.data.corr.view$Fence = NULL
house.data.corr.view$MoSold = NULL
house.data.corr.view$YrSold = NULL
house.data.corr.view$SaleType = NULL
house.data.corr.view$SaleCondition = NULL
house.data.corr.view$GarageType = NULL
house.data.corr.view$MiscVal = NULL
house.data.corr.view$KitchenAbvGr = NULL
house.data.corr.view$ExterCond = NULL
house.data.corr.view$PavedDrive = NULL
house.data.corr.view$Functional = NULL
house.data.corr.view$GarageCond = NULL

corr = cor(house.data.corr.view, use = "complete.obs")
corr[upper.tri(corr)] = 0
diag(corr) = 0

#a plot of the selected matrix
ggcorrplot(corr,
           type = "lower",
           lab = T,
           hc.order = T,
           title = "Data Correlation Matrix")



#From the above graph, we can now start to remove highly correlated vars from the data-set, lets highly those above 0.8;
#GrLivArea & TotRmsAbvGrd; 0.83
#TotalBsmtSF & X1stFlrSF; 0.82

#lets also check the VIF
vif(house.data.corr)

#lets begin our removal process:
#first we will remove GrLivArea since it has the highest VIF score and highest correlation coef (with TotRmsA)
house.data.corr.view$GrLivArea = NULL
house.data.corr$GrLivArea = NULL
house.data$GrLivArea = NULL
vif(house.data.corr)

#all of our VIF scores are below 10 so we are ok to proceed with the current selection (a correlation coef of 0.82 is ok to work with)

#there is no missing data in the dataset since a reason has been given fo when a value is NA in a column
#therefore our final dataset is:
summary(house.data)
dim(house.data)

#house.data.temp = as.data.frame(sapply(house.data,function(x) scale(x)))

#lastly, before modelling , we need to scale the continuous variables, can achieve this using a for loop
for (i in names(house.data)) {
  if (is.numeric(house.data[[i]])) {
    house.data[[i]] = scale(house.data[[i]])
  }
}

write.csv(house.data,"house_data_cleaned.csv")

