# PROBLEM PAGE: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data
# OBJECTIVE: predict the price of house based in the attributes of the data

# DEFINING THE WORKING DIRECTORY
setwd('./')

# LOADING FUNCTIONS
source("functions.R")

# LOADING LIBRARIES
library(corrplot)
library(dplyr)
library(ggplot2)
library(randomForest)
library(outliers)
library(ModelMetrics)

df_house_train <- read.csv("../Dados/train.csv")

#================== EXPLORATORY ANALYSIS ==============================

names(df_house_train)

summary(df_house_train)

str(df_house_train)

columnsWithManyNA <- c("MiscFeature", "Fence", "FireplaceQu", "Alley", "PoolQC", "LotFrontage")


cors <- cor(na.omit(df_house_train[, unlist(lapply(df_house_train, is.numeric))]))
corrplot(cors, type="upper", tl.cex = 0.5, diag = F, order = "FPC")


categoricColumns <- names(df_house_train[, unlist(lapply(df_house_train, is.numeric)) == F])
numericColumns <- names(df_house_train[, unlist(lapply(df_house_train, is.numeric)) == T])

for(col in categoricColumns){
  g <- ggplot(df_house_train, aes(x = df_house_train[, col] , y = SalePrice)) +
        geom_boxplot() +
          labs(title=paste("SalePrice per", col), x=col, y = "SalePrice")
  print(g)
}

for(col in numericColumns){
  g <- ggplot(df_house_train, aes(x = df_house_train[, col] , y = SalePrice)) +
    geom_point() +
    labs(title=paste("SalePrice per", col), x=col, y = "SalePrice")
  print(g)
}

# variables that can be collinear
collinearColumns <- c("GarageArea", "LandContour", "X1stFlrSF", "X2ndFlrSF")

#================ CLEANING AND TRANSFORMING THE DATAS ==================

df_house_train <- deleteColumns(df_house_train, columnsWithManyNA)
df_house_train <- deleteColumns(df_house_train, collinearColumns)

df_house_train$LotShape_F <- sapply(df_house_train$LotShape, function(x){ifelse(x == "Reg", 1, 0)})  
df_house_train$Functional_F <- sapply(df_house_train$Functional, function(x){ifelse(x == "Typ", 1, 0)})  

df_house_train$Utilities_F <- sapply(df_house_train$Utilities, function(x){ifelse(x == "AllPub", 1, 0)})  
df_house_train$Street_F <- sapply(df_house_train$Street, function(x){ifelse(x == "Pave", 1, 0)})  
df_house_train$Heating_F <- sapply(df_house_train$Heating, function(x){ifelse(x == "GasA", 1, 0)})  
df_house_train$CentralAir_F <- sapply(df_house_train$CentralAir, function(x){ifelse(x == "Y", 1, 0)})  
df_house_train$TotPorchArea_F <- df_house_train$EnclosedPorch +
                                 df_house_train$WoodDeckSF +
                                 df_house_train$OpenPorchSF +
                                 df_house_train$ScreenPorch 

df_house_train$BsmtFinType1 <- sapply(df_house_train$BsmtFinType1, 
                                      function(x){
                                        if(x == "GLQ")
                                          return(1)
                                        else if( x == "ALQ")
                                          return(0.85)
                                        else if(x == "BLQ")
                                          return(0.5)
                                        else if(x == "Rec")
                                          return(0.85)
                                        else if(x == "LwQ")
                                          return(0.3)
                                        else if(x == "Unf")
                                          return(0.05)
                                        })

for(indice in 1:(dim(df_house_train)[1])){
  df_house_train[indice, "BsmtF_F"] <- df_house_train[indice, "BsmtFinSF1"] * df_house_train[indice, "BsmtFinType1"]
}

df_house_train <- deleteColumns(df_house_train, c("BsmtFinSF1", "BsmtFinType1"))

columnsPorch <- c("EnclosedPorch", "WoodDeckSF", "OpenPorchSF", "ScreenPorch")
oldColumns <- c("LotShape", "Functional", "Street", "Utilities", "Heating", "CentralAir")
columnsBath <- c("FullBath", "HalfBath", "BsmtHalfBath", "BsmtFullBath")
columnsWithCorr0 <- c("LowQualFinSF", "MiscVal", "X3SsnPorch", "MSSubClass", "YrSold")

df_house_train$TotBathInHouse_F <- df_house_train$FullBath + df_house_train$HalfBath + df_house_train$BsmtHalfBath + df_house_train$BsmtFullBath

df_house_train <- deleteColumns(df_house_train, columnsBath)
df_house_train <- deleteColumns(df_house_train, oldColumns)
df_house_train <- deleteColumns(df_house_train, columnsWithCorr0)
df_house_train <- deleteColumns(df_house_train, columnsPorch)

# treating missing data
df_house_train <- subset(df_house_train, is.na(df_house_train$Electrical) == F)
df_house_train <- subset(df_house_train, is.na(df_house_train$MasVnrArea) == F)
df_house_train <- subset(df_house_train, is.na(df_house_train$GarageQual) == F)
df_house_train <- subset(df_house_train, is.na(df_house_train$BsmtFinType2) == F)
df_house_train <- subset(df_house_train, is.na(df_house_train$BsmtExposure) == F)

sum(is.na(df_house_train))
dim(df_house_train)

cors <- cor(na.omit(df_house_train[, unlist(lapply(df_house_train, is.numeric))]))
corrplot(cors, type="upper", tl.cex = 0.5, diag = F, order = "FPC")

#================ FEATURE SELECTION ====================================

model <- randomForest(SalePrice ~ . 
                      -Id, 
                      data = df_house_train, 
                      ntree = 100, 
                      nodesize = 10,
                      importance = TRUE)

varImpPlot(model)


columnsWithMoreImp <- c("GrLivArea", "Neighborhood", "OverallQual", "TotalBsmtSF",
                        "BsmtF_F", "LotArea", "GarageCars", "TotBathInHouse_F", "ExterQual",
                        "OverallCond", "MSZoning", "BsmtQual", "TotRmsAbvGrd", "TotPorchArea_F")


summary(df_house_train[, columnsWithMoreImp])

for(col in columnsWithMoreImp){
  g <- ggplot(df_house_train, aes(x = df_house_train[, col] , y = SalePrice)) +
    geom_point() +
    labs(title=paste("SalePrice per", col), x=col, y = "SalePrice")
  print(g)
}

#================ CLEANING AND TRANSFORMING THE DATAS ==================
# TREATING OUTLIERS 
outlier(df_house_train$SalePrice)

df_house_train %>%
  select(Id, SalePrice) %>%
  arrange(desc(SalePrice)) %>%
  head()

# eliminating the outliers
df_house_train <- subset(df_house_train, !(df_house_train$Id %in% c(692, 1183)))

# verifing the normality
infoNormality(df_house_train, "LotArea")
infoNormality(df_house_train, "TotalBsmtSF")
infoNormality(df_house_train, "GrLivArea")
infoNormality(df_house_train, "TotPorchArea_F")
infoNormality(df_house_train, "BsmtF_F")
infoNormality(df_house_train, "SalePrice")

# put in normal distribuition 
for(var in c("LotArea", "GrLivArea", "SalePrice")){
  df_house_train[, var] <- log10(df_house_train[, var])  
}

#================== SPLIT THE DATA IN TRAIN AND TEST  ==================
set.seed(101)
df_house_train$split <- sapply(df_house_train$Id, function(x)ifelse(runif(1, 0, 1) <= 0.8, 1, 0))

train <- subset(df_house_train, split == 1)
test <- subset(df_house_train, split == 0)
target <- test$SalePrice

train$Id <- NULL
test$Id <- NULL

test$split <- NULL
train$split <- NULL

test$SalePrice <- NULL


#================== CREATING THE MODELS OF MACHINE LEARNING ============
model_lm <- lm(SalePrice ~ 
                 GrLivArea +
                 Neighborhood +
                 OverallQual + 
                 TotalBsmtSF + 
                 BsmtF_F +
                 LotArea + 
                 GarageCars + 
                 TotBathInHouse_F + 
                 ExterQual + 
                 OverallCond + 
                 MSZoning + 
                 BsmtQual +
                 TotRmsAbvGrd + 
                 TotPorchArea_F,
              data=train)

pred_lm <- predict(model_lm, test[, columnsWithMoreImp])

summary(model_lm)

model_rf <- randomForest(SalePrice ~ 
               GrLivArea +
               Neighborhood +
               OverallQual + 
               TotalBsmtSF + 
               BsmtF_F +
               LotArea + 
               GarageCars + 
               TotBathInHouse_F + 
               ExterQual + 
               OverallCond + 
               MSZoning + 
               BsmtQual +
               TotRmsAbvGrd + 
               TotPorchArea_F,
               ntree = 150,
               mtry = 10,
             data=train)

pred_rf <- predict(model_rf, test[, columnsWithMoreImp])

rmse(target, pred_rf)

#================== INTERPRETING THE RESIDUALS =========================
# residuals for linear model

residuals_lm <- data.frame(real = 10^target, predict = 10^pred_lm) %>%
  mutate(residuals = real - predict)

residuals_lm %>%
  select(residuals) %>%
  summarise(mean = mean(residuals), median = median(residuals),
            sd = sd(residuals), sum = sum(residuals))

# verify if the residuals are in normal distribuition
ggplot(residuals_lm, aes(x=residuals)) +
  geom_histogram(bins=30)

shapiro.test(residuals_lm$residuals) 

# residuals for random forest model
residuals_rf <- data.frame(real = 10^target, predict = 10^pred_rf) %>%
  mutate(residuals = real - predict)

residuals_rf %>%
  select(residuals) %>%
  summarise(mean = mean(residuals), median = median(residuals),
            sd = sd(residuals), sum = sum(residuals))

# verify if the residuals are in normal distribuition
ggplot(residuals_rf, aes(x=residuals)) +
  geom_histogram(bins=30)

shapiro.test(residuals_rf$residuals) 
#=================== MAKING THE SAME FOR THE TEST DATA =================

df_house_test <- read.csv("../Dados/test.csv")

df_house_test <- deleteColumns(df_house_test, columnsWithManyNA)
df_house_test <- deleteColumns(df_house_test, collinearColumns)

df_house_test$GarageCars <- imputationByMeanMedian(df_house_test$GarageCars, mean(na.omit(df_house_test$GarageCars)), median(na.omit(df_house_test$GarageCars)))
df_house_test$TotalBsmtSF <- imputationByMeanMedian(df_house_test$TotalBsmtSF, mean(na.omit(df_house_test$TotalBsmtSF)), median(na.omit(df_house_test$TotalBsmtSF)))
df_house_test$BsmtQual <- fillNA(df_house_test, "BsmtQual", c("Ex", "Fa", "Gd", "TA"), c(0.09, 0.038, 0.41, 0.45))
df_house_test$MSZoning <- fillNA(df_house_test, "MSZoning", c("C (all)", "FV", "RH", "RL", "RM"), c(0.01, 0.05, 0.007, 0.76, 0.16))
df_house_test$BsmtFinType1 <- fillNA(df_house_test, "BsmtFinType1", c("ALQ", "BLQ", "GLQ", "LwQ", "Rec", "Unf"), c(0.15, 0.086, 0.3, 0.06, 0.11, 0.3))


summary(df_house_test)

df_house_test$LotShape_F <- sapply(df_house_test$LotShape, function(x){ifelse(x == "Reg", 1, 0)})  
df_house_test$Functional_F <- sapply(df_house_test$Functional, function(x){ifelse(x == "Typ", 1, 0)})  


df_house_test$Utilities_F <- sapply(df_house_test$Utilities, function(x){ifelse(x == "AllPub", 1, 0)})  
df_house_test$Street_F <- sapply(df_house_test$Street, function(x){ifelse(x == "Pave", 1, 0)})  
df_house_test$Heating_F <- sapply(df_house_test$Heating, function(x){ifelse(x == "GasA", 1, 0)})  
df_house_test$CentralAir_F <- sapply(df_house_test$CentralAir, function(x){ifelse(x == "Y", 1, 0)})  
df_house_test$TotPorchArea_F <- df_house_test$EnclosedPorch +
                                df_house_test$WoodDeckSF +
                                df_house_test$OpenPorchSF +
                                df_house_test$ScreenPorch 



df_house_test$BsmtFinType1 <- sapply(df_house_test$BsmtFinType1, 
                                      function(x){
                                        if(x == "GLQ")
                                          return(1)
                                        else if( x == "ALQ")
                                          return(0.85)
                                        else if(x == "BLQ")
                                          return(0.5)
                                        else if(x == "Rec")
                                          return(0.85)
                                        else if(x == "LwQ")
                                          return(0.3)
                                        else if(x == "Unf")
                                          return(0.05)
                                      })

for(indice in 1:(dim(df_house_test)[1])){
  df_house_test[indice, "BsmtF_F"] <- df_house_test[indice, "BsmtFinSF1"] * df_house_test[indice, "BsmtFinType1"]
}

df_house_test <- deleteColumns(df_house_test, c("BsmtFinSF1", "BsmtFinType1"))


oldColumns <- c("LotShape", "Functional", "Street", "Utilities", "Heating", "CentralAir")
columnsBath <- c("FullBath", "HalfBath", "BsmtHalfBath", "BsmtFullBath")
columnsWithCorr0 <- c("LowQualFinSF", "MiscVal", "X3SsnPorch", "MSSubClass", "YrSold")
columnsPorch <- c("EnclosedPorch", "WoodDeckSF", "OpenPorchSF", "ScreenPorch")

df_house_test$TotBathInHouse_F <- df_house_test$FullBath + df_house_test$HalfBath + df_house_test$BsmtHalfBath + df_house_test$BsmtFullBath

df_house_test <- deleteColumns(df_house_test, columnsBath)
df_house_test <- deleteColumns(df_house_test, oldColumns)
df_house_test <- deleteColumns(df_house_test, columnsWithCorr0)
df_house_test <- deleteColumns(df_house_test, columnsPorch)

df_house_test[which(is.na(df_house_test$TotBathInHouse_F)), "TotBathInHouse_F"] <- 3
df_house_test[which(is.na(df_house_test$BsmtF_F)), "BsmtF_F"] <- mean(na.omit(df_house_test$BsmtF_F))

columnsWithMoreImp <- c("GrLivArea", "Neighborhood", "OverallQual", "TotalBsmtSF",
                        "BsmtF_F", "LotArea", "GarageCars", "TotBathInHouse_F", "ExterQual",
                        "OverallCond", "MSZoning", "BsmtQual", "TotRmsAbvGrd", "TotPorchArea_F")

summary(df_house_test[, columnsWithMoreImp])


for(var in c("LotArea", "GrLivArea")){
 df_house_test[, var] <- log10(df_house_test[, var])  
}

sum(is.na(df_house_test[, columnsWithMoreImp]))
dim(df_house_test)

pred_test_lm <- predict(model_lm, df_house_test[, columnsWithMoreImp])
pred_test_rf <- 10^predict(model_rf, df_house_test[, columnsWithMoreImp])
submit <- data.frame(Id = df_house_test$Id, SalePrice=pred_test_rf)

write.csv(submit, file="submit.csv", row.names=FALSE)
