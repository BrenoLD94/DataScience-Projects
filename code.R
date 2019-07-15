setwd("/home/breno/DataScience/Github/DataScience_projects/Titanic/DataScience_projects")

# the main goal is catch up 81% of accuracy in predict the possibility surviving 

# loading libraries
library(dplyr)
library(corrplot)
library(gmodels)

file <- "./train.csv"

# loading the dataset
df <- read.csv(file, stringsAsFactors = F)
summary(df)

View(df)
str(df)

# split the categorical and numerical data for exploratory analysis
df_numeric <- df[sapply(df, is.numeric) == T]
df_categoric <- df[sapply(df, is.numeric) == F] 

head(df_numeric)
head(df_categoric)

# exploratory analysis for numeric data
summary(df_numeric)

# there are missing values
sum(is.na(df_numeric))

df_cor <- cor(na.omit(df_numeric))
corrplot(df_cor, method="color")

# plot for verify the outliers values
boxplot(df_numeric$Age, main="BoxPlot fot the age", ylab="Age")
boxplot(df_numeric$Fare, main="BoxPlot fot the Fare", ylab="Fare")

# exploratory analysis for categoric data
summary(df_categoric)
View(df_categoric)

# more women has survived
CrossTable(x = df_categoric$Sex, y=df_numeric$Survived)

CrossTable(x = df_categoric$Embarked, y=df_numeric$Survived)

# cleaning the dataset
# treating the miss values for column age. The miss datas will be fill be the mean of the age
mean_age <- mean(na.omit(df$Age))
df$Age[is.na(df$Age)] <- mean_age
summary(df)

# the column Cabin has many values missing
df$Cabin <- NULL
# I'll assume that the name and ticket won't influence the survived
df$Name <- NULL
df$Ticket <- NULL
df$PassengerId <- NULL

df <- subset(df, Embarked != '')

df <- df %>% 
        select(Survived:Embarked) %>%
        mutate(Parent = SibSp + Parch)

# transforming the strings datas in numeric for the method Knn
df$Age <- as.integer(df$Age)

# convert the column sex in numeric 
sex_to_numeric <- function(input)
{
  if(input == "male")
  {
    return(1)
  }
  else
  {
    return(0)
  }
}

# converting the column embarked in numeric
embarked_to_numeric <- function(input)
{
  if(input == "S")
  {
    return(1)
  }
  else if(input == "C")
  {
    return(2)
  }
  else
  {
    return(3)
  }
}

# converting the column Fare to discrete 
Fare_to_discrete <- function(input){
  if(input <= 10)
  {
    return(0)
  }
  else if(input > 10 && input <= 33)
  {
    return(1)
  }
  else if(input > 33 && input <= 90)
  {
    return(2)
  }
  else
  {
    return(3)
  }
}

summary(df$Age)
Age_to_units <- function(input)
{
  if(input <= 10)
  {
    return(1)
  } 
  else if(input > 10 && input <= 18)
  {
    return(2)
  }
  else if(input > 18 && input <= 25)
  {
    return(3)
  }
  else if(input > 25 && input <= 40)
  {
    return(4)
  }
  else if(input > 40 && input <= 60)
  {
    return(5)
  }
  else 
  {
    return(6)
  }
}

df$Sex <- sapply(df$Sex , sex_to_numeric)
df$Embarked <- sapply(df$Embarked, embarked_to_numeric)
df$Fare <- sapply(df$Fare, Fare_to_discrete)
df$Age <- sapply(df$Age, Age_to_units)

df <- df %>%
  select(Survived:Parent) %>%
  mutate(AgeClass = Age*Pclass)

# split the datas in train and test
set.seed(100)
??sample.split
library(caTools)
amostra <- sample.split(df$Pclass, SplitRatio = 0.70)

train <- subset(df, amostra == T)
test <- subset(df, amostra == F)

# split the variable target to predict
target_train <- train$Survived
train$Survived <- NULL

target_test <- test$Survived
test$Survived <- NULL

summary(train)
summary(test)

??knn
library(class)

normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

train2 <- as.data.frame(lapply(train, normalizar))
test2 <- as.data.frame(lapply(test, normalizar))

# creating the model
model_knn1 <- knn(train = train2,
                  test = test2,
                  cl = as.factor(target_train),
                  k = 21)

CrossTable(x = target_test, y= model_knn2)

summary(train2)

# trying another machine learning method
??svm
library(e1071)

target_test2 <- test2$Survived
test2$Survived <- NULL

model_svm <- svm(Survived ~ .,
                  data = train2,
                  type = "C-classification",
                  kernel = "radial")

pred_svm <- predict(model_svm, test2)
mean(pred_svm == target_test2)

??randomForest
library(randomForest)
rf_model <- randomForest(factor(Survived) ~ .,
                         data = train2)


pred_rf <- predict(rf_model, test2)
mean(pred_rf == target_test2)

# making the same transformation that we did for test data
df_test <- read.csv("test.csv", stringsAsFactors = F)

df_test$Cabin <- NULL
df_test$Name <- NULL
df_test$Ticket <- NULL

mean_age_test <- mean(na.omit(df_test$Age))
df_test$Age[is.na(df_test$Age)] <- mean_age_test

df_test$Sex <- sapply(df_test$Sex , sex_to_numeric)
df_test$Embarked <- sapply(df_test$Embarked, embarked_to_numeric)
df_test$Fare <- sapply(df_test$Fare, Fare_to_discrete)
df_test$Age <- sapply(df_test$Age, Age_to_units)

df_test <- df_test %>% 
  select(PassengerId:Embarked) %>%
  mutate(Parent = SibSp + Parch)

df_test <- df_test %>%
  select(PassengerId:Parent) %>%
  mutate(AgeClass = Age*Pclass)

summary(df_test)

df_test[subset(df_test, is.na(Fare))$PassengerId == df_test$PassengerId, 7] <- mean(na.omit(df_test$Fare))

passId <- df_test$PassengerId
df_test$PassengerId <- NULL

df_test <- as.data.frame(lapply(df_test, normalizar))

predict_final <- predict(rf_model, df_test)  

result <- data.frame(PassengerId = passId, Survived = predict_final)

?write.csv
write.csv(result, "Submissions.csv", row.names = F)
