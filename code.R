setwd("/home/breno/DataScience/Github/DataScience_projects/Titanic/DataScience_projects")

# the main goal is catch up 85% of accuracy

# loading libraries
library(dplyr)
library(corrplot)

file <- "./train.csv"

# loading the dataset
df <- read.csv(file)

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

