# load data

train <- read.csv("pml-training.csv",h=T)
test <- read.csv("pml-testing.csv",h=T)
dim(train); dim(test)
library(dplyr)
library(parallel)
library(doParallel)
library(ggplot2)
library(caret)
indx <- grep("kurtosis_|skewness_|max_|min_|amplitude_|var_|avg_|stddev_|timestamp",names(train))
train2 <- train[,-indx]
train2 <- train2[,-1]
##train3 <- filter(train, new_window=="yes")

#create a validation set to test model performance
valindx <- createDataPartition(train2$classe,p=0.80)[[1]]
train <- train2[valindx,]
val <- train2[-valindx,]
indx <- grep("kurtosis_|skewness_|max_|min_|amplitude_|var_|avg_|stddev_|timestamp",names(test))
test2 <- test[,-indx]
#tests



modrf <- train(classe ~ ., preProc=c("pca"),data=train2,method="rf")
