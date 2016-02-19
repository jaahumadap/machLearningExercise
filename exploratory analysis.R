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
train <- train[,-indx]
train <- train[,-1]
##train3 <- filter(train, new_window=="yes")

#create a validation set to test model performance
valindx <- createDataPartition(train$classe,p=0.80)[[1]]
train <- train[valindx,]
val <- train[-valindx,]
indx <- grep("kurtosis_|skewness_|max_|min_|amplitude_|var_|avg_|stddev_|timestamp",names(test))
test <- test[,-indx]
#tests

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)



fitControl <- trainControl(method = "cv",
                           number = 15,
                           allowParallel = TRUE)

x <- train[,-56]
y <- train[,56]

system.time(modrf <- train(x,y,data=train,method="rf",trainControl = fitControl))

modrfPCA <- train(x,y,data=train,method="rf", preProcess="pca", trainControl = fitControl)


stop
modgbm <- train(x,y,data=train,method="gbm",verbose=FALSE) #did not work
modbboost <- train(x,y,data=train,method="blackboost")
