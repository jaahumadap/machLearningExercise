# load data

train <- read.csv("pml-training.csv",h=T)
test <- read.csv("pml-testing.csv",h=T)
dim(train); dim(test)
library(plyr)
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
test <- test[,-c(1,indx)]
#tests

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)



fitControl <- trainControl(method = "cv",
                           number = 15,
                           allowParallel = TRUE)

x <- train[,-c(2,3,56)]
y <- train[,56]

modrf <- train(x,y,data=train,method="rf",trainControl = fitControl)

modrfPCA <- train(x,y,data=train,method="rf", preProcess="pca", trainControl = fitControl)


stopCluster(cluster)
modgbm2 <- train(classe ~ ., data=train[,-c(2,3)], method="gbm",verbose=FALSE) #did not work
modgbmPCA <- train(classe ~ ., data=train[,-c(2,3)], method="gbm",verbose=FALSE, preProcess = "pca")

# Display the results
results <- resamples(list(RF=modrf, RF_PCA=modrfPCA, GBM = modgbm, GBM_PCA = modgbmPCA))

bwplot(results)

#Predict classe in validation set and calculate accuracy
#Random forest
predrf <- predict(modrf, val)
confusionMatrix(predrf,val$classe)$overall

#Random forests with PCA
predrfPCA <- predict(modrfPCA, val)
confusionMatrix(predrfPCA,val$classe)$overall

#GBM
predgbm <- predict(modgbm, val)
confusionMatrix(predgbm,val$classe)

#GBM with PCA
predgbmPCA <- predict(modgbmPCA, val)
confusionMatrix(predgbmPCA,val$classe)

#generate new predictions for test data set
preds <- predict(modrf, test[,-56])
data.frame(problem_id=test[,56], prediction=preds)
