#Libraries 
library(caret)
library(pROC)
library(mlbench)

#Example-1 Classification
data <- read.csv("binary.csv")
str(data)
data$admit[data$admit==0] <- "No"
data$admit[data$admit==1] <- "Yes" 
str(data)
data$admit <- as.factor(data$admit)

#Data Partition
set.seed(111)
ind <- sample(2,nrow(data),replace=T,prob=c(0.8,0.2))
training <- data[ind==1,]
test <- data[ind==2,]

#KNN model
#To train the model we use the repeated cross validation method
krControl <- trainControl(method="repeatedcv",
                          number=10,
                          repeats=3)
fit <- train(admit~.,data=training,method="knn",
             tuneLength=20,
             trControl=krControl,
             preProc=c("center","scale"))

#Model performance
plot(fit)
#varriable importance
varImp(fit)
#Prediction
pred <- predict(fit,newdata=test)
confusionMatrix(pred,test$admit)

#with Roc
krControl <- trainControl(method="repeatedcv",
                          number=10,
                          repeats=3,
                          classProbs = T,
                          summaryFunction = twoClassSummary)
fit <- train(admit~.,data=training,method="knn",
             tuneLength=20,
             trControl=krControl,
             preProc=c("center","scale"),
             metric="ROC",
             tuneGrid=expand.grid(k=1:60))
fit
plot(fit)
varImp(fit)
pred <- predict(fit,newdata=test)
confusionMatrix(pred,test$admit)


#Example2:BostonHousing for Regression

data <- BostonHousing
str(data)

#Data Partition
set.seed(111)
ind <- sample(2,nrow(data),replace=T,prob=c(0.8,0.2))
training <- data[ind==1,]
test <- data[ind==2,]
#Method
trControl <- trainControl(method="repeatedcv",
                          number=10,
                          repeats = 3)
fit <- train(medv ~.,data=training,
             tuneGrid=expand.grid(k=1:60),
             method="knn",
             trControl=trControl)
#performance
fit
plot(fit)
varImp(fit)
pred <- predict(fit,test)
RMSE(pred,test$medv)
plot(pred~test$medv)
