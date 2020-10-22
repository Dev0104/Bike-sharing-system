library(caret)
library(pROC)
library(mlbench)
#Importing the data into R studio

bike_hour<-read.csv("D:\\DMML\\Data Sets\\hour.csv")

#Checking missing values
p = function(x) {sum(is.na(x)/length(x))*100}  #Function to calculate the percentage of missing values
apply(bike_hour, 2, p)  

#Deleting the redundant variables for analysis.
bike_hour <- bike_hour[, -c(1,2,3,4,5,6,12,15)]
bike_hour
str(bike_hour)
summary(bike_hour)

#Splitting the data into training and testing set
ran <- sample(1:nrow(bike_hour),0.7 * nrow(bike_hour)) #70 % training data and 20% test data

trainset <- bike_hour[ran,]
str(bike_hour)
testset<-bike_hour[-ran,]
str(testset)
dim(trainset)
dim(testset)


train_target <-trainset[,1]
test_target<-testset[,1]
library("class")

#Applying the KNN model.
#install.packages('FNN')
library(FNN)
reg_results <- knn.reg(trainset[,-4], testset[,-4], trainset$cnt, k = 132) #k value was selected as a square root of the total number of observations
str(reg_results)
reg_results

summary(reg_results)

#Evaluation
library(ModelMetrics)
rms_e <- rmse(test_target,reg_results$pred)
rms_e
MSE = mse(test_target,reg_results$pred)    #Mean square error
MSE
MAE = mae(test_target,reg_results$pred)    #Mean absolute error
MAE

