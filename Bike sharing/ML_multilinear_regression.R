
#Importing data
bike_hour <- read.csv("D:\\DMML\\Data Sets\\hour.csv")
View(bike_hour)

#Checking missing values
p = function(x) {sum(is.na(x)/length(x))*100}  #Function to calculate the percentage of missing values
apply(bike_hour, 2, p)  

#Viewing the structure of the data
head(bike_hour)
str(bike_hour)
summary(bike_hour)

#Removing unnecessary columns
bike_hour <- bike_hour[, -c(1,2,3,4,5,6,12,15)]
bike_hour
#pairs(bike_hour[1:9])     #Matrix of scatterplot


#Splitting the data
set.seed(222)
library(caTools)
split = sample.split(bike_hour, SplitRatio = 0.7)
split
train = subset(bike_hour, split=="TRUE")
test = subset(bike_hour, split=="FALSE")

#Building the model
model1 = lm(cnt~holiday+weekday+workingday+temp+windspeed+registered+hum+weathersit,data=train)
summary(model1)
#Windspeed is removed as it has no significance in predicting the count as seen in the summary of model1
model2 = lm(cnt~holiday+weekday+workingday+temp+registered+hum+weathersit,data=train)
summary(model2)
model2


#Prediction
pred = predict(model2,test)
pred
summary(pred)

#Comparing predicted vs actual
par(mfrow=c(1,1))
plot(test$cnt, type="l",lty=1.8,col="red")
lines(pred, type="l",lty=1.8,col="blue")

#Accuracy
library(Metrics)
RMSE = rmse(pred,bike_hour$cnt)  #Root mean square error
RMSE
MSE = mse(pred,bike_hour$cnt)    #Mean square error
MSE
MAE = mae(pred,bike_hour$cnt)    #Mean absolute error
MAE

