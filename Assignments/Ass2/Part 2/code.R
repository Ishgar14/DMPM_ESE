library(Metrics)
library(caret)
library(dplyr)
library(scales)

# 1. Read the dataset
cars<-read.csv("ToyotaCorolla.csv")

# 2. Create a model
model<-lm(Price ~ Age + KM + FuelType + HP + MetColor + Automatic + CC + Doors + Weight,
          data = cars)
print(model)
print(summary(model))

# 3. Filter out the parameters with less significance
model<-lm(Price ~ Age + KM + FuelType + HP + CC + Weight, data = cars)
print(model)
print(summary(model))
pred1<-predict(model)
resd1<-residuals(model)

predict(model, data.frame(Age=5, KM=2000, FuelType="Diesel", HP=90, CC=2000, Weight=1200))


# 4. Scatter and Residual Plots
par(mfrow = c(2,1))
plot(cars$Age,resd1,main = "Residual Plot(Age and Price)",abline(0,0,col = "red"),ylab =
         "Residuals",xlab = "Price in $")
plot(cars$KM,resd1,main = "Residual Plot(KM and Price)",abline(0,0,col = "red"),ylab =
         "Residuals",xlab = "Price in $")
plot(cars$HP,resd1,main = "Residual Plot(HP and Price)",abline(0,0,col = "red"),ylab =
         "Residuals",xlab = "Price in $")
plot(cars$CC,resd1,main = "Residual Plot(CC and Price)",abline(0,0,col = "red"),ylab =
         "Residuals",xlab = "Price in $")

# 5. Metrics and Evaluation
x<-cbind(cars$Price,pred1)
x<-data.matrix(x)
x<-rescale(x)
x<-as.data.frame(x)
mae<-MAE(x$V1,x$pred1)
mse<-mse(x$V1,x$pred1)
rmse<-RMSE(x$V1,x$pred1)
r2<-R2(x$V1,x$pred1)
cat("\nMean Absolute Error:",mae,"\n\nMean Squared Error:",mse)
cat("\n\nRoot Mean Squared Error:",rmse,"\n\nR-squared:",r2,"\n\n")

# 6. Predictions
x=1:length(pred1)

plot(x, cars$Price, 
     pch=19, col = "yellow",main = "Model Evaluation",
     xlab = "Count", ylab = "Price")

lines(x, pred1,col="red")

legend("topright", legend = c("y-original", "y-predicted"),
       col = c("yellow", "red"), 
       pch = c(19,NA), lty = c(NA,1))
