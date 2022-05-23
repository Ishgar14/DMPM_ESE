setwd("C:\\VS_Workshop\\Sem 6\\Data Mining and Predictive Modelling\\Assignments\\Ass6") # nolint

library(datasets)
library(caTools)
library(caret)
library(Metrics)
library(party)
library(dplyr)
library(magrittr)
library(tree)
library(corrplot)

df = read.csv("./prostate.csv")
head(df)

corrplot(cor(df), diag=FALSE)

# lbph doesnt have enough correlation with lcavol so drop it
df = select(df, -lbph)
head(df)

#Splitting dataset into 4:1 or 80:20 ratio for train and test data
sample_data <- sample.split(df, SplitRatio = 0.8)
train_data <- subset(df, sample_data == TRUE)
test_data <- subset(df, sample_data == FALSE)

dim(train_data)
dim(test_data)

# Create the decision tree model using ctree and plot the model
model <- tree(lcavol ~ ., train_data, mincut=1)
# The minimum number of observations 
# to include in either child node = 1  
model
plot(model)
text(model)

# m = cv.tree(model, K=9)
# plot(m)

# Pruning the tree
prune.tree(model)

cut_model = prune.tree(model, k=9)
cut_model = prune.tree(model, k=1)
cut_model
plot(cut_model)
text(cut_model)

errors = function(pred) {
    mae <- MAE(test_data$lcavol, pred)
    mse <- mse(test_data$lcavol, pred)
    rmse <- RMSE(test_data$lcavol, pred)
    r2 <- R2(test_data$lcavol, pred)

    cat("\nMean Absolute Error:", mae, "\nMean Squared Error:", mse, "\nRMSE:", rmse, "\nR-squared:", r2, "\n")
}

errors(predict(model, test_data))
errors(predict(cut_model, test_data))
