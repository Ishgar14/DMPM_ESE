library(Hmisc)
library(corrplot)
library(dplyr)
library(tidyverse)
library(Metrics)

df = read.csv('C:\\VS_Workshop\\Sem 6\\Data Mining and Predictive Modelling\\Assignments\\Ass5\\insurance.csv')

head(df)
summary(df)

# We need to convert categorical data to numeric data aka encoding the data
encode_category = function(x, order = unique(x)) {
    as.numeric(factor(x, levels = order, exclude = NULL))
}

df[['sex']] = encode_category(df[['sex']])
df[['smoker']] = encode_category(df[['smoker']])
df[['region']] = encode_category(df[['region']])

# Plotting graphs
corrplot(cor(df), method='circle', diag=FALSE)

ggplot(data = df, aes(x = sex, y = smoker)) + 
    geom_violin(color = 'red')

plot(table(df$sex, df$smoker))

ggplot(data = df, aes(x = smoker)) + 
    geom_histogram()

ggplot(data = df, aes(x = region)) + 
    geom_bar(color = 'green', alpha = .5)

# Building the model
model = lm(charges ~ age + smoker + bmi, df)
model

summary(df)

predictions = predict(model)

# predictions

mae1 = mae(df$charges, predictions)
mse1 = mse(df$charges, predictions)


cat(
    cat("Mean Absolute Error: ", mae1),
    cat("\nMean Squared Error: ", mse1),
    cat("\nMean Squared Error: ", mse1),
    cat("\n")
)


cat("Hi", mae1)
