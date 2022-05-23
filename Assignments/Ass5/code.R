setwd("C:\\VS_Workshop\\Sem 6\\Data Mining and Predictive Modelling\\Assignments\\Ass5")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(Metrics)
library(caret)
library(scales)
library(caTools)
library(corrplot)

dataset <- read.csv("AB_NYC_2019.csv")

head(dataset)
summary(dataset)

# Find NA Values
print(colSums(is.na(dataset)))

# Fill 0 into NA
dataset$reviews_per_month[is.na(dataset$reviews_per_month) == TRUE] <- 0

# split the column values of last_review into year, month and day by delimiter pf '-'
data_new <- tidyr::separate(dataset, last_review, c("Year", "Month", "Day"), sep = "-")

# Put 0 whereever we find NA values
data_new$Year[is.na(data_new$Year) == TRUE] <- 0
data_new$Month[is.na(data_new$Month) == TRUE] <- 0
data_new$Day[is.na(data_new$Day) == TRUE] <- 0

data_new$neighbourhood_group <- as.factor(data_new$neighbourhood_group)
data_new$room_type <- as.factor(data_new$room_type)
data_new$Year <- as.integer(data_new$Year)
data_new$Month <- as.integer(data_new$Month)
data_new$Day <- as.integer(data_new$Day)

head(data_new)
print(colSums(is.na(data_new)))
data_new <- na.omit(data_new)
print(colSums(is.na(data_new)))

# Correlation
correlation <- cor(data_new[, sapply(data_new, is.numeric)])
corrplot(cor(data_new[, sapply(data_new, is.numeric)]))

# Plotting the graphs
ggplot(data = data_new, mapping = aes(neighbourhood_group, fill = room_type)) +
  geom_bar()

price_roomtype <- data_new %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(Mean_Price = mean(price))

ggplot(price_roomtype, aes(x = reorder(neighbourhood_group, -Mean_Price), y = Mean_Price, fill = room_type)) +
  geom_bar(stat = "identity", colour = "black", position = position_dodge())

ggplot(data = data_new, mapping = aes(number_of_reviews, price)) +
  geom_point() +
  facet_wrap(data_new$room_type)

ggplot(data = data_new, mapping = aes(neighbourhood_group, availability_365)) +
  geom_boxplot()


# Building the model
model <- lm(price ~ host_id + neighbourhood_group + latitude + longitude + room_type + minimum_nights + number_of_reviews +
  Year + calculated_host_listings_count + availability_365, data = data_new)

print(model)
print(summary(model))

pred1 <- predict(model)
resd1 <- residuals(model)

x <- cbind(data_new$price, pred1)
x <- data.matrix(x)
x <- rescale(x)
x <- as.data.frame(x)

mae <- MAE(x$V1, x$pred1)
mse <- mse(x$V1, x$pred1)
rmse <- RMSE(x$V1, x$pred1)
r2 <- R2(x$V1, x$pred1)

cat("\nMAE:", mae, "\nMSE:", mse, "\nRMSE:", rmse, "\nR-squared:", r2)
