setwd("C:\\VS_Workshop\\Sem 6\\Data Mining and Predictive Modelling\\Assignments\\Ass6")

# install.packages("party")
library(party)
# Check for inbuilt datasets by running `data()`
View(iris)
head(iris)
dim(iris)
input_dataset <- iris

# Create the tree.
tree_model <- ctree(Species ~ ., data = input_dataset)
plot(tree_model)

#Conclusion From the decision tree shown above we can conclude that 
# anyone whose
#iris score is less than 38.3 and age is more than 6 is not a native Speaker.

library(datasets)
# installed.packages('caTools')
library(caTools)
library(party)
library(dplyr)
library(magrittr)

#Splitting dataset into 4:1 or 80:20 ratio for train and test data
sample_data <- sample.split(iris, SplitRatio = 0.8)
train_data <- subset(iris, sample_data == TRUE)
test_data <- subset(iris, sample_data == FALSE)

# Create the decision tree model using ctree and plot the model
model <- ctree(Species ~ ., train_data)
plot(model)


# testing the people who are native speakers # and those who are not
predict_model <- predict(model, test_data)
predict_model

# creates a table to count how many are classified
# as native speakers and how many are not
# Basically confusion matrix
conf_matrix <- table(test_data$Species, predict_model)
conf_matrix

# Metrics
paste(conf_matrix[2, ])
paste(conf_matrix[1], conf_matrix[5], conf_matrix[8], conf_matrix[9])
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

paste("Accuracy for test is found to be", accuracy)
