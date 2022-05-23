library(dplyr)
library(caret)
library(reshape2)
library(pROC)
library(corrplot)
library(caTools)

flight = read.csv("FlightDelays.csv")
head(flight)
summary(flight)
str(flight)

table(flight$delay)
flight = flight %>% mutate(delay = ifelse(delay == "ontime",0,1))


# We need to convert categorical data to numeric data aka encoding the data
encode_category = function(x, order = unique(x)) {
    as.numeric(factor(x, levels = order, exclude = NULL))
}


flight[["tailnu"]]  = encode_category(flight[["tailnu"]])
flight[["dest"]]    = encode_category(flight[["dest"]])
flight[["origin"]]  = encode_category(flight[["origin"]])
flight[["carrier"]] = encode_category(flight[["carrier"]])

# We don't really need date 
flight = select(flight, -date)
head(flight)


# Split the data for training and testing sets
set.seed(101)
sample = sample.split(flight$delay, SplitRatio = .70)
train  = subset(flight, sample == TRUE)
test   = subset(flight, sample == FALSE)

count(train)
count(test)

# Plot the correlation heat map
corrplot(cor(train), tl.col="black")


# Build the model
logreg = glm(delay ~ ., family = binomial(link = 'logit'),
              data = train)
summary(logreg)

# Predict the values using the model
prob = logreg %>% predict(test, type = "response")


test$prob = prob
threshold = 0.3

head(select(test, prob))

# If prediction is less than threshold then put 0 otherwise 1
test = test %>% mutate(predicted = ifelse(prob < threshold,0,1))
head(test)

# The confusion matrix
mat = table(test$delay, test$predicted)
mat

# Metrics to check efficiency of model
accuracy   = (mat[1] + mat[4]) / (sum(mat))
error_rate = 1 - accuracy
precision  = mat[1] / (mat[1] + mat[3])
recall     = mat[1] / (mat[1] + mat[2])

cat("Accuracy: ", accuracy * 100,
    "%\nError Rate:", error_rate * 100,
    "%\nPrecision: ",precision * 100,
    "%\nRecall:",recall * 100,"%")

# ROCR curve
roc = roc(test$delay ~ prob, plot = TRUE, print.auc = TRUE)
