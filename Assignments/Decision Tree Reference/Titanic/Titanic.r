## The purpose of this dataset is to predict
## which people are more likely to survive after
## the collision with the iceberg.
## The dataset contains 13 variables and 1309 observations.
## The dataset is ordered by the variable x

setwd("C:\\VS_Workshop\\Sem 6\\Data Mining and Predictive Modelling\\Assignments\\Ass6") # nolint

install.packages("tree")
library(tree)
set.seed(678) ##   for  sampling  function

titanic <- read.csv(file.choose()) ##  read  dataset
View(titanic)
head(titanic)
tail(titanic)

shuffle_index <- sample(1:nrow(titanic)) ## randomize order to include all classes
titanic <- titanic[shuffle_index, ] ##  put  records  in a shuffled  order

## Drop variables home.dest,cabin, name, x and ticket
## Create factor variables for pclass and survived
## Drop the NA


library(dplyr)

## Clean the dataset
clean_titanic <- select(.data = titanic, -c("home.dest", "cabin", "name", "x", "ticket"))
# Convert to factor level
clean_titanic <- mutate(
    .data = clean_titanic,
    pclass = factor(pclass, levels = c(1, 2, 3), labels = c("Upper", "Middle", "Lower")),
    survived = factor(survived, levels = c(0, 1), labels = c("No", "Yes"))
)
na.omit(clean_titanic)
glimpse(clean_titanic)
View(clean_titanic)

##  Create train/test set
n <- nrow(clean_titanic)
n1 <- floor(0.8 * n)
n2 <- n - n1
train <- sample(1:n, n1)

data_train <- clean_titanic[train, ]
data_test <- clean_titanic[-train, ]
View(data_train)
View(data_test)
dim(data_train)
dim(data_test)
prop.table(table(data_train$survived))
prop.table(table(data_test$survived)) ## In both dataset, survivors same = 40 percent.

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)


fit <- rpart(survived ~ ., data = data_train, method = "class")
rpart.plot(fit, extra = 101)

##  make  prediction
predict_unseen <- predict(fit, data_train, type = "class")
table_mat <- table(data_train$survived, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test