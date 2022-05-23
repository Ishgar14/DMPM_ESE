# install.packages("tidyverse")
# install.packages("Hmisc")

setwd("C:\\VS_Workshop\\Sem 6\\Data Mining and Predictive Modelling\\Lab ESE Practice\\Ass3")
library(tidyverse)
library(dplyr)
library(Hmisc)

dirty_df = read.csv('ToyotaCorolla - Dirty.csv')


check = function(dataset) {
    print(cat("Number of null values", sum(is.na(dataset)), " "))
    print(cat("% of null values", mean(is.na(dataset)), " "))
    
    print("Mean of all colums")
    for (i in 1:ncol(dataset)) {
        print(mean(dataset[,i], na.rm = TRUE))
    }
}

is.na(dirty_df)
check(dirty_df)


head(rename(dirty_df, Kilometers = KM))

clean_df = na.omit(dirty_df)

head(clean_df %>% mutate(priceTrans = Price / 100))

head(select(clean_df, -MetColor))

head(arrange(clean_df, Age))

slice(clean_df, 4:17)

sample_n(clean_df, 20)

head(filter(clean_df, FuelType == 'Petrol'))

glimpse(clean_df)

boxplot(clean_df$Price)

boxplot(clean_df$Age)

boxplot(clean_df$Weight)

print("Outliers of Weight are ")
boxplot.stats(clean_df$Weight)$out


# Numerical Imputation
dirty_df$Age = impute(dirty_df$Age, fun=mean)

dirty_df$CC = impute(dirty_df$CC, fun=mean)

dirty_df$Weight = impute(dirty_df$Weight, fun=mean)

for (i in 1:ncol(dirty_df)) {
    print(sum(is.na(dirty_df[,i])))
}

print("Phew! No null values anymore!")
