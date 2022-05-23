setwd("C:\\VS_Workshop\\Sem 6\\Data Mining and Predictive Modelling\\Assignments\\Ass10")

# install.packages("arules")
library("arules")

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

df = read.csv("./retail_dataset.csv")
encoded_df = df
head(df)
for(col in colnames(df)) {
    encoded_df[,col] = encode_ordinal(df[,col])
}

# Setup the rules using apriori algorithm
rules = apriori(df, parameter = list(support = 0.01, confidence = 0.3))
rules
summary(rules)

inspect(head(sort(rules, by = "confidence")))

inspect(head(sort(
            apriori(
                encoded_df, 
                parameter = list(support = 0.02, confidence = 0.4),
                # rhs = list("Cheese")
            ),
            by = "confidence"
        )
    )
)

