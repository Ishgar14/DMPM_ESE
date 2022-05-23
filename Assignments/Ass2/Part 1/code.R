df = read.csv("HT-WT-Age.csv")[2:4]

head(df)
summary(df)


model1 = lm(df$Weight ~ df$Height)
model1


pred1 = predict(model1)
resd1 = residuals(model1)

summary(model1)


plot(df$Weight, df$Height, 
     main = "Height and Weight",
     abline(lm(df$Height ~ df$Weight)),
     ylab = "Height in cm",
     xlab = "Weight in kg"
)

plot(df$Height, resd1, 
     main = "Residual Plot(HT and WT)",
     abline(0,0),
     ylab = "Residuals",
     xlab = "Height in cm"
)



model2 <- lm(df$Weight ~ df$Age)

print(model2)
print(summary(model2))

pred2 <- predict(model2)
resd2 <- residuals(model2)

print(pred2)
print(resd2)

plot(df$Weight,
     df$Age,
     main = "Age and Weight",
     abline(lm(df$Age~df$Weight)),
     ylab = "Age in years",
     xlab = "Weight in kg"
)

plot(df$Age,
     resd2,
     main = "Residual Plot(Age and WT)",
     abline(0,0),
     ylab = "Residuals",
     xlab = "Age in years"
)

