install.packages("party")
library(party)
View(readingSkills)
print(head(readingSkills))
input.dat <- readingSkills[c(1:105),] # Create the input data frame.

# Create the tree.
output.tree <- ctree(nativeSpeaker ~ age + shoeSize + score,data = input.dat)

# Plot the tree.

png(file = "decision_tree.png")   ##  if wand  to  save 
plot(output.tree)
dev.off()


#Conclusion From the decision tree shown above we can conclude that anyone whose 
#readingSkills score is less than 38.3 and age is more than 6 is not a native Speaker.


library(datasets)
installed.packages('caTools')
library(caTools)
library(party)
library(dplyr)
library(magrittr)

##Splitting dataset into 4:1 ratio for train and test data
sample_data = sample.split(readingSkills, SplitRatio = 0.8)
train_data <- subset(readingSkills, sample_data == TRUE)
test_data <- subset(readingSkills, sample_data == FALSE)

# Create the decision tree model using ctree and plot the model 
model<- ctree(nativeSpeaker ~ ., train_data)
plot(model)


# testing the people who are native speakers # and those who are not
predict_model<-predict(model, test_data)
predict_model

# creates a table to count how many are classified
# as native speakers and how many are not
m_at <- table(test_data$nativeSpeaker, predict_model)
m_at

#Determining the accuracy of the model developed 

ac_Test <- sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for test is found to be', ac_Test))







