
## *** European Protein Consumption, in grams/person-day *** ##
# read in the data
food <- read.csv(choose.files())
head(food)

# first, clustering on just Red and White meat (p=2) and k=3 clusters
set.seed(1) # to fix the random starting clusters
grpMeat <- kmeans(food[, c("WhiteMeat", "RedMeat")], centers = 3, iter.max = 10, nstart = 10)
grpMeat


# list of cluster assignments
o <- order(grpMeat$cluster)
data.frame(food$Country[o], grpMeat$cluster[o])

# plotting cluster assignments on Red and White meat scatter plot
plot(food$Red, food$White, type = "n", xlim = c(3, 19), xlab = "Red Meat", ylab = "White Meat")
text(x = food$Red, y = food$White, labels = food$Country, col = grpMeat$cluster + 1)

# same analysis, but now with clustering on all protein groups
# change the number of clusters to 7
set.seed(1)
grpProtein <- kmeans(food[, -1], centers = 7, iter.max = 10, nstart = 10)
o <- order(grpProtein$cluster)

data.frame(food$Country[o], grpProtein$cluster[o])
plot(food$Red, food$White, type = "n", xlim = c(3, 19), xlab = "Red Meat", ylab = "White Meat")
text(x = food$Red, y = food$White, labels = food$Country, col = rainbow(7)[grpProtein$cluster])
