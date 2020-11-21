library(ggplot2)
library(readr)
library(fpc)
library(class)

dclust <- read_csv("divclust.csv")
View(dclust)

# Set maximum cluster and define function 
max_k <- 15
kmean_withinss <- function(k) {
  cluster <- kmeans(dclust, k)
  return (cluster$tot.withinss)
}

# Run algorithm over a range to create a data frame to plot the graph
wss <- sapply(2:max_k, kmean_withinss)
elbow <-data.frame(2:max_k, wss)

# Plot the graph with ggplot
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point(size=3, colour="#CC0000")  +
  geom_line(aes(group=1), colour="#000099") + theme_light()+
  scale_x_continuous(breaks = seq(1, 15, by = 1)) + ggtitle("Dividend Yield Elbow Plot")
ggsave("DividendYieldElbow.png", width = 5, height = 5)

# Create a cluster with the number of nodes determined by the With-In-Sum-Of-Squares
k5 <- kmeans(dclust, centers = 5, nstart = 100)
ggplot(dclust, aes(dclust$x, dclust$y, color=k5$cluster)) +
  theme_light()+ geom_point() + ggtitle("Dividend Yield Clusters") +
  scale_colour_gradientn(colours=rainbow(3))
ggsave("DividentYieldClust.png", width = 5, height = 5)

## Repeat the process for Price to Sales ratio

psclust <- read_csv("psclust.csv")
View(psclust)

kmean_withinss <- function(k) {
  cluster <- kmeans(psclust, k)
  return (cluster$tot.withinss)
}

wss <- sapply(2:max_k, kmean_withinss)
elbow <-data.frame(2:max_k, wss)


ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point(size=3, colour="#CC0000")  +
  geom_line(aes(group=1), colour="#000099") + theme_light()+
  scale_x_continuous(breaks = seq(1, 15, by = 1)) + ggtitle("Price to Sales Elbow Plot")
ggsave("Price2SalesElbow.png", width = 5, height = 5)

k6 <- kmeans(psclust, centers = 6, nstart = 100)
ggplot(psclust, aes(psclust$x, psclust$y, color=k6$cluster)) +
  theme_light()+ geom_point() + ggtitle("Price to Sales Clusters") +
  scale_colour_gradientn(colours=rainbow(3))
ggsave("Price2Sales6.png", width = 5, height = 5)
