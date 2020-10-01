install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")
install.packages("ggpubr")
library("ggpubr")
install.packages("readxl")
library("readxl")
install.packages("ggpubr")
library("ggpubr")
library(tidyverse)
library(broom)
install.packages("mctest")
library(mctest)
library(investr)
library(robustbase)
library(tidyverse)
install.packages("caret")
library(caret)
install.packages("corrplot")
library(corrplot)
library(factoextra)

oscarsDF <- data.frame(read.csv("C:\\Users\\smuke\\Downloads\\the_oscar_award.csv"))
movieDf <- data.frame(read.csv("C:\\Users\\smuke\\Downloads\\movie_industry.csv"))
genome_scores <- data.frame(read.csv("C:\\Users\\smuke\\Downloads\\movie_lense\\movie_lense\\genome-scores.csv"))
tags <- data.frame(read.csv("C:\\Users\\smuke\\Downloads\\movie_lense\\movie_lense\\tags.csv"))
links <- data.frame(read.csv("C:\\Users\\smuke\\Downloads\\movie_lense\\movie_lense\\links.csv"))
movienames <-data.frame(read.csv("C:\\Users\\smuke\\Downloads\\movie_lense\\movie_lense\\movies.csv"))
numericMoveDF <- movieDf %>% select(budget, gross, runtime, score, votes, year, genre)
numericMoveDF$netrevenue <- numericMoveDF$gross - numericMoveDF$budget
corMovieDF <- cor(numericMoveDF, method = c("pearson", "kendall", "spearman"))
corrplot(corMovieDF, method="pie")
fit <- lm(netrevenue ~ score + votes, data = numericMoveDF)

#Finds the most relevant tags for the movies
MovieID <- left_join(genome_scores, movienames) %>%
      filter(genome_scores$movieId != movienames$movieId)
MovieID <- subset(MovieID, select = -c(genres))
MovieID <- MovieID %>% filter(MovieID$relevance > .9)  
MovieID <- transform(MovieID, title = substr(MovieID$title, 1, nchar(as.character(MovieID$title))-7))
MovieID <- rename(MovieID, c("name"="title"))
#Adding gross and budget and net revenue
MovieID$gross <- movieDf$gross[match(MovieID$name, movieDf$name)]
MovieID$budget <- movieDf$budget[match(MovieID$name, movieDf$name)]
MovieID$netrevenue <- MovieID$gross - MovieID$budget


#Try k-means (didn't work)
set.seed(123)
fviz_nbclust(movieDf, kmeans, method = "silhouette")
fviz_nbclust(numericMoveDF, kmeans, method = "silhouette")
km.res <- kmeans(movieDf, 2, nstart = 25)
dd <- cbind(numericMoveDF, cluster = km.res$cluster)
