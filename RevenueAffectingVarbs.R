install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")
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
#Remove null values
MovieID <- na.omit(MovieID)
#Get median net revenue by take
TagtoMedian <- MovieID %>%
  group_by(tagId) %>% 
  summarise(median_net = median(netrevenue))
ggplot(TagtoMedian, aes(tagId, median_net)) +
  geom_point(shape = 16, size = 1, show.legend = FALSE, color = "blue") +
  theme_minimal() +
  labs(x= "Tag ID", y = "Median Net Revenue (USD)", title = "Tags with Highest Median Net Revenue") 
#Finding max value tags
maxValInds <- which(TagtoMedian$median_net > 1.5e8)
#Highest grossing tags
genome_tags[maxValInds, ]
#Add Oscars to MovieID
MovieID$Oscar <- ifelse(MovieID$name %in% oscarsDF$name, 1, 0)
#Oscars Tag analysis
TagToOscars <- MovieID %>%
  group_by(tagId) %>% 
  summarise(oscarChance = mean(Oscar))
relevantOscar <- TagToOscars[which(sum(MovieID)),]
maxValIndOscar <- which(relevantOscar$oscarChance > .8)
genome_tags[maxValIndOscar, ]
ggplot(relevantOscar, aes(tagId, oscarChance)) +
  geom_point(shape = 16, size = 1, show.legend = FALSE, color = "blue") +
  theme_minimal() +
  labs(x= "Tag ID", y = "Chance of Winning an Oscar", title = "Tags with Highest Chance of Winning Oscars") 

write.csv(MovieID, "C:\\Users\\smuke\\OneDrive\\Desktop\\Datathon2020\\MoviesWithGenomeTags.csv")
write.csv(TagtoMedian, "C:\\Users\\smuke\\OneDrive\\Desktop\\Datathon2020\\TagToRevenue.csv")
write.csv(TagToOscars, "C:\\Users\\smuke\\OneDrive\\Desktop\\Datathon2020\\TagToOscar.csv")
