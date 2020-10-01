# load libraries
library(xgboost)
library(gbm)

# read in data
movies = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/movies.csv")
ratings = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/ratings.csv")
genome_tags = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/genome-tags.csv")
#tags = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/tags.csv")
genome_scores = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/genome-scores.csv")
#links = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/links.csv")
#the_oscar_award = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/the_oscar_award.csv")
#movie_industry = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_industry.csv")

# add binary columns for category
movies$genres = as.character(movies$genres)
cats = c()
for (i in 1:nrow(movies)){
  split = unlist(strsplit(movies$genres[i],'[|]'))
  cats = unique(c(cats,split))
}
cats = cats[-length(cats)]
for (i in 1:length(cats)){
  movies[,cats[i]] = grepl(cats[i],movies$genres, fixed = T)
}

# create a year column in "ratings"
sec_in_year = 3600*24*365.25
ratings$year = round(ratings$timestamp/sec_in_year,0) + 1970

# create a column in "ratings" that has the count of ratings done by each user
user_count = aggregate(ratings$userId, list(ID = ratings$userId), length)
user_mean = aggregate(ratings$rating, list(ID = ratings$userId), mean)
user_var = aggregate(ratings$rating, list(ID = ratings$userId), var)
user_var[is.na(user_var[,'x']),'x'] = 0 
fxn_count <- function(y){
  user_count$x[y]
}
fxn_mean <- function(y){
  user_mean$x[y]
}
fxn_var <- function(y){
  user_var$x[y]
}
ratings$count = as.numeric(lapply(ratings$userId, fxn_count))
ratings$mean = as.numeric(lapply(ratings$userId, fxn_mean))
ratings$var = as.numeric(lapply(ratings$userId, fxn_var))

# aggregate movie ratings by movieId
mean_movie_rating = aggregate(ratings$rating, list(movieId = ratings$movieId), mean)
var_movie_rating = aggregate(ratings$rating, list(movieId = ratings$movieId), var)

# aggregate number of ratings per rater by movieId
mean_count_movie  = aggregate(ratings$count, list(movieId = ratings$movieId), mean)


# merge "movies" with the mean and variance of each rating
movies2 = merge(movies,mean_movie_rating, by = 'movieId')
colnames(movies2)[ncol(movies2)] = 'Mean Rating'
movies3 = merge(movies2,var_movie_rating, by = 'movieId')
colnames(movies3)[ncol(movies3)] = 'Var Rating'
movies4 = merge(movies3,mean_count_movie, by = 'movieId')
colnames(movies4)[ncol(movies4)] = 'Mean Count'

# add tag relevances to movies
movie_tags = data.frame(movieId = unique(genome_scores$movieId))
movie_tags[,paste('tag',1:nrow(genome_tags))] = NA
movie_tags[,2:1129] = matrix(genome_scores$relevance,nrow = 13176, ncol = 1128,byrow = T)
tags_pca <- prcomp(movie_tags[,2:1129])
  # with 13 PCs, you can explain 50% of the variance
summary(tags_pca)$importance[3,1:13]
   # with 344 PCs, you can explain 90% of the variance
summary(tags_pca)$importance[3,1:344]

movie_tags[,paste('PCA',1:13)] = tags_pca$x[,1:13]

# merge "movies3" with the newly created principal components
movies5 = merge(movies4,movie_tags[,c('movieId',paste('PCA',1:13))])

# run regression
out = lm(formula = `Mean Rating` ~ Adventure + Animation + Children + Comedy + Fantasy + Romance + Drama + Action + Crime + 
           Thriller + Horror + Mystery + `Sci-Fi`  + IMAX + Documentary + War + Musical +  Western + `Film-Noir` + `PCA 1` + 
           `PCA 2` + `PCA 3`+ `PCA 4`+ `PCA 5`+ `PCA 6`+ `PCA 7`+ `PCA 8`+ `PCA 9`+ `PCA 10`+ `PCA 11`+ `PCA 12`+ `PCA 13`+ `Mean Count`, data = movies5)
summary(out)

plot(movies5$`Mean Rating`,out$fitted.values)


# XGBoost predictor
# train with 75% of data, test on remaining 25%
idx = sample(1:nrow(movies5), round(nrow(movies5)*3/4,0))
train = movies5[idx,c('Mean Rating','Adventure', 'Animation', 'Children', 'Comedy', 'Fantasy', 'Romance', 'Drama', 'Action', 'Crime', 
                                'Thriller', 'Horror', 'Mystery', 'Sci-Fi', 'IMAX','Documentary','War', 'Musical',  'Western' ,'Film-Noir' ,'PCA 1' , 
                                'PCA 2' , 'PCA 3', 'PCA 4', 'PCA 5', 'PCA 6', 'PCA 7', 'PCA 8', 'PCA 9', 'PCA 10', 'PCA 11', 'PCA 12', 'PCA 13', 'Mean Count')]
test = movies5[-idx,c('Mean Rating','Adventure', 'Animation', 'Children', 'Comedy', 'Fantasy', 'Romance', 'Drama', 'Action', 'Crime', 
                            'Thriller', 'Horror', 'Mystery', 'Sci-Fi', 'IMAX','Documentary','War', 'Musical',  'Western' ,'Film-Noir' ,'PCA 1' , 
                            'PCA 2' , 'PCA 3', 'PCA 4', 'PCA 5', 'PCA 6', 'PCA 7', 'PCA 8', 'PCA 9', 'PCA 10', 'PCA 11', 'PCA 12', 'PCA 13', 'Mean Count')]
X_train = as.matrix(train[,-1])
X_test = as.matrix(test[,-1])
Y_train = as.matrix(train[,1])
Y_test = as.matrix(test[,1])

params <- list(booster = "gbtree", objective = "reg:linear", eta = 0.3, gamma = 0,
               max_depth = 6)

xgb_train <- xgb.DMatrix(data = X_train, label = Y_train)
xgbcv <- xgb.cv(params = params, data = xgb_train, nfold = 10, nrounds = 10,verbose=F)
cv_nrounds = which.min(xgbcv$evaluation_log$test_rmse_mean)
xgb_optb <- xgboost(params = params, data = xgb_train, nround = cv_nrounds,verbose=F)


# group test data in a dense matrix and use the trained model to make out-of-sample predictions
xgb_test <- xgb.DMatrix(data = X_test, label = Y_test)
xgb_pred_out_of_sample <- predict(xgb_optb, X_test)


plot(Y_test, xgb_pred_out_of_sample)
cor(Y_test, xgb_pred_out_of_sample)

# Boosting method 2
train = as.data.frame(as.matrix(train)*1)
boost = gbm(`Mean Rating` ~ ., data = train, distribution = "gaussian", 
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
tibble::as_tibble(summary(boost))
boost_tst_pred = predict(boost, newdata = test, n.trees = 5000)
calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
tst_rmse = calc_rmse(boost_tst_pred, test$`Mean Rating`)
plot(boost_tst_pred,test$`Mean Rating`,
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Boosted Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)
text(x = 2.5, y = 4, paste('R-sq = ',round(cor(boost_tst_pred,Y_test),3)),cex = 2.5)


