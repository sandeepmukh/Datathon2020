# load libraries
library(xgboost)
library(gbm)
library(leaps)
library(devtools)


# read in data
movies = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/movies.csv")
ratings = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/ratings.csv")
genome_tags = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/genome-tags.csv")
tags = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/tags.csv")
genome_scores = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/genome-scores.csv")
links = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/links.csv")
#the_oscar_award = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/the_oscar_award.csv")
movie_industry = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_industry.csv")


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
ggbiplot(tags_pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE,   labels=genome_tags$tag)

  # with 13 PCs, you can explain 50% of the variance
summary(tags_pca)$importance[3,1:13]
   # with 344 PCs, you can explain 90% of the variance
summary(tags_pca)$importance[3,1:344]

movie_tags[,paste('PCA',1:13)] = tags_pca$x[,1:13]

# merge "movies3" with the newly created principal components
movies5 = merge(movies4,movie_tags[,c('movieId',paste('PCA',1:13))])


df = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/data_clean.csv")


df[,c('Adventure', 'Animation', 'Children', 'Comedy', 'Fantasy', 'Romance', 'Drama', 'Action', 'Crime', 
      'Thriller', 'Horror', 'Mystery', 'Sci.Fi', 'IMAX','War', 'Musical',  'Western' ,'Film.Noir')] = 
  df[,c('Adventure', 'Animation', 'Children', 'Comedy', 'Fantasy', 'Romance', 'Drama', 'Action', 'Crime', 
         'Thriller', 'Horror', 'Mystery', 'Sci.Fi', 'IMAX','War', 'Musical',  'Western' ,'Film.Noir')]*1

df$net = df$gross - df$budget

#### ANALYSIS BELOW
# use one of the following y's below
y = 'Mean.Rating'
y = 'Var.Rating'
y = 'net'



idx = sample(1:nrow(df), round(nrow(df)*3/4,0))
train = df[idx,c(y,'Adventure', 'Animation', 'Children', 'Comedy', 'Fantasy', 'Romance', 'Drama', 'Action', 'Crime', 
                       'Thriller', 'Horror', 'Mystery', 'Sci.Fi', 'IMAX','War', 'Musical',  'Western' ,'Film.Noir' ,'PCA.1' , 
                       'PCA.2' , 'PCA.3', 'PCA.4', 'PCA.5', 'PCA.6', 'PCA.7', 'PCA.8', 'PCA.9', 'PCA.10', 'PCA.11', 'PCA.12', 'PCA.13', 'Mean.Count','budget','runtime','year.x','rating')]
test = df[-idx,c(y,'Adventure', 'Animation', 'Children', 'Comedy', 'Fantasy', 'Romance', 'Drama', 'Action', 'Crime', 
                       'Thriller', 'Horror', 'Mystery', 'Sci.Fi', 'IMAX','War', 'Musical',  'Western' ,'Film.Noir' ,'PCA.1' , 
                       'PCA.2' , 'PCA.3', 'PCA.4', 'PCA.5', 'PCA.6', 'PCA.7', 'PCA.8', 'PCA.9', 'PCA.10', 'PCA.11', 'PCA.12', 'PCA.13', 'Mean.Count','budget','runtime','year.x','rating')]

train = df[idx,c(y,'Adventure', 'Comedy', 'Drama', 'Action', 
                   'IMAX','budget','runtime','year.x','rating')]
test = df[-idx,c(y,'Adventure', 'Comedy', 'Drama', 'Action', 
                 'IMAX','budget','runtime','year.x','rating')]


# best subset selection
fit_all = regsubsets(sqrt(`Var.Rating`) ~ ., train,nvmax = 30)
fit_all_sum = summary(fit_all)

# plot results of best subset selection
# assess models based on RSS, Cp, Adjusted RSq, and BIC
par(mfrow = c(1, 1))

plot(fit_all_sum$bic, xlab = "Number of Variables", ylab = "BIC", type = 'b', main = "Model Selection using BIC")
best_bic = which.min(fit_all_sum$bic)
points(best_bic, fit_all_sum$bic[best_bic], 
       col = "red", cex = 2, pch = 20)


# select the "best" model based on BIC
best_model = which.min(fit_all_sum$bic)
predict.regsubsets = function(object, newdata, id, ...) {
  
  form  = as.formula(object$call[[2]])
  mat   = model.matrix(form, newdata)
  coefs = coef(object, id = id)
  xvars = names(coefs)
  
  mat[, xvars] %*% coefs
}
y_pred = predict(fit_all,test,id= best_model)
y_actual = sqrt(test$`Var.Rating`)

# calculate the MAE and RMSE
MAE = mean(abs(y_pred-y_actual))
RMSE = sqrt(mean((y_actual - y_pred) ^ 2))

# plot results
par(mfrow = c(1,1))
plot(y_pred,y_actual,
     xlab = "Predicted Standard Deviation of Movie Rating", ylab = "Actual", 
     main = "Test Set Prediction of Movielens Rating Std. Dev.",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)
text(x = 0.85, y = 1.3, paste('R-sq = ',round(cor(y_actual,y_pred),3)),cex = 2)


#  view coefficients
View(coef(fit_all,20))


# Boosting method 2
boost = gbm(net ~ ., data = train, distribution = "gaussian", 
                    n.trees = 500, interaction.depth = 4, shrinkage = 0.01)
View(tibble::as_tibble(summary(boost)))
Y_test  = test$net
boost_tst_pred = predict(boost, newdata = test, n.trees = 5000)
calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
tst_mae = mean(abs(boost_tst_pred - Y_test))
tst_rmse = calc_rmse(boost_tst_pred, Y_test)
plot(boost_tst_pred,test$net,
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted Net Sales",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)
text(x = 3e7, y=3e8, paste('R-sq = ',round(cor(boost_tst_pred,Y_test),3)),cex = 2)






