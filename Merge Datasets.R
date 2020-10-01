library(DescTools)

movies = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_lense/movies.csv")
movie_industry = read.csv("/Users/blakesteines/Desktop/Fall\ 2020\ Datathon/movie_industry.csv")

movie_industry$name = as.character(movie_industry$name)
colnames(movies)[2] = 'name'

movies_merged = merge(movies,movie_industry,by='name')

head(movies_merged)

movies_merged$dif = abs(as.numeric(as.character(movies_merged$year.x))-as.numeric(movies_merged$year.y))>2
movies_merged = movies_merged[movies_merged$dif == 0,]
movies_merged = movies_merged[movies_merged$budget != 0,]

