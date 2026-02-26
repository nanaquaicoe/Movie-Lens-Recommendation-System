#Data Prep#
#By Dr. Nana Quaicoe adapted from Harvardx Data Science Certificate#

####Loading the MovieLens Dataset####
library(tidyverse)
library(dslabs)
data("movielens")

####Examine the Data####
movielens|>as_tibble()


####Examine the Number of Unique Users Rating a Movie####
movielens|>summarize(n_users=n_distinct(userId), n_movies=n_distinct(movieId))

####Consider only movies rated 5 times or more and users that have rated more than 100 of these movies####
set.seed(2006)
indexes<-split(1:nrow(movielens), movielens$userId)###split data according to userid###
test_ind <- sapply(indexes, function(ind) sample(ind, ceiling(length(ind)*.2))) |>
  unlist(use.names = TRUE) |> sort() ###for each user it selects 20% of their ratings###
train_set <- movielens[-test_ind,]  #creating the train set###
test_set <- movielens[test_ind,]  ###creating the test set###


###remove entries using the semi_join function##
test_set<- test_set|>semi_join(train_set, by="movieId")
train_set<-train_set|>semi_join(test_set, by="movieId")

#####Use pivot_wider to make a matric with users represented by rows and movies by columns####
y <- select(train_set, movieId, userId, rating) |>
  pivot_wider(names_from = movieId, values_from = rating) 
rnames <- y$userId
y <- as.matrix(y[,-1])
rownames(y) <- rnames
movie_map <- train_set |> select(movieId, title) |> distinct(movieId, .keep_all = TRUE)