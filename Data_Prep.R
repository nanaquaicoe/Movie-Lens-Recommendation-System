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
train_set <- movielens[-test_ind,].  #creating the train set###
test_set <- movielens[test_ind,].    ###creating the test set###
