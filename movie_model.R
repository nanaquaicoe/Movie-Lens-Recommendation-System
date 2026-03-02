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


###remove entries using the semi_join function in order to exclude movies that are not 
##both test and trains ets
test_set<- test_set|>semi_join(train_set, by="movieId")
train_set<-train_set|>semi_join(test_set, by="movieId")

#####Use pivot_wider to make a matrix with users represented by rows and movies by columns####
y <- select(train_set, movieId, userId, rating) |>
  pivot_wider(names_from = movieId, values_from = rating) 
rnames <- y$userId
y <- as.matrix(y[,-1])
rownames(y) <- rnames
###a table to map movie ids to titles##
movie_map <- train_set |> select(movieId, title) |> distinct(movieId, .keep_all = TRUE)

####A FIRST MODEL###
### Start by building the simplest recommendation system:
##we predict the same rating for all movies regardless of user
###assume a model that gives the same rating for all movies and users explained by random variation
### 1. Find the average of all ratings:
mu<-mean(y, na.rm=TRUE)
naive_rmse<-RMSE(test_set$rating, mu) ## finding the RMSE (actual, predicted)
###RMSE tells us how far off our predictions are
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse) ### creating a results table

#2. Modeling Movie Effects##
b_i<-colMeans(y-mu, na.rm=TRUE) ### modeling movie effects/bias

fit_movies <- data.frame(movieId = as.integer(colnames(y)), 
                         mu = mu, b_i = b_i)
left_join(test_set, fit_movies, by = "movieId") |> 
  mutate(pred = mu + b_i) |> 
  summarize(rmse = RMSE(rating, pred))  ####using movie effects we see our model improve###

#3. Modeling User Effects##
b_u<-rowMeans(y, na.rm=TRUE) ## computing the average rating for user u

b_u <- rowMeans(sweep(y - mu, 2, b_i), na.rm = TRUE)

fit_users <- data.frame(userId = as.integer(rownames(y)), b_u = b_u)

left_join(test_set, fit_movies, by = "movieId") |> 
  left_join(fit_users, by = "userId") |> 
  mutate(pred = mu + b_i + b_u) |> 
  summarize(rmse = RMSE(rating, pred)) #### predicting with user effects###


