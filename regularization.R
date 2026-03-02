###examining movies that had a top score####
> n <-  colSums(!is.na(y))
> fit_movies$n <- n
> best <- fit_movies |> left_join(movie_map, by = "movieId") |> 
  +     mutate(average_rating = mu + b_i) |>
  +     filter(average_rating == 5 & n>1) 
> test_set |> 
  +     group_by(movieId) |>
  +     summarize(test_set_averge_rating = mean(rating)) |>
  +     right_join(best, by = "movieId") |>
  +     select(title, average_rating, n, test_set_averge_rating) 


####to select lambda which is the penalty to penalize using small samples###

lambdas <- seq(0, 10, 0.1)

sums <- colSums(y - mu, na.rm = TRUE)
rmses <- sapply(lambdas, function(lambda){
  b_i <-  sums / (n + lambda)
  fit_movies$b_i <- b_i
  left_join(test_set, fit_movies, by = "movieId") |> mutate(pred = mu + b_i) |> 
    summarize(rmse = RMSE(rating, pred)) |>
    pull(rmse)
})

###select the values that minimizes the RMSE##

qplot(lambdas, rmses, geom = "line")
lambda <- lambdas[which.min(rmses)]
print(lambda)


#once we have our lambda, then we can add it to our estimates##
fit_movies$b_i_reg <- colSums(y - mu, na.rm = TRUE) / (n + lambda)

###estimate the user effects with the new movie effects and find the RMSE##
fit_users$b_u <- rowMeans(sweep(y - mu, 2, b_i), na.rm = TRUE)
left_join(test_set, fit_movies, by = "movieId") |> 
  left_join(fit_users, by = "userId") |> 
  mutate(pred = mu + b_i_reg + b_u) |> 
  summarize(rmse = RMSE(rating, pred))


