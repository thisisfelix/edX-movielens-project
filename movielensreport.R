#### Data Setup
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# high level exploring the given data
explanation_table <- tibble(column = colnames(edx), 
                            description = c("Unique identifier of the user giving the rating", 
                                            "Unique identifier of the movie", 
                                            "Rating given by the user on a scale from 0.5 to 5", 
                                            "Timestamp of the rating, given in miliseconds since January 1, 1970", 
                                            "Title of the movie (release year in brackets)", 
                                            "Combination of genres the movie is categorized into"))
knitr::kable(explanation_table, caption = "Columns in the MovieLens data")

# check if lubridate is installed, install if necessary & load library
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)
# add date column
edx <- edx %>% mutate(date = as_datetime(timestamp))

#### Data Exploration & Visualization

# number of entries in the edx set
num_entries <- edx %>% nrow()
# number of individual movies in the edx set
num_movies <- unique(edx$movieId) %>% length()
# number of unique users in the edx set
num_users <- unique(edx$userId) %>% length()

# jitter plot for avg rating of movies by number of ratings
edx %>% group_by(movieId) %>% 
  summarize(n = n(), avg_rating = mean(rating)) %>% 
  ggplot(aes(n, avg_rating)) + 
  geom_jitter() + 
  geom_abline(aes(intercept = mean(edx$rating), slope = 0), col = "red") +
  ggtitle("Fig. 1: Average Movie Ratings") +
  xlab("Number of ratings per movie") + 
  ylab("Average rating per movie")

# jitter plot for avg rating per user by number of ratings
edx %>% group_by(userId) %>% 
  summarize(n = n(), avg_rating = mean(rating)) %>% 
  ggplot(aes(n, avg_rating)) + 
  geom_jitter() +
  geom_hline(aes(yintercept = mean(edx$rating)), col = "red") + 
  ggtitle("Fig. 2: Average User Ratings") + 
  xlab("Number of ratings per user") +
  ylab("Average rating per user")

# add a new column for number of genres
edx <- edx %>% mutate(num_genres = str_count(genres, "\\|")+1)
# list of genres
genrelist <- edx %>% filter(num_genres == 1) %>% distinct(genres, .keep_all = FALSE)
knitr::kable(genrelist, caption = "Genres in the MovieLens data")

# plot number of movies against number of genres per movie
edx %>% group_by(movieId) %>% 
  summarize(num_genres = mean(num_genres)) %>% 
  ggplot(aes(num_genres)) + 
  geom_histogram(binwidth = 1) +
  ggtitle("Fig. 3: Genres per movie") +
  xlab("Number of genres of a movie") + 
  ylab("Amount of movies")
# plot avg rating per category "number of genres"
edx %>% group_by(num_genres) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  ggplot(aes(num_genres,avg_rating)) + 
  geom_point() +
  geom_smooth() +
  ggtitle("Fig. 4: Average ratings per numbers of genres") +
  xlab("Numbers of genres in a movie") +
  ylab("Average rating of the movies")

# number of genre combinations in the sample
genre_combs <- edx %>% group_by(genres) %>% summarize(n = n()) %>% nrow()

# top 20 genre combinations average rating
edx %>% group_by(genres) %>% 
  summarize(n = n(), avg_rating = mean(rating), se_rating = sd(rating)) %>% 
  slice_max(order_by = n, n = 20) %>% 
  ggplot(aes(x = genres, y = avg_rating, ymin = avg_rating - 2*se_rating, ymax = avg_rating + 2*se_rating)) + 
  geom_point() + 
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Fig. 5: Average rating per genre combination") +
  xlab("Genre combinations") +
  ylab("Average rating")

# Create column for release year
edx <- edx %>% mutate(RelYear = as.numeric(str_sub(title, -5, -2)))
# plot avg rating vs release year
edx %>% group_by(RelYear) %>% 
  summarize(avg_rating = mean(rating), n = n()) %>% 
  ggplot(aes(RelYear, avg_rating)) + 
  geom_point(aes(size = sqrt(n))) +
  geom_smooth() +
  geom_hline(aes(yintercept = mean(edx$rating)), col = "red") +
  ggtitle("Fig. 6: Average movie rating by release year") +
  xlab("Movie Release Year") +
  ylab("Average rating")

# mutate edx data to have a dedicated review year column
edx <- edx %>% mutate(RevYear = year(date))
# average ratings over the years
edx %>% group_by(RevYear) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  ggplot(aes(RevYear, avg_rating)) + 
  geom_point() + 
  geom_smooth() +
  ylim(3.4, 4) +
  ggtitle("Fig. 7: Average ratings by review year") +
  xlab("Review Year") +
  ylab("Average rating")
# average ratings per month
edx %>% mutate(date = month(date)) %>% 
  group_by(date) %>% 
  summarize(n = n(), avg_rating = mean(rating)) %>% 
  ggplot(aes(date, avg_rating)) + 
  geom_point() + 
  geom_smooth() +
  ylim(3.4, 4) +
  ggtitle("Fig. 8: Average ratings by review month") +
  xlab("Review Month") +
  ylab("Average rating")

# reformat data to calculate time difference between release year and review year
edx <- edx %>% mutate(RelYear = as.numeric(RelYear), timediff = RevYear - RelYear)
# check data for consistency (is there a case of RevYear < RelYear?)
time_error <- sum(edx$timediff < 0)
# manually set those to timediff = 0
edx <- edx %>% mutate(timediff = ifelse(timediff < 0, 0, timediff))

# plot graph
edx %>% group_by(timediff) %>% 
  summarize(avg_rating = mean(rating), n = n()) %>% 
  ggplot(aes(timediff, avg_rating)) + 
  geom_point(aes(size = sqrt(n))) + 
  geom_smooth() +
  ggtitle("Fig. 9: Average rating based on time between release and review") +
  xlab("Time difference between release and review (in years)") +
  ylab("Average rating")

#### Model development

# Set seed to have consistent results
set.seed(1910, sample.kind = "Rounding") # if using R 3.5 or earlier, use set.seed(1910)
# Create train (80%) and test (20%) sets
test_set_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_set_index,]
test_set <- edx[test_set_index,]

# clean up test set to not include users or movies that are not in the train set
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# define target RMSE
target_rmse <- 0.86490

## Basic Model
# Calculate average rating in train set
mu_hat <- mean(train_set$rating)
# Calculate RMSE in test set
basic_rmse <- RMSE(test_set$rating, mu_hat)
# add result to table
rmse_results <- tibble(model = c("Target RMSE", "Basic average"), 
                       RMSE = c(target_rmse, basic_rmse), 
                       difference = c(0, basic_rmse - target_rmse))

## Movie Factor Model
# create movie specific factor
movie_avgs <- train_set %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu_hat))
# add b_i to predictions
predicted_ratings <- mu_hat + test_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  pull(b_i)
# calculate RMSE
b_i_rmse <- RMSE(predicted_ratings, test_set$rating)
# add new model RMSE to summary table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Movie Effect", RMSE = b_i_rmse, 
                                 difference = b_i_rmse - target_rmse))

## User Factor Model
# create user specific factor
user_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))
# add b_u to predictions
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu_hat + b_u + b_i) %>%
  pull(pred)
# calculate RMSE
add_b_u_rmse <- RMSE(predicted_ratings, test_set$rating)
# add new model RMSE to results table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Movie + User Effect", RMSE = add_b_u_rmse, 
                                 difference = add_b_u_rmse - target_rmse))

## Genre Factor Model
# create genre specific factor
genre_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))
# add b_g to predictions
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  summarize(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)
# calculate RMSE
add_b_g_rmse <- RMSE(predicted_ratings, test_set$rating)
# add RMSE to table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Movie + User + Genre Effect", 
                                 RMSE = add_b_g_rmse, 
                                 difference = add_b_g_rmse - target_rmse))

## Release Year Factor Model
# create release year specific factor
release_avgs <- train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  group_by(RelYear) %>%
  summarize(b_rel = mean(rating - mu_hat - b_i - b_u - b_g))
# calculate four factor prediction values
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(release_avgs, by = "RelYear") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_rel) %>%
  pull(pred)
# calculate four factor RMSE
add_b_rel_rmse <- RMSE(predicted_ratings, test_set$rating)
# add new model RMSE to table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Movie + User + Genre + Release Year Effect", 
                                 RMSE = add_b_rel_rmse, 
                                 difference = add_b_rel_rmse - target_rmse))

## Time Difference (Review Time) Factor Model
# create time difference specific factor
timediff_avgs <- train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(release_avgs, by = "RelYear") %>%
  group_by(timediff) %>%
  summarize(b_t = mean(rating - mu_hat - b_i - b_u - b_g - b_rel))
# calculate four factor prediction values
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(release_avgs, by = "RelYear") %>%
  left_join(timediff_avgs, by = "timediff") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_rel + b_t) %>%
  pull(pred)
# calculate four factor RMSE
add_b_t_rmse <- RMSE(predicted_ratings, test_set$rating)
# add new model RMSE to table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Movie + User + Genre + Release Year + 
                                 Time Difference Effect", 
                                 RMSE = add_b_t_rmse, 
                                 difference = add_b_t_rmse - target_rmse))

## Regularization
# display table
knitr::kable(rmse_results, caption = "Factor Models RMSE")

## Basic Regularized Model
lambdas <- seq(0, 10, 0.25)
# create function that calculates RMSE for different lambdas
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i) %>% 
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
# find lambda with the lowest RMSE
lambda <- lambdas[which.min(rmses)]
# add to table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Regularized Movie Effect", 
                                 RMSE = min(rmses), 
                                 difference = min(rmses) - target_rmse))

## Regularized Models for the other existing models 
# create function that calculates RMSE for different lambdas - 2 factor model
rmses_2f <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>% 
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
# find lambda with the lowest RMSE
lambda_2f <- lambdas[which.min(rmses_2f)]
# add to table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Regularized Movie + User Effect", 
                                 RMSE = min(rmses_2f), 
                                 difference = min(rmses_2f) - target_rmse))

# create function that calculates RMSE for different lambdas - 3 factor model
rmses_3f <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>% 
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
# find lambda with the lowest RMSE
lambda_3f <- lambdas[which.min(rmses_3f)]
# add to table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Regularized Movie + User + Genre Effect", 
                                 RMSE = min(rmses_3f), 
                                 difference = min(rmses_3f) - target_rmse))

# create function that calculates RMSE for different lambdas - 4 factor model
rmses_4f <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n() + l))
  
  b_rel <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    group_by(RelYear) %>%
    summarize(b_rel = sum(rating - b_u - b_i - b_g - mu)/(n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_rel, by = "RelYear") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_rel) %>% 
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
# find lambda with the lowest RMSE
lambda_4f <- lambdas[which.min(rmses_4f)]
# add to table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Regularized Movie + User + Genre + 
                                 Release Year Effect", 
                                 RMSE = min(rmses_4f), 
                                 difference = min(rmses_4f) - target_rmse))


# create function that calculates RMSE for different lambdas - final model
rmses_final <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n() + l))
  
  b_rel <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    group_by(RelYear) %>%
    summarize(b_rel = sum(rating - b_u - b_i - b_g - mu)/(n() + l))
  
  b_t <- train_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_rel, by = "RelYear") %>%
    group_by(timediff) %>%
    summarize(b_t = sum(rating - b_u - b_i - b_g - b_rel - mu)/(n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_rel, by = "RelYear") %>%
    left_join(b_t, by = "timediff") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_rel + b_t) %>% 
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
# find lambda with the lowest RMSE
lambda_final <- lambdas[which.min(rmses_final)]
# add to table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Regularized Movie + User + Genre + 
                                 Release Year + Time Difference Effect", 
                                 RMSE = min(rmses_final), 
                                 difference = min(rmses_final) - target_rmse))

knitr::kable(rmse_results, caption = "Factor and Regularized Models RMSE")

#### Final Hold Out Test

# set up validation set to feature all new columns from part 3
validation <- validation %>% 
  mutate(RevYear = year(as_datetime(timestamp)), 
         RelYear = as.numeric(str_sub(title, -5, -2)), 
         timediff = ifelse(RevYear-RelYear <0, 0, RevYear - RelYear))

# calculate average rating across edx data set
mu_final <- mean(edx$rating)

# calculate movie factor
b_i <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu_final)/(n() + lambda_final))
# calculate user factor
b_u <- edx %>% 
  left_join(b_i, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = sum(rating - mu_final - b_i)/(n() + lambda_final))
# calculate genre factor
b_g <- edx %>% 
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% 
  group_by(genres) %>% 
  summarize(b_g = sum(rating - mu_final - b_i - b_u)/(n() + lambda_final))
# calculate release year factor
b_rel <- edx %>% 
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% 
  left_join(b_g, by = "genres") %>% 
  group_by(RelYear) %>% 
  summarize(b_rel = sum(rating - mu_final - b_i - b_u - b_g)/(n() + lambda_final))
# calculate time difference factor
b_t <- edx %>% 
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% 
  left_join(b_g, by = "genres") %>% 
  left_join(b_rel, by = "RelYear") %>% 
  group_by(timediff) %>% 
  summarize(b_t = sum(rating - mu_final - b_i - b_u - b_g - b_rel)/(n() + lambda_final))
# calculate predictions for final hold out test
predicted_ratings <- validation %>% 
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% 
  left_join(b_g, by = "genres") %>% 
  left_join(b_rel, by = "RelYear") %>% 
  left_join(b_t, by = "timediff") %>% 
  mutate(pred = mu_final + b_i + b_u + b_g + b_rel + b_t) %>% 
  pull(pred)

# final test
hold_out_rmse <- RMSE(predicted_ratings, validation$rating)
# add to table
rmse_results <- bind_rows(rmse_results, 
                          tibble(model = "Final Hold Out Test", 
                                 RMSE = hold_out_rmse, 
                                 difference = hold_out_rmse - target_rmse))

# final visualization in table
final_table <- tibble(model = c("Target RMSE", "Final Hold Out Test"), 
                      RMSE = c(target_rmse, hold_out_rmse), 
                      difference = c("", hold_out_rmse - target_rmse))
knitr::kable(final_table, caption = "Final Hold Out RMSE")
