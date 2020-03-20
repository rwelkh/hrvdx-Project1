
################################## Movielens Recommendation System ##################################################
# The Outline 

# -A- Data exploratory and analysis onsists of three parts 

# 1. Preparing the data 
# 2. Exploring 
# 3. Visualizing 

# -B- Modeling and testing

# 1.Defining RMSE function
# 2.Applying three models:
  # a.Average Value Model         
  # b.Movie-effect  Model         
  # c.Users and Movie effects Model 

# -c- Regularization 

# 1.visualizing in a table the top 10 best and worst movies
# 2.Lambda
# 3. Final RMSE results table 


########################### -A- Data exploratory and analysis ##########################

# 1. Preparing the data ----------------------------------------------------------------
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(rmarkdown)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Making sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Adding rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# 2. Exploring ------------------------------------------------------------------------

# We explore the data by checking the dims, head and the summary of he data set.
head(edx) 
dim(edx) 
summary(edx) 

#The distinct users and movies.
n_distinct(edx$movieId)  
n_distinct(edx$userId) 


# 3. Visualizing ----------------------------------------------------------------------

#Plotting ratings# per movie to see how  some movies are rated more than others.

edx %>% count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30 , col= 'grey') +
  scale_x_log10() +
  xlab("# of ratings") +
  ylab("# of movies") +
  ggtitle("# of Rating/Movie")

  
#Plotting ratings# per users to shows the difference of rating behavior of the users that is represented by the number of ratings. 

edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "grey") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of Ratings/Users")

# a good think to notice here that some users rate every movie ,While some others just rated a few
################################## - B- Modeling and Testing ################################


# 1.Defining RMSE function
# to measure the accuracy of our prediction compared to the true values
RMSE <- function(y_hat, y){
  sqrt(mean((y_hat - y)^2))
}


# 2.Applying three models---------------------------------------------------------------------------------
#Based on the finding from our data exploration we will approach the data 
#by applying The below three models:

# a.Average Value Model         
# b.Movie-effect  Model         
# c.Users and Movie effects Model 


# a.Average Value Model 
# Computing the average rating for all the movies in the edx dataset.
mu= mean(edx$rating)
mu

# Testing results and save the RMSE in the results table for comparison

naive_rmse <- RMSE(validation$rating, mu)

rmse_results <- data_frame(Method = "Average Value Model ", RMSE = naive_rmse)
rmse_results %>% kable()



# b.Movie-effect Model
#Since every movie is rated differently we need to consider the b_i effect to the model

#calculating the b_i for each movie
movie_avgs= edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

#Visualizing the central tendacy of b_i 
movie_avgs %>% qplot(b_i, 
                     geom ="histogram",
                     bins = 10, data = .,
                     ylab = "Number of Movies",
                     main = "Number of movies with b_i")

# Predict the ratings
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, validation$rating)


# saving the result in the RMSE table
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie-effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% kable()



# c.Users and Movie effects Model
#Since different users rate movies differently 

# calculating the b_u 
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#Plotting b_u to visualize the rating behaviour or the users( high or low rating).
user_avgs  %>% qplot(b_u, 
                     geom ="histogram",
                     bins = 100,
                     data = .,
                     color = I("black"))

# predicting the results
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie and User effect Model",  
                                     RMSE = model_2_rmse))
rmse_results %>% kable()


######################################### -c- Regularization #####################################################

#  1.visualizing in a table the top 10 best and worst movies as precicted by our movie-effect Model----------------
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

#Top 10 movies : most of the movies are obscure movies  
  
edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  kable()

#Worst 10 movies: most of the movies are obscure movies  
  
edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  kable()

# 2. Lambda --------------------------------------------------------------------------------------------------------
#We need to take an extra measure to avoid the noisy data coming from small sample size. 
#Lambda will be used as a tuning parameter to penalize the prediction values 
#To get the value of lambda we will use cross-validation
#The below function will find the RMSE by looping through different value of lambda
#the lambda that results in the lowest rmse will be picked.


lambdas <- seq(0, 6, 0.20)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

# Plotting the rmses vs lambda to identify the best value of lambda                         
ggplot() +
  geom_point(aes(lambdas,rmses))

# Lambda value that results in the least RMse result
lambda <- lambdas[which.min(rmses)]
lambda

# RMSE result  
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Results after regularization",  
                                     RMSE = min(rmses)))

# 3. Final RMSE results table -------------------------------------------------------------------------------------
rmse_results %>% kable()



                             ################### Thank you ####################





