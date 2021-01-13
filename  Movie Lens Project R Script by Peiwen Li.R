#########################################
# HarvardX Data Science MovieLens Project 
# Student: Peiwen Li 
#########################################

#########################################
#Part 1 Dataset Download & Wrangling 
#########################################

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")
str(movielens)
# Validation set will be 10% of MovieLens data
set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

################################
# Data Wrangling 
################################


# Find out what the dataset looks like and check whether there's missing value in the dataset. 
str(edx)
edx[!complete.cases(edx),]
#From the result of above codes, we can learn that, there are 9000055 observations and 6 variables which are userID (integer),movieID(numerical),rating(numerical),timestamp(integer),title(character) and genres(character).
#Since the returned results is 0 row, then we can know that there's no missing value in this dataset. 

#Change the time format 
# The timestamp format is changed into year and month format so as to analyze whether there's seasonal trend or bias in years in later analysis. 
# There's a year in the title, which was the year when the movie first released to the public. Extract the year to a seperate column.
library(stringi)
library(lubridate)
release <- stringi::stri_extract_last_regex(edx$title, "(\\d{4})", comments = TRUE ) %>% as.numeric()
edxtidy <- edx %>% mutate (Year_Released = release, Year_Rated = year(as_datetime(timestamp)), Month_Rated = month(as_datetime(timestamp)))%>%select(-timestamp)
head(edxtidy)

# Unique values for each variables.

number_user <- n_distinct(edxtidy$userId)
number_movie <- n_distinct(edxtidy$movieId)
number_genre <- n_distinct(edxtidy$genres)
number_Year_Released <- n_distinct(edxtidy$Year_Released)
number_Year_Rated <- n_distinct(edxtidy$Year_Rated)
number_Month_Rated <- n_distinct(edxtidy$Month_Rated)
number_glimpse <- data.table(edx_variable = c("User","Movie","Genre","Year_Release","Year_Rated","Month_Rated"), variable_number = c(number_user,number_movie,number_genre,number_Year_Released,number_Year_Rated,number_Month_Rated))
number_glimpse


########################################
# Part 2 Exploratory Analysis of edx set
########################################

# Study the distribution of the dataset 
#summarize rating 
summary(edxtidy$rating)
edxtidy %>% group_by(rating) %>% summarize( n = n()) 

#Distribution of Movie Ratings
edxtidy %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "steelblue", color = "black", bins = 10) +
  scale_x_log10() +
  ggtitle(" Movies Distribution")

#Distribution of User Ratings
edxtidy %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "#E7B800", color = "black", bins = 10) +
  scale_x_log10() +
  ggtitle(" Users Distribution")

#Distribution of Movie Genre
edxtidy %>% group_by(genres) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "#00AFBB", color = "black", bins = 10) +
  scale_x_log10() +
  ggtitle(" Movie Genre Distribution")

# Distribution of rating year
edxtidy %>% group_by(Year_Rated) %>% summarize(n = n())  %>%
  ggplot(aes(x = Year_Rated, y=n)) + geom_point() +
  ggtitle(" Rating Year Distribution")

# Distribution of rating month
edxtidy %>% group_by(Month_Rated) %>% summarize(n = n()) %>%
  ggplot(aes(x = Month_Rated, y=n)) + geom_point() +
  ggtitle(" Rating Month Distribution")

# Distribution of release year 
edxtidy %>% group_by(Year_Released) %>% summarize(n = n())  %>%
  ggplot(aes(x = Year_Released, y=n)) + geom_line() +
  ggtitle(" Released Year Distribution")

# Explore the standard deviation of rating by Variables 
par(mfrow=c(3,1))
edxtidy %>% group_by(movieId) %>% summarize(movie_mean_sd = sd(rating))%>%as.data.frame()%>%plot()
edxtidy %>% group_by(userId) %>% summarize(user_mean_sd = sd(rating))%>%as.data.frame()%>%plot()
edxtidy %>% group_by(Year_Released) %>% summarize(year_release_mean_sd = sd(rating))%>%as.data.frame()%>%plot()

#Reorganize the movie genres
singlegenre <- edxtidy %>% separate_rows(genres, sep ="\\|")
head(singlegenre)
genreclass <- singlegenre%>%group_by(genres)%>%summarize(n = n())
genreclass

#Distribution of Rating by Genre
library(ggplot2)
avg_genreclass <- singlegenre%>%group_by(genres)%>%summarize(avg_rate_genreclass = mean(rating))
singlegenre %>%
  ggplot(aes(genres,rating)) + geom_boxplot()+ coord_flip()+
  labs(y = "Genre", x = "Viewers' Rating") +
  ggtitle("Distribution of Rating by Genre")

# Study further in the disbrition of the skewed genre.
Skewedgenres <-c("Western","War","Romance","Mystery","Musical","IMAX","Horror","Film-Noir","Drama","Documentary","Crime","Animation")
skewed_genre <- singlegenre%>%filter(genres %in% Skewedgenres ) 
skewed_genre %>%ggplot(aes(x = rating,color=genres,fill = genres))+
  geom_density(alpha=0.6) +
  ylab("") +
  xlab("Density of Rating by Genre")

########################################
# Part 3 Data Modeling 
########################################

########################################
#  Modeling 
########################################

#Split the dataset into one set for training and one set for testing. 
## Split the tidied dataset "edxtidy" into traning_set and testing_set, use 20% of the "edx" dataset for testing purposes.
set.seed(1)
test_index2 <- createDataPartition(y = edxtidy$rating, times = 1, p = 0.2, list = FALSE)
train <- edxtidy[-test_index2,]
test <- edxtidy[test_index2,]
# Semi_join, so as to make sure the movies in the training set are also in test set. 
test <- test %>% semi_join(train, by = "movieId") %>%semi_join(train, by = "userId")
nrow(test)

#Use the train set to train the model. To begin with, I will start with the linear model and assume all the movie has the same rating rated by all users in all the time, then will start with adding one bias variable at a time, and test the model using RMSE (Residual Mean Square Error)
#Step 1: Start with average model
avg_rate_mean <- mean(train$rating)
avg_rate_mean
step1_rsme <- RMSE(test$rating,avg_rate_mean)
step1_rsme

#Step 2: Add the movie bias by each movie to the model 
bias_movie_train <- train%>%group_by(movieId) %>% summarize(movie_bias_rate = mean(rating-avg_rate_mean))
pre_rate_test_step2 <- avg_rate_mean+test %>% left_join(bias_movie_train,by = "movieId")%>%.$movie_bias_rate
step2_rsme <- RMSE(pre_rate_test_step2, test$rating)
step2_rsme

#Step 3: Add the user bias by each user to the model 2
bias_user_train <- train%>%left_join(bias_movie_train,by = "movieId")%>%group_by(userId) %>% summarize(user_bias_rate = mean(rating-avg_rate_mean-movie_bias_rate))
pre_rate_test_step3 <- pre_rate_test_step2 + test %>% left_join(bias_movie_train,by = "movieId")%>% left_join(bias_user_train,by = "userId")%>%.$user_bias_rate
step3_rsme <- RMSE(pre_rate_test_step3, test$rating)
step3_rsme

#Step4: Add the release year bias to the model in Step 3 
# Further study the release year
avg_train_yearrelease <- train%>%group_by(Year_Released)%>%summarize(avg_rate_train_yearrelease = mean(rating))
plot(avg_train_yearrelease)
#Add the release year bias to the model 
bias_year_train <-train%>%left_join(bias_movie_train,by = "movieId")%>% left_join(bias_user_train,by = "userId")%>%group_by(Year_Released) %>% summarize(year_bias_rate = mean(rating-avg_rate_mean-movie_bias_rate-user_bias_rate))
pre_rate_test_step4 <- test%>%left_join(bias_movie_train,by = "movieId")%>% left_join(bias_user_train,by = "userId")%>% left_join(bias_year_train,by = "Year_Released")%>%mutate(pre = avg_rate_mean+movie_bias_rate+user_bias_rate+year_bias_rate )%>%.$pre
step4_rsme <- RMSE(pre_rate_test_step4, test$rating)
step4_rsme


#Step 5  Add Movie Genres Bias instead of Year Released Bias to Step 3 Model 
bias_genre_train <-train%>%left_join(bias_movie_train,by = "movieId")%>% left_join(bias_user_train,by = "userId")%>%group_by(genres) %>% summarize(genre_bias_rate = mean(rating-avg_rate_mean-movie_bias_rate-user_bias_rate))
pre_rate_test_step5 <- test%>%left_join(bias_movie_train,by = "movieId")%>% left_join(bias_user_train,by = "userId")%>% left_join(bias_genre_train,by = "genres")%>%mutate(pre = avg_rate_mean+movie_bias_rate+user_bias_rate+genre_bias_rate )%>%.$pre
step5_rsme <- RMSE(pre_rate_test_step5, test$rating)
step5_rsme

#Step 6 Add all movies, users, year released and movie genres bias 
pre_rate_test_step6 <- test%>%left_join(bias_movie_train,by = "movieId")%>% left_join(bias_user_train,by = "userId")%>% left_join(bias_year_train,by = "Year_Released")%>% left_join(bias_genre_train,by = "genres")%>%mutate(pre = avg_rate_mean+movie_bias_rate+user_bias_rate+year_bias_rate+genre_bias_rate )%>%.$pre
step6_rsme <- RMSE(pre_rate_test_step6, test$rating)
step6_rsme

#Print the RSME result for all steps
modelname<- c("Average","Movie Effect","Movie User Effect","Movie User Year Combo Effect", "Movie User Genre Combo Effect","Movie User Year Genre Combo Effect" )
rsme_result <- data.frame(model=modelname,rsme= c(step1_rsme,step2_rsme,step3_rsme,step4_rsme,step5_rsme,step6_rsme))
print(rsme_result)

########################################
#  Regularization  
########################################

#Conduct regularization on model 5 "Movie User Genre Combo Effect"
lambdas <- seq(0,10,.5)
rmses <- sapply(lambdas, function(l){
  avg_rate_mean <- mean(train$rating)
  bias_movie_train <- train%>%group_by(movieId) %>% summarize(movie_bias_rate = sum(rating-avg_rate_mean)/(n()+l))
  bias_user_train <- train%>%left_join(bias_movie_train,by = "movieId")%>%group_by(userId) %>% summarize(user_bias_rate = sum(rating-avg_rate_mean-movie_bias_rate)/(n()+l))
  bias_genre_train <-train%>%left_join(bias_movie_train,by = "movieId")%>% left_join(bias_user_train,by = "userId")%>%group_by(genres) %>% summarize(genre_bias_rate = sum(rating-avg_rate_mean-movie_bias_rate-user_bias_rate)/(n()+l))
  pre_rate <- test%>%left_join(bias_movie_train,by = "movieId")%>% left_join(bias_user_train,by = "userId")%>% left_join(bias_genre_train,by = "genres")%>%mutate(pre = avg_rate_mean+movie_bias_rate+user_bias_rate+genre_bias_rate)%>%.$pre
  return(RMSE(pre_rate, test$rating))
})
qplot(lambdas,rmses)
lambdas[which.min(rmses)] 

########################################
# Part 4 Model Validation 
########################################
l <- 5
avg_rate_va <- mean(validation$rating)
bias_movie_va <- validation%>%group_by(movieId) %>% summarize(movie_bias_rate_va = sum(rating-avg_rate_va)/(n()+l))
bias_user_va <- validation%>%left_join(bias_movie_va,by = "movieId")%>%group_by(userId) %>% summarize(user_bias_rate_va = sum(rating-avg_rate_va-movie_bias_rate_va)/(n()+l))
bias_genre_va <-validation%>%left_join(bias_movie_va,by = "movieId")%>% left_join(bias_user_va,by = "userId")%>%group_by(genres) %>% summarize(genre_bias_rate_va = sum(rating-avg_rate_va-movie_bias_rate_va-user_bias_rate_va)/(n()+l))
pre_rate_va <- validation%>%left_join(bias_movie_va,by = "movieId")%>% left_join(bias_user_va,by = "userId")%>% left_join(bias_genre_va,by = "genres")%>%mutate(pre = avg_rate_mean+movie_bias_rate_va+user_bias_rate_va+genre_bias_rate_va)%>%.$pre
RMSE(pre_rate_va, validation$rating)







