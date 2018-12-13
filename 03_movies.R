library(statsr)
library(dplyr)
library(ggplot2)
library(GGally)

# load movies dataset
load("movies.Rdata")

# select relevant columns for regression analysis
movies <- movies %>% select(title_type, genre, runtime, mpaa_rating,
                            thtr_rel_year, thtr_rel_month, thtr_rel_day,
                            imdb_rating, imdb_num_votes, critics_rating,
                            critics_score, audience_rating, audience_score,
                            best_pic_nom, best_pic_win, best_actor_win,
                            best_actress_win, best_dir_win, top200_box)

" The data has been selected randomly from IMDB and Rotten Tomatoes.
  As the average person does not have an account on these websites, it's hard to say how well the audience 
  scores represent the general public opinion. The people who vote on these websites are more invested into movies 
  than those who don't. "


"------------------------------------------------------------------------------------------------------------"

" 
  Does critic score influence audience opinion?

  audience_score: Audience score on Rotten Tomatoes
  imdb_rating: Rating on IMDB

  critics_score: Critics score on Rotten Tomatoes
"

str(movies$audience_score) # numeric - integers
str(movies$imdb_rating)    # numeric - decimal
str(movies$critics_score)  # numeric - integers


# check NAs
sum(is.na(movies$audience_score))
sum(is.na(movies$imdb_rating))
sum(is.na(movies$critics_score))
# no NAs

# select columns
scoreCmp <- movies %>% select(audience_score, imdb_rating, critics_score)

# check ranges
range(scoreCmp$audience_score) # 11 - 97
range(scoreCmp$imdb_rating)    # 1.9 - 9.0
range(scoreCmp$critics_score)  # 1 - 100

# summary statistics for audience score
scoreCmp %>% summarise(mean = mean(audience_score), sd = sd(audience_score), median = median(audience_score), 
                     pc25 = quantile(audience_score, 0.25), pc75 = quantile(audience_score, 0.75))

# distribution plot
x11()
ggplot(data = scoreCmp, aes(x = audience_score)) + geom_histogram(binwidth = 10)

" A bit left-skewed. "

# summary statistics for imdb ratings
scoreCmp %>% summarise(mean = mean(imdb_rating), sd = sd(imdb_rating), median = median(imdb_rating), 
                       pc25 = quantile(imdb_rating, 0.25), pc75 = quantile(imdb_rating, 0.75))

# distribution plot
x11()
ggplot(data = scoreCmp, aes(x = imdb_rating)) + geom_histogram(binwidth = 1)

" More left-skewed than the RT audience scores. Less variability. "

# summary statistics for critics score
scoreCmp %>% summarise(mean = mean(critics_score), sd = sd(critics_score), median = median(critics_score), 
                       pc25 = quantile(critics_score, 0.25), pc75 = quantile(critics_score, 0.75))

# distribution plot
x11()
ggplot(data = scoreCmp, aes(x = critics_score)) + geom_histogram(binwidth = 10)

" Left-skewed, but a lot more evenly spread out than audience scores on RT and IMDB. "

# compare all three

scoreCmp$imdb_rating <- scoreCmp$imdb_rating * 10 # change scale of imdb rating for comparison

x11()
ggplot(data = stack(scoreCmp), aes(x = ind, y = values)) + 
  geom_boxplot() +
  ylab("score") +
  xlab("rater")

scoreCmp$imdb_rating <- scoreCmp$imdb_rating / 10 

# check correlation
x11()
ggpairs(data = scoreCmp)

" Audience ratings on RT and IMDB are highly correlated. Slightly less correlation with RT critics score. "

# build regression models

# RT audience score explained by critics score
cmpMdl_rt <- lm(audience_score ~ critics_score, data = scoreCmp)


# condition check for least squares line

# linearity and constant variability
x11()
ggplot(data = cmpMdl_rt, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

" Residuals are randomly scattered and variability is constant. "

# normal distribution of residuals

# histogram
x11()
ggplot(data = cmpMdl_rt, aes(x = .resid)) +
  geom_histogram(binwidth = 1) +
  xlab("Residuals")

" Subtle right skew. "

# normal probability plot
x11()
ggplot(data = cmpMdl_rt, aes(sample = .resid)) + stat_qq() + stat_qq_line()

" Slight distortion at the ends. "

" The conditions appear to be satisfied. "

# plot the least squares line over the points
x11()
ggplot(data = cmpMdl_rt, aes(x = critics_score, y = audience_score)) +
  geom_jitter() +
  geom_smooth(method = "lm")

summary(cmpMdl_rt)

" critics_score has a very low p-value (<2e-16), so it is a significant predictor.
  Adjusted R-squared value is 0.4952 i.e 49.52% of the variability is explained by the model.
  Slope estimate for critics_score is 0.50144; For an increase of 1 for the critics score on RT, the 
  audience score rises by 0.50144. "



# IMDB rating explained by critics score
cmpMdl_imdb <- lm(imdb_rating ~ critics_score, data = scoreCmp)


# condition check for least squares line

# linearity and constant variability
x11()
ggplot(data = cmpMdl_imdb, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

" Residuals are randomly scattered. Variability is constant and lower compared to the model for RT audience score. "

# normal distribution of residuals

# histogram
x11()
ggplot(data = cmpMdl_imdb, aes(x = .resid)) +
  geom_histogram(binwidth = 1) +
  xlab("Residuals")

" Subtle left skew. "

# normal probability plot
x11()
ggplot(data = cmpMdl_imdb, aes(sample = .resid)) + stat_qq() + stat_qq_line()

" Mostly normal, but some distortion at the ends. "

" The conditions appear to be satisfied. "

# plot the least squares line over the points
x11()
ggplot(data = cmpMdl_imdb, aes(x = critics_score, y = imdb_rating)) +
  geom_jitter() +
  geom_smooth(method = "lm")

summary(cmpMdl_imdb)

" critics_score has a very low p-value (<2e-16), so it is a significant predictor.
  Adjusted R-squared value is 0.5846 i.e 58.46% of the variability is explained by the model. 
  Slope estimate for critics_score is 0.0292177; For an increase of 10 for the critics score on RT, the 
  imdb rating rises by 0.29. "


" There does appear to be a correlation between critic and audience scores. In general, audiences 
  tend to give higher ratings to movies that are highly rated by the critics. However, audience 
  scores seem to be lower than critic scores on average. It is unknown whether audiences are 
  influenced by critics, or if their ratings are unbiased. "

rm(scoreCmp, cmpMdl_imdb, cmpMdl_rt) #cleanup


"------------------------------------------------------------------------------------------------------------"


" 
  Which kind of movies do the audiences prefer in general? 

  title_type: Type of movie (Documentary, Feature Film, TV Movie)
  genre: Genre of movie (Action & Adventure, Comedy, Documentary, Drama, Horror, Mystery & Suspense, Other)
  runtime: Runtime of movie (in minutes)
  mpaa_rating: MPAA rating of the movie (G, PG, PG-13, R, Unrated)

  audience_score: Audience score on Rotten Tomatoes
"

str(movies$title_type)  # categorical - 3 levels
str(movies$genre)       # categorical - 11 levels
str(movies$runtime)     # numeric - integer
str(movies$mpaa_rating) # ordinal - 6 levels

# check NAs
sum(is.na(movies$title_type))  # 0 NAs
sum(is.na(movies$genre))       # 0 NAs
sum(is.na(movies$runtime))     # 1 NA
sum(is.na(movies$mpaa_rating)) # 0 NAs

# select columns
movieParams <- movies %>% 
  filter(!is.na(runtime)) %>%
  select(title_type, genre, runtime, mpaa_rating, audience_score)
  

# distribution of title types in dataset
levels(movieParams$title_type)

movieParams %>%
  group_by(title_type) %>%
  summarise(pc = n()*100/nrow(movieParams))

# visualise distribution
x11()
ggplot(data = movieParams, aes(x = title_type, fill = title_type)) +
  geom_bar()

# distribution of genres in dataset
levels(movieParams$genre)

movieParams %>%
  group_by(genre) %>%
  summarise(pc = n()*100/nrow(movieParams))

# visualise distribution
x11()
ggplot(data = movieParams, aes(x = genre, fill = genre)) +
  geom_bar()

# distribution of mpaa ratings in dataset
levels(movieParams$mpaa_rating)

movieParams %>%
  group_by(mpaa_rating) %>%
  summarise(pc = n()*100/nrow(movieParams))

# visualise distribution
x11()
ggplot(data = movieParams, aes(x = mpaa_rating, fill = mpaa_rating)) +
  geom_bar()

# summary statistics for runtime
movieParams %>%
  summarise(mean = mean(runtime), sd = sd(runtime),
            median = median(runtime), pc25 = quantile(runtime, 0.25), pc75 = quantile(runtime, 0.75))

# histogram of runtime distribution
x11()
ggplot(data = movieParams, aes(x = runtime)) +
  geom_histogram(binwidth = 10)

" Right skewed distribution. "

# boxplot of runtime
x11()
ggplot(data = movieParams, aes(y = runtime)) +
  geom_boxplot()

# regression model for predicting audience score on RT using the above parameters
rating_mdl <- lm(audience_score ~ ., data = movieParams)


# condition check for least squares line

# linearity and constant variability
x11()
ggplot(data = rating_mdl, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

" Negative trend visible. "

# normal distribution of residuals

# histogram
x11()
ggplot(data = rating_mdl, aes(x = .resid)) +
  geom_histogram(binwidth = 1) +
  xlab("Residuals")

" Left skew. "

# normal probability plot
x11()
ggplot(data = rating_mdl, aes(sample = .resid)) + stat_qq() + stat_qq_line()

" Curved plot. "

" Linear model does not seem to fit. "


summary(rating_mdl)
" Adjusted R-squared value is 0.2223; Only 22.23% of the variability in the data is explained by the linear model. "

x11()
ggplot(movieParams, aes(x = runtime, y = audience_score)) +
  geom_jitter() +
  stat_smooth(method = "lm")

x11()
ggplot(movieParams, aes(x = runtime, y = audience_score, colour = title_type)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)

x11()
ggplot(movieParams, aes(x = runtime, y = audience_score, colour = mpaa_rating)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)

x11()
ggplot(movieParams, aes(x = runtime, y = audience_score, colour = genre)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)

" A linear regression model is not suitable for predicting the audience ratings through defining traits of the movies. "

rm(rating_mdl, movieParams) # cleanup


"------------------------------------------------------------------------------------------------------------"

" What are the factors that can be used to predict the audience opinion of a movie? "


# check NAs
sum(is.na(movies)) # 1 NA

# drop imdb ratings, votes, and categorical RT audience rating columns
which(names(movies) == "imdb_rating")
which(names(movies) == "imdb_num_votes")
which(names(movies) == "audience_rating")
fullDat <- movies[,c(-8,-9,-12)]

# drop row with NA
fullDat <- fullDat %>% na.omit()


# build full model
fullMdl <- lm(audience_score ~ ., data = fullDat)

summary(fullMdl)
" Adjusted R-squared value of the full model is 0.5277. "


# backwards elimination using adjusted R-squared

# copy dataset
which(names(fullDat) == "audience_score")
modldat <- fullDat[,c(10,1:9,11:16)]

# Adjusted R-squared for full model
Rsq <- summary(fullMdl)$adj.r.squared

flag <- TRUE
run <- 0

while(flag == TRUE)
{
  run <- run + 1  
  print("----------------------------------------------------------------------------------------")
  print(paste("PASS : ", run))
  print(paste("Current R-squared: ", Rsq))
  
  rvals <- data.frame(exp_var = character(),
                      Rsq = numeric(),
                      stringsAsFactors = FALSE)
  
  for(i in 2:ncol(modldat))
  {
    modl <- lm(audience_score ~ ., data = modldat[,-i])
    rvals[i-1,1] <- names(modldat[,i])
    rvals[i-1,2] <- summary(modl)$adj.r.squared
  }
  
  max_index <- which.max(rvals[,2])
  maxval <- rvals[max_index,2]
  maxvar <- rvals[max_index,1] 
  print(paste("Highest R-squared was attained by removal of: ", maxvar, " , new R-squared: ", maxval))
  
  if(max(rvals[,2]) > Rsq)
  {
    print("New R-squared is greater than previous. Dropping variable. ")
    Rsq <- maxval
    modldat <- modldat[,-(max_index + 1)]
    flag <- TRUE
    
  } else
  {
    print("New R-squared is not greater than previous. Stopping. ")
    print("Final model: ")
    print(summary(lm(audience_score ~ ., data = modldat)))
    flag <- FALSE
  }
} 

" Final R-squared value is 0.5332; 53.32% of the variability in the data is explained by the model. "


" According to the model created through backwards elimination using R-squared value, the following variables 
  are the best for predicting the audience RT scores: 
  
  genre: The order of popularity in decreasing order is as follows:
  Documentary > Musicals > Art House > Animation > Drama > Other > Action > Comedy > Mystery > Fantasy > Horror

  runtime: Higher is preferable, but not by much.
  
  theatre release year: Older movies have higher ratings, but not by much.
  
  critic rating: Movies rated as 'Certified Fresh' have the highest audience scores. 
  Movies rated as 'Rotten' are next in popularity, and 'Fresh' movies are the least liked.

  critic score: The audience score rises with the critic score.

  best picture nomination: Movies nominated for 'best picture' have much higher ratings

  best actress award winner: Ratings seem to be lower for movies that feature actresses 
  who have won the best actress award.

"


