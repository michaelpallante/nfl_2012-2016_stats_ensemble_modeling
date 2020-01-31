# Assignment 4 R Code
# MSDS 456
# Michael Pallante


# Data imported from https://github.com/maksimhorowitz/nflscrapR/tree/master/data
# NFL Player Statistics Data from 2012-2016


# The objective of this assignment is to use NFL player statistics from 2012-2016 as measurables to build a 
# predictive model that predicts future regular season offensive touchdown output. The offensive categories 
# included are passing, rushing, and receiving statistics from every regular season game of each season. Our data
# will be split into a training and test set after data preparation procedures. The objective of this assignment will
# be completed using an ensemble predictive model. Each individual model within our ensemble model will be a multiple
# linear regression model. We will compare our evaluation model with a null model to see if using previous season
# player statistics is a better predictor of future season player statistics than that of the basic null
# model. Using cross validation techniques, we will see whether or not our ensemble model performs better than
# the null model. The model performance evaluation metric that we will be using is root mean squared error, or RMSE.


# Set the R working directory.
setwd("~/Northwestern University/Summer 2018 Quarter/MSDS 456/Assignments/Assignment 4/NFLPlayerStatsData")


# Load the raw data from https://github.com/maksimhorowitz/nflscrapR/tree/master/data into R.
load("C:/Users/Michael/AppData/Local/Temp/playerstats16-4.rda")
load("C:/Users/Michael/AppData/Local/Temp/playerstats15-2.rda")
load("C:/Users/Michael/AppData/Local/Temp/playerstats14-1.rda")
load("C:/Users/Michael/AppData/Local/Temp/playerstats13-1.rda")
load("C:/Users/Michael/AppData/Local/Temp/playerstats12-1.rda")


# Export the loaded raw data into .csv files.
write.csv(playerstats12, "NFLPlayerStats2012.csv")
write.csv(playerstats13, "NFLPlayerStats2013.csv")
write.csv(playerstats14, "NFLPlayerStats2014.csv")
write.csv(playerstats15, "NFLPlayerStats2015.csv")
write.csv(playerstats16, "NFLPlayerStats2016.csv")


# Read the .csv files into R and prepare the data.
stats2012 <- read.csv("NFLPlayerStats2012.csv", stringsAsFactors = FALSE, 
                      colClasses = c("character","character","character","character",
                                     "character","character","character","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

stats2012keep <- stats2012[,c("Season","Team","name","pass.att","pass.comp","passyds","pass.tds",  
                                  "rush.att","rushyds","rushtds","recept","recyds","rec.tds","games")]

stats2013 <- read.csv("NFLPlayerStats2013.csv", stringsAsFactors = FALSE, 
                      colClasses = c("character","character","character","character",
                                     "character","character","character","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

stats2013keep <- stats2013[,c("Season","Team","name","pass.att","pass.comp","passyds","pass.tds",  
                              "rush.att","rushyds","rushtds","recept","recyds","rec.tds","games")]

stats2014 <- read.csv("NFLPlayerStats2014.csv", stringsAsFactors = FALSE, 
                      colClasses = c("character","character","character","character",
                                     "character","character","character","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

stats2014keep <- stats2014[,c("Season","Team","name","pass.att","pass.comp","passyds","pass.tds",  
                              "rush.att","rushyds","rushtds","recept","recyds","rec.tds","games")]

stats2015 <- read.csv("NFLPlayerStats2015.csv", stringsAsFactors = FALSE, 
                      colClasses = c("character","character","character","character",
                                     "character","character","character","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

stats2015keep <- stats2015[,c("Season","Team","name","pass.att","pass.comp","passyds","pass.tds",  
                              "rush.att","rushyds","rushtds","recept","recyds","rec.tds","games")]

stats2016 <- read.csv("NFLPlayerStats2016.csv", stringsAsFactors = FALSE, 
                      colClasses = c("character","character","character","character",
                                     "character","character","character","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

stats2016keep <- stats2016[,c("Season","Team","name","pass.att","pass.comp","passyds","pass.tds",  
                              "rush.att","rushyds","rushtds","recept","recyds","rec.tds","games")]


# Split the data into training and test data sets, and then upload into .csv files.
training <- rbind(stats2012keep, stats2013keep, stats2014keep, stats2015keep)

test <- stats2016keep


# Develop a variable as a summary measure for total touchdowns, total.tds, in training and test data sets.
training$total.tds <- rowSums(training[,c("pass.tds","rushtds","rec.tds")])

test$total.tds <- rowSums(test[,c("pass.tds","rushtds","rec.tds")])


# Export training and test data into .csv files.
write.csv(training, "Training.csv")

write.csv(test, "Test.csv")


# View structure and summary of training and test data sets to review and understand the data.
str(training)
summary(training)

str(test)
summary(test)


# The modeling function for this assignment may involve many steps, so
# we set it up as a function. Input to the function includes definition of
# the training and test sets for an iteration/fold of cross-validation.
# Within the cross-validation procedure, training_input and test_input will be 
# subsets of the original full training set. At the very end of the model
# development process, of course, training_input and test_input will be the full
# training and test sets, but that only comes at the very end of the process.
# The function returns the root mean-square error in test_input set. 

eval_model <- function(training_input, test_input) {
  passing.model <- lm(total.tds ~ pass.att + pass.comp + passyds + pass.tds, data = training_input, na.action = 'na.omit')
  passing.predict <- rep(NA, times = nrow(test_input))
  for (i in seq(along = test_input$name))
    if (!is.na(test_input$pass.att[i]) && !is.na(test_input$pass.comp[i])
        && !is.na(test_input$passyds[i]) && !is.na(test_input$pass.tds[i]))
      passing.predict[i] <- predict.lm(passing.model, newdata = test_input[i,])
  
  rushing.model <- lm(total.tds ~ rush.att + rushyds + rushtds, data = training_input, na.action = 'na.omit')
  rushing.predict <- rep(NA, times = nrow(test_input))
  for (i in seq(along = test_input$name))
    if (!is.na(test_input$rush.att[i]) && !is.na(test_input$rushyds[i]) && !is.na(test_input$rushtds[i]))
      rushing.predict[i] <- predict.lm(rushing.model, newdata = test_input[i,])
  
  receiving.model <- lm(total.tds ~ recept + recyds + rec.tds, data = training_input, na.action = 'na.omit')
  receiving.predict <- rep(NA, times = nrow(test_input))
  for (i in seq(along = test_input$name))
    if (!is.na(test_input$recept[i]) && !is.na(test_input$recyds[i]) && !is.na(test_input$rec.tds[i]))
      receiving.predict[i] <- predict.lm(receiving.model, newdata = test_input[i,])
  
  offensivetd.model <- lm(total.tds ~ pass.tds + rushtds + rec.tds, data = training_input, na.action = 'na.omit')
  offensivetd.predict <- rep(NA, times = nrow(test_input))
  for (i in seq(along = test_input$name))
    if (!is.na(test_input$pass.tds[i]) && !is.na(test_input$rushtds[i]) && !is.na(test_input$rec.tds[i]))
      offensivetd.predict[i] <- predict.lm(offensivetd.model, newdata = test_input[i,])
  
  
  # We are creating an ensemble or hybrid prediction by averaging all component
  # model predictions with non-missing values. This is done one player at a time.    
  response_predict <- rep(NA, times = nrow(test_input))
  for (i in seq(along = test_input$name)) 
    response_predict[i] <- mean(c(passing.predict[i], rushing.predict[i], receiving.predict[i],
                                  offensivetd.predict[i]), na.rm = TRUE)
  
  response_actual <- test_input$total.tds
  ensemble_data_frame <- data.frame(passing.predict, rushing.predict, receiving.predict,
                                    offensivetd.predict, response_predict, response_actual)
  
  
  # To check calculations, we can examine the first rows of the ensemble_data_frame
  cat('\nFirst and last six rows of ensemble_data_frame\n')
  print(head(ensemble_data_frame)) 
  cat(' . . . \n')
  print(tail(ensemble_data_frame))        
  
  
  # compute and return root mean-square error in test_input
  sqrt(mean((response_predict - response_actual)^2, na.rm = TRUE))
}


# Whatever model is used for prediction, we want it to do better than a null model
# that predicts the mean response value for every player. Null model is like no model.
null_model <- function(training_input, test_input) {
  # for demonstration purposes we show what would be the prediction 
  # of a null model... predicting the mean touchdowns for every player in test_input
  response_predict <- mean(test_input$total.tds)
  response_actual <- test_input$total.tds
  # compute and return root mean-square error in test_input
  sqrt(mean((response_predict - response_actual)^2))
}


# Cross-validation work
library(cvTools)
set.seed(9999)  # for reproducibility   
nfolds <- 10                  

study_folds <- cvFolds(nrow(training), K = nfolds, type = 'consecutive')

cv_model_results <- numeric(nfolds)  # initialize array to store fold model results
cv_null_results <- numeric(nfolds)  # initialize array to store fold null results
for (ifold in 1:nfolds) {
  cat('\nWorking on fold ', ifold, '\n')
  this_fold_test_data <- training[study_folds$which == ifold,]
  this_fold_training_data <- 
    training[study_folds$which != ifold,]
  # fit model and get root mean-square error for this iteration   
  cv_model_results[ifold] <- eval_model(training_input = this_fold_training_data,
                                        test_input = this_fold_test_data)    
  cv_null_results[ifold] <- null_model(training_input = this_fold_training_data,
                                       test_input = this_fold_test_data)    
}
cat('\n', 'Cross-validation My Model Average Root Mean-Square Error:', 
    mean(cv_model_results))  

cat('\n', 'Cross-validation No Model Average Root Mean-Square Error:', 
    mean(cv_null_results))

cv_model_results_mean <- mean(cv_model_results)
cv_null_results_mean <- mean(cv_null_results)


# Visualizations of Results
plot(cv_model_results, xlab = "Model Results", ylab = "RMSE", main = "Model Performance", type = "p", col = "blue", ylim = c(0,0.6), xlim = c(0,10))
points(cv_null_results, col = "red")
abline(h = cv_model_results_mean, col = "blue")
abline(h = cv_null_results_mean, col = "red")
legend("bottomleft", legend=c("Evaluation Model RMSE Values", "Null Model RMSE Values"), col=c("blue", "red"), pch = 1, bty= "n", cex=0.8)
legend("bottomright", legend=c("Evaluation Model Average RMSE", "Null Model Average RMSE"), col=c("blue", "red"),lty=c(1,1),bty= "n", cex=0.8)


