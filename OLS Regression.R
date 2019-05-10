# Derek Higham
# OLS Regression Project

######## Download appropriate packages and install them
install.packages("rJava")
install.packages("readr")
install.packages("pbkrtest")
install.packages("car")
install.packages("leaps")
install.packages("MASS")
install.packages("xlsxjars")
install.packages("xlsx")
install.packages("mice")
install.packages("VIM")

library(rJava)
library(readr)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(xlsxjars)
library(xlsx)
library(mice)
library(VIM)


# Read in the csv file into an R data frame
path <- '/Users/derekhigham/Documents/school/MSDS 411/Unit_1/'
file <- paste(path, 'moneyball.csv', sep='')
data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)

head(data)
str(data)


# Imputation of missing data values
md.pattern(data)

impute.data <- mice(data=data, method="pmm")
summary(impute.data)

completed.data <- complete(impute.data,1)

# Add new variables
completed.data$TEAM_BATTING_1B <- completed.data$TEAM_BATTING_H - 
  completed.data$TEAM_BATTING_HR - completed.data$TEAM_BATTING_3B - 
  completed.data$TEAM_BATTING_2B

completed.data$TEAM_BASERUN_SB_PCT <- completed.data$TEAM_BASERUN_SB/
  (1.0*completed.data$TEAM_BASERUN_SB + completed.data$TEAM_BASERUN_CS)


# Summary stats of imputed data set - include in write-up
summary(completed.data)


###### Exploratory Data Analysis #####

# Perform EDA to manually identify potential predictor variables (model.1)

# Create Histogram and Boxplot pairings for each variable to include in the appendix.
# Wins - Use lower bound for lower outliers, upper bound for higher outliers.
par(mfrow=c(1,2))
hist(completed.data$TARGET_WINS, col = "#A71930", xlab = "TARGET_WINS", 
     main = "Histogram of Wins")
boxplot(completed.data$TARGET_WINS, col = "#A71930", main = "Boxplot of Wins")


# Hits and Doubles
par(mfrow=c(2,2))
hist(completed.data$TEAM_BATTING_H, col = "#A71930", xlab = "Team_Batting_H", 
     main = "Histogram of Hits")
hist(completed.data$TEAM_BATTING_2B, col = "#09ADAD", xlab = "Doubles", 
     main = "Histogram of Doubles")
boxplot(completed.data$TEAM_BATTING_H, col = "#A71930", main = "Boxplot of Hits")
boxplot(completed.data$TEAM_BATTING_2B, col = "#09ADAD", main = "Boxplot of Doubles")


# Triples and Home Runs
par(mfrow=c(2,2))
hist(completed.data$TEAM_BATTING_3B, col = "#A71930", xlab = "Triples", 
     main = "Histogram of Triples")
hist(completed.data$TEAM_BATTING_HR, col = "#DBCEAC", xlab = "Home Runs", 
     main = "Histogram of Home Runs")
boxplot(completed.data$TEAM_BATTING_3B, col = "#A71930", main = "Boxplot of Triples")
boxplot(completed.data$TEAM_BATTING_HR, col = "#DBCEAC", main = "Boxplot of Home Runs")


# Walks, Strikeouts, HBP
par(mfrow=c(2,3))
hist(completed.data$TEAM_BATTING_BB, col = "#A71930", xlab = "Walks", 
     main = "Histogram of Walks")
hist(completed.data$TEAM_BATTING_SO, col = "#09ADAD", xlab = "Strikeouts", 
     main = "Histogram of Strikeouts")
hist(completed.data$TEAM_BATTING_HBP, col = "#DBCEAC", xlab = "Hit By Pitches", 
     main = "Histogram of HBP")
boxplot(completed.data$TEAM_BATTING_BB, col = "#A71930", main = "Boxplot of Walks")
boxplot(completed.data$TEAM_BATTING_SO, col = "#09ADAD", main = "Boxplot of Strikeouts")
boxplot(completed.data$TEAM_BATTING_HBP, col = "#DBCEAC", main = "Boxplot of HBP")


# Stolen Bases and Caught Stealing
par(mfrow=c(2,2))
hist(completed.data$TEAM_BASERUN_SB, col = "#A71930", xlab = "Stolen Bases", 
     main = "Histogram of Steals")
hist(completed.data$TEAM_BASERUN_CS, col = "#DBCEAC", xlab = "Caught Stealing", 
     main = "Histogram of CS")
boxplot(completed.data$TEAM_BASERUN_SB, col = "#A71930", main = "Boxplot of Steals")
boxplot(completed.data$TEAM_BASERUN_CS, col = "#DBCEAC", main = "Boxplot of CS")


# Hits and Home Runs
par(mfrow=c(2,2))
hist(completed.data$TEAM_PITCHING_H, col = "#A71930", xlab = "Hits Against", 
     main = "Histogram of Hits Against")
hist(completed.data$TEAM_PITCHING_HR, col = "#09ADAD", xlab = "Home Runs Against", 
     main = "Histograms of HR Against")
boxplot(completed.data$TEAM_PITCHING_H, col = "#A71930", 
        main = "Boxplot of Hits Against")
boxplot(completed.data$TEAM_PITCHING_HR, col = "#09ADAD", 
        main = "Boxplot of HR Against")


# Walks and Strikeouts
par(mfrow=c(2,2))
hist(completed.data$TEAM_PITCHING_BB, col = "#A71930", xlab = "Walks Allowed", 
     main = "Histogram of Walks Allowed")
hist(completed.data$TEAM_PITCHING_SO, col = "#DBCEAC", xlab = "Strikeouts", 
     main = "Histograms of Strikeouts")
boxplot(completed.data$TEAM_PITCHING_BB, col = "#A71930", 
        main = "Boxplot of Walks Allowed")
boxplot(completed.data$TEAM_PITCHING_SO, col = "#DBCEAC", 
        main = "Boxplot of Strikeouts")


# Double Plays and Errors 
par(mfrow=c(2,2))
hist(completed.data$TEAM_FIELDING_DP, col = "#A71930", xlab = "Double Plays", 
     main = "Histogram of Double Plays")
hist(completed.data$TEAM_FIELDING_E, col = "#09ADAD", xlab = "Errors Committed", 
     main = "Histogram of Errors Committed")
boxplot(completed.data$TEAM_FIELDING_DP, col = "#A71930", 
        main = "Boxplot of Double Plays")
boxplot(completed.data$TEAM_FIELDING_E, col = "#09ADAD", 
        main = "Boxplot of Errors Committed")


# Singles and Stolen Base %
par(mfrow=c(2,2))
hist(completed.data$TEAM_BATTING_1B, col = "#A71930", xlab = "Singles", 
     main = "Histogram of Singles")
hist(completed.data$TEAM_BASERUN_SB_PCT, col = "#A71930", xlab = "Stolen Base Pct", 
     main = "Histogram of Stolen Base Pct")
boxplot(completed.data$TEAM_BATTING_1B, col = "#A71930", 
        main = "Boxplot of Singles")
boxplot(completed.data$TEAM_BASERUN_SB_PCT, col = "#A71930", 
        main = "Boxplot of Stolen Base Pct")



# Create scatterplots of each predictor variable against TARGET_WINS to identify
# potential candidate predictors
par(mfrow = c(2, 2))
plot(completed.data$TEAM_BATTING_H,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Base Hits - Batting & Wins')
plot(completed.data$TEAM_BATTING_2B,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Doubles - Batting & Wins')
plot(completed.data$TEAM_BATTING_3B,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Triples - Batting & Wins')
plot(completed.data$TEAM_BATTING_HR,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Homeruns - Batting & Wins')

par(mfrow = c(2, 2))
plot(completed.data$TEAM_BATTING_BB,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Walks - Batting & Wins')
plot(completed.data$TEAM_BATTING_SO,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Strike Outs - Batting & Wins')
plot(completed.data$TEAM_BASERUN_SB,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Stolen Bases - Batting & Wins')
plot(completed.data$TEAM_BASERUN_CS,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Caught Stealing - Batting & Wins')

par(mfrow = c(2, 2))
plot(completed.data$TEAM_BATTING_HBP,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Hit By Pitch - Batting & Wins')
plot(completed.data$TEAM_PITCHING_H,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Base Hits - Pitching & Wins')
plot(completed.data$TEAM_PITCHING_HR,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Homeruns - Pitching & Wins')
plot(completed.data$TEAM_PITCHING_BB,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Walks - Pitching & Wins')

par(mfrow = c(2, 2))
plot(completed.data$TEAM_PITCHING_SO,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Strike Outs - Pitching & Wins')
plot(completed.data$TEAM_FIELDING_E,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Errors - Pitching & Wins')
plot(completed.data$TEAM_FIELDING_DP,completed.data$TARGET_WINS,
     xlab='Base Hits - Batting',ylab='Wins',main='Double Plays - Pitching & Wins')

par(mfrow = c(1, 2))
plot(completed.data$TEAM_BATTING_1B,completed.data$TARGET_WINS,
     xlab='Singles - Batting',ylab='Wins',main='Singles - Batting & Wins')
plot(completed.data$TEAM_BASERUN_SB_PCT,completed.data$TARGET_WINS,
     xlab='Stolen Base Pct - Batting',ylab='Wins',main='Stolen Base Pct - Batting & Wins')



##### Model Development #####
# model.1
# Fit Linear Regression model
model.1 <- lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_1B + TEAM_BATTING_2B +
                TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_PITCHING_H +
                TEAM_PITCHING_HR + TEAM_PITCHING_BB, data=completed.data)

# Display model summary
summary(model.1)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.1 <- mean(model.1$residuals^2)
mae.1 <- mean(abs(model.1$residuals))
mse.1
mae.1

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.1$residuals)
qqline(model.1$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.data$TEAM_BATTING_H,model.1$residuals,xlab='Predictor: TEAM_BATTING_H',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_1B,model.1$residuals,xlab='Predictor: TEAM_BATTING_1B',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_2B,model.1$residuals,xlab='Predictor: TEAM_BATTING_2B',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_BB,model.1$residuals,xlab='Predictor: TEAM_BATTING_BB',
     ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))
plot(completed.data$TEAM_BATTING_SO,model.1$residuals,xlab='Predictor: TEAM_BATTING_SO',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BASERUN_SB,model.1$residuals,xlab='Predictor: TEAM_BASERUN_SB',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_PITCHING_H,model.1$residuals,xlab='Predictor: TEAM_PITCHING_H',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_PITCHING_HR,model.1$residuals,xlab='Predictor: TEAM_PITCHING_HR',
     ylab='Residual')
title('Residual vs Predictor')

plot(completed.data$TEAM_PITCHING_BB,model.1$residuals,xlab='Predictor: TEAM_PITCHING_BB',
     ylab='Residual')
title('Residual vs Predictor')


# Stepwise (both) variable selection
both.lm <- lm(formula = TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_1B + 
                    TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
                    TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + 
                    TEAM_BASERUN_CS + TEAM_BATTING_HBP + TEAM_PITCHING_H + 
                    TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_PITCHING_SO + 
                    TEAM_FIELDING_E + TEAM_FIELDING_DP, 
                  data = completed.data, na.action=na.exclude)
stepwise.both.lm <- stepAIC(both.lm, direction = "both")
summary(stepwise.both.lm)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.both.lm <- mean(stepwise.both.lm$residuals^2)
mae.both.lm <- mean(abs(stepwise.both.lm$residuals))
mse.both.lm
mae.both.lm

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(stepwise.both.lm$residuals)
qqline(stepwise.both.lm$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.data$TEAM_BATTING_H,stepwise.both.lm$residuals,xlab='Predictor: TEAM_BATTING_H',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_1B,stepwise.both.lm$residuals,xlab='Predictor: TEAM_BATTING_1B',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_2B,stepwise.both.lm$residuals,xlab='Predictor: TEAM_BATTING_2B',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_BB,stepwise.both.lm$residuals,xlab='Predictor: TEAM_BATTING_BB',
     ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))
plot(completed.data$TEAM_BATTING_SO,stepwise.both.lm$residuals,xlab='Predictor: TEAM_BATTING_SO',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BASERUN_SB,stepwise.both.lm$residuals,xlab='Predictor: TEAM_BASERUN_SB',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_HBP,stepwise.both.lm$residuals,xlab='Predictor: TEAM_BATTING_HBP',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_PITCHING_H,stepwise.both.lm$residuals,xlab='Predictor: TEAM_PITCHING_H',
     ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))
plot(completed.data$TEAM_PITCHING_HR,stepwise.both.lm$residuals,xlab='Predictor: TEAM_PITCHING_HR',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_PITCHING_BB,stepwise.both.lm$residuals,xlab='Predictor: TEAM_PITCHING_BB',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_PITCHING_SO,stepwise.both.lm$residuals,xlab='Predictor: TEAM_PITCHING_SO',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_FIELDING_E,stepwise.both.lm$residuals,xlab='Predictor: TEAM_FIELDING_E',
     ylab='Residual')
title('Residual vs Predictor')

plot(completed.data$TEAM_FIELDING_DP,stepwise.both.lm$residuals,xlab='Predictor: TEAM_FIELDING_DP',
     ylab='Residual')
title('Residual vs Predictor')


# Compute the VIF values for the stepwise variable selection method
library(car)
sort(vif(stepwise.both.lm),decreasing=TRUE)

# Stepwise (both) variable selection - removing bad variables
both.2.lm <- lm(formula = TARGET_WINS ~ TEAM_PITCHING_HR + TEAM_BATTING_2B +
                  TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_FIELDING_E +
                  TEAM_PITCHING_BB + TEAM_PITCHING_H + TEAM_PITCHING_SO +
                  TEAM_BASERUN_SB + TEAM_FIELDING_DP + TEAM_BATTING_HBP, 
              data = completed.data, na.action=na.exclude)
summary(both.2.lm)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.both.2.lm <- mean(both.2.lm$residuals^2)
mae.both.2.lm <- mean(abs(both.2.lm$residuals))
mse.both.2.lm
mae.both.2.lm


# Stepwise (both) variable selection with logarithmic transformations

# Perform logarithmic transformations of predictor variables with no 0 values
completed.data$log_TEAM_BATTING_H <- log(completed.data$TEAM_BATTING_H)
completed.data$log_TEAM_BATTING_1B <- log(completed.data$TEAM_BATTING_1B)
completed.data$log_TEAM_BATTING_2B <- log(completed.data$TEAM_BATTING_2B)
completed.data$log_TEAM_BATTING_BB <- log(completed.data$TEAM_BATTING_BB)
completed.data$log_TEAM_BATTING_HBP <- log(completed.data$TEAM_BATTING_HBP)
completed.data$log_TEAM_PITCHING_H <- log(completed.data$TEAM_PITCHING_H)
completed.data$log_TEAM_FIELDING_E <- log(completed.data$TEAM_FIELDING_E)
completed.data$log_TEAM_FIELDING_DP <- log(completed.data$TEAM_FIELDING_DP)

# Stepwise (both) variable selection
log.both.lm <- lm(formula = TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_1B + 
                TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
                TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + 
                TEAM_BASERUN_CS + TEAM_BATTING_HBP + TEAM_PITCHING_H + 
                TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_PITCHING_SO + 
                TEAM_FIELDING_E + TEAM_FIELDING_DP + log_TEAM_BATTING_H + 
                  log_TEAM_BATTING_1B + log_TEAM_BATTING_2B + log_TEAM_BATTING_HBP + 
                  log_TEAM_PITCHING_H + log_TEAM_FIELDING_E + log_TEAM_FIELDING_DP, 
              data = completed.data, na.action=na.exclude)
stepwise.log.both.lm <- stepAIC(log.both.lm, direction = "both")
summary(stepwise.log.both.lm)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.log.both.lm <- mean(stepwise.log.both.lm$residuals^2)
mae.log.both.lm <- mean(abs(stepwise.log.both.lm$residuals))
mse.log.both.lm
mae.log.both.lm

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(log.both.lm$residuals)
qqline(log.both.lm$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.data$TEAM_BATTING_H,log.both.lm$residuals,xlab='Predictor: TEAM_BATTING_H',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_2B,log.both.lm$residuals,xlab='Predictor: TEAM_BATTING_2B',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_BB,log.both.lm$residuals,xlab='Predictor: TEAM_BATTING_BB',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_SO,log.both.lm$residuals,xlab='Predictor: TEAM_BATTING_SO',
     ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))
plot(completed.data$TEAM_BASERUN_SB,log.both.lm$residuals,xlab='Predictor: TEAM_BASERUN_SB',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BASERUN_CS,log.both.lm$residuals,xlab='Predictor: TEAM_BASERUN_CS',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_BATTING_HBP,log.both.lm$residuals,xlab='Predictor: TEAM_BATTING_HBP',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_PITCHING_H,log.both.lm$residuals,xlab='Predictor: TEAM_PITCHING_H',
     ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))
plot(completed.data$TEAM_PITCHING_HR,log.both.lm$residuals,xlab='Predictor: TEAM_PITCHING_HR',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_PITCHING_BB,log.both.lm$residuals,xlab='Predictor: TEAM_PITCHING_BB',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_PITCHING_SO,log.both.lm$residuals,xlab='Predictor: TEAM_PITCHING_SO',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$TEAM_FIELDING_E,log.both.lm$residuals,xlab='Predictor: TEAM_FIELDING_E',
     ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))
plot(completed.data$TEAM_FIELDING_DP,log.both.lm$residuals,xlab='Predictor: TEAM_FIELDING_DP',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$log_TEAM_BATTING_1B,log.both.lm$residuals,xlab='Predictor: log_TEAM_BATTING_1B',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$log_TEAM_BATTING_2B,log.both.lm$residuals,xlab='Predictor: log_TEAM_BATTING_2B',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$log_TEAM_PITCHING_H,log.both.lm$residuals,xlab='Predictor: log_TEAM_PITCHING_H',
     ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(1, 2))
plot(completed.data$log_TEAM_FIELDING_E,log.both.lm$residuals,xlab='Predictor: log_TEAM_FIELDING_E',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.data$log_TEAM_FIELDING_DP,log.both.lm$residuals,xlab='Predictor: log_TEAM_FIELDING_DP',
     ylab='Residual')
title('Residual vs Predictor')


##### Model Comparison #####
# Calculate AIC for all 4 models
AIC(model.1)
AIC(stepwise.both.lm)
AIC(both.2.lm)
AIC(log.both.lm)

# Calculate BIC for all 4 models
BIC(model.1)
BIC(stepwise.both.lm)
BIC(both.2.lm)
BIC(log.both.lm)


##### Model Scoring of Test Data #####
# Read in the csv file into an R data frame
path <- '/Users/derekhigham/Documents/school/MSDS 411/Unit_1/'
file <- paste(path, 'moneyball_test.csv', sep='')
test.data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)

head(test.data)
str(test.data)


# Imputation of missing data values
md.pattern(test.data)

impute.test.data <- mice(data=test.data, method="pmm")
summary(impute.test.data)

completed.test.data <- complete(impute.test.data,1)

# Add new variables
completed.test.data$TEAM_BATTING_1B <- completed.test.data$TEAM_BATTING_H - 
  completed.test.data$TEAM_BATTING_HR - completed.test.data$TEAM_BATTING_3B - 
  completed.test.data$TEAM_BATTING_2B

completed.test.data$TEAM_BASERUN_SB_PCT <- completed.test.data$TEAM_BASERUN_SB/
  (1.0*completed.test.data$TEAM_BASERUN_SB + completed.test.data$TEAM_BASERUN_CS)

# Summary stats of imputed data set - include in write-up
summary(completed.test.data)

# Perform logarithmic transformations of predictor variables with no 0 values
completed.test.data$log_TEAM_BATTING_H <- log(completed.test.data$TEAM_BATTING_H)
completed.test.data$log_TEAM_BATTING_1B <- log(completed.test.data$TEAM_BATTING_1B)
completed.test.data$log_TEAM_BATTING_2B <- log(completed.test.data$TEAM_BATTING_2B)
completed.test.data$log_TEAM_BATTING_BB <- log(completed.test.data$TEAM_BATTING_BB)
completed.test.data$log_TEAM_BATTING_HBP <- log(completed.test.data$TEAM_BATTING_HBP)
completed.test.data$log_TEAM_PITCHING_H <- log(completed.test.data$TEAM_PITCHING_H)
completed.test.data$log_TEAM_FIELDING_E <- log(completed.test.data$TEAM_FIELDING_E)
completed.test.data$log_TEAM_FIELDING_DP <- log(completed.test.data$TEAM_FIELDING_DP)

# Stand Alone Scoring Process using stepwise.both.lm
completed.test.data$P_TARGET_WINS <- 41.1641753 +
  0.0841544*completed.test.data$TEAM_BATTING_H -
  0.0427669*completed.test.data$TEAM_BATTING_1B -
  0.0599690*completed.test.data$TEAM_BATTING_2B +
  0.0171084*completed.test.data$TEAM_BATTING_BB -
  0.0179485*completed.test.data$TEAM_BATTING_SO +
  0.0539319*completed.test.data$TEAM_BASERUN_SB -
  0.0566865*completed.test.data$TEAM_BATTING_HBP +
  0.0014089*completed.test.data$TEAM_PITCHING_H +
  0.0407459*completed.test.data$TEAM_PITCHING_HR -
  0.0103917*completed.test.data$TEAM_PITCHING_BB +
  0.0037110*completed.test.data$TEAM_PITCHING_SO -
  0.0431048*completed.test.data$TEAM_FIELDING_E -
  0.1183308*completed.test.data$TEAM_FIELDING_DP

#subset of data set for the deliverable "Scored data file"
prediction <- completed.test.data[c("INDEX","P_TARGET_WINS")]

# Write prediction data set to directory
write.csv(prediction, '/Users/derekhigham/Documents/school/MSDS 411/Unit_1/prediction.csv')
  

