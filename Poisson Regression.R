# Derek Higham
# Poisson Regression

##### Download appropriate packages and install them #####
install.packages('aod', dependencies=TRUE)
install.packages('car', dependencies=TRUE)
install.packages('corrplot', dependencies=TRUE)
install.packages('dplyr', dependencies=TRUE)
install.packages('glm2', dependencies=TRUE)
install.packages('InformationValue', dependencies=TRUE)
install.packages('leaps', dependencies=TRUE)
install.packages('MASS', dependencies=TRUE)
install.packages('mice', dependencies=TRUE)
install.packages('pbkrtest', dependencies=TRUE)
install.packages('psych', dependencies=TRUE)
install.packages('readr', dependencies=TRUE)
install.packages('rJava', dependencies=TRUE)
install.packages('ROCR', dependencies=TRUE)
install.packages('simputation', dependencies=TRUE)
install.packages('VIM', dependencies=TRUE)
install.packages('xlsx', dependencies=TRUE)
install.packages('xlsxjars', dependencies=TRUE)
install.packages('zoo', dependencies=TRUE)
install.packages('missForest', dependencies=TRUE)
install.packages('pscl', dependencies=TRUE)
install.packages('ggplot2', dependencies=TRUE)

library(aod)
library(car)
library(corrplot)
library(dplyr)
library(glm2)
library(InformationValue)
library(leaps)
library(MASS)
library(mice)
library(pbkrtest)
library(psych)
library(readr)
library(rJava)
library(ROCR)
library(simputation)
library(VIM)
library(xlsx)
library(xlsxjars)
library(zoo)
library(missForest)
library(pscl)
library(ggplot2)


##### Read in the csv file into an R data frame #####
path <- '/Users/derekhigham/Documents/school/MSDS 411/Unit 3/'
file <- paste(path, 'Wine_Training.csv', sep='')
wine <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)

head(wine)
str(wine)


##### Imputation Process #####
# check min values of all variables for any negative 
# values that may need to be adjusted
summary(wine)

# to address invalid negative values for chemistry variables,
# we first tranformed any negative values to NA (null) values
wine$FixedAcidity[wine$FixedAcidity < 0] <- NA
wine$VolatileAcidity[wine$VolatileAcidity < 0] <- NA
wine$CitricAcid[wine$CitricAcid < 0] <- NA
wine$ResidualSugar[wine$ResidualSugar < 0] <- NA
wine$Chlorides[wine$Chlorides < 0] <- NA
wine$FreeSulfurDioxide[wine$FreeSulfurDioxide < 0] <- NA
wine$TotalSulfurDioxide[wine$TotalSulfurDioxide < 0] <- NA
wine$Sulphates[wine$Sulphates < 0] <- NA
wine$Alcohol[wine$Alcohol < 0] <- NA

# check if the above transformations worked
summary(wine)

# now that negative values have been transformed to NA values,
# we will use predictive mean-matching and the mice function to
# impute missing values across the data set
md.pattern(wine)

impute.wine <- mice(data=wine, method="pmm", seed=300)
summary(impute.wine)

# summary of imputed dataset - include in write-up
completed.wine <- complete(impute.wine,1)

# round LabelAppeal and STARS variables to nearest whole number to comply
# with the intent of the variable structure
completed.wine$STARS <- round(completed.wine$STARS, digits = 0)

summary(completed.wine)

# write imputed (final) dataset to local drive
write.csv(completed.wine, '/Users/derekhigham/Documents/school/MSDS 411/Unit 3/completed_wine.csv')


##### Exploratory Data Analysis #####

# Distribution of TARGET variable
ggplot(data=completed.wine, aes(completed.wine$TARGET)) + 
  geom_histogram(binwidth=1, col="BLUE", aes(fill=..count..)) + 
  scale_fill_gradient("Count", low="BLUE", high="RED")

target.mean <- mean(completed.wine$TARGET)
target.mean
target.variance <- var(completed.wine$TARGET)
target.variance

# Construct an initial correlation plot of the variables in completed.wine
# Default uses Pearson coefficient - Look at correlations with TARGET
# Significant variables will be used for manual variable selection
cor(completed.wine)


# Create Histogram and Boxplot pairings for each variable to include in the appendix.
# Wins - Use lower bound for lower outliers, upper bound for higher outliers.
hist(completed.wine$TARGET, col = "#A71930", xlab = "TARGET", 
     main = "Histogram of Wins")

# FixedAcidity, VolatileAcidity, CitricAcid, and ResidualSugar
par(mfrow=c(2,2))
hist(completed.wine$FixedAcidity, col = "#A71930", xlab = "FixedAcidity", 
     main = "Histogram of FixedAcidity")
hist(completed.wine$VolatileAcidity, col = "#09ADAD", xlab = "VolatileAcidity", 
     main = "Histogram of VolatileAcidity")
hist(completed.wine$CitricAcid, col = "#A71930", xlab = "CitricAcid", 
     main = "Histogram of CitricAcid")
hist(completed.wine$ResidualSugar, col = "#09ADAD", xlab = "ResidualSugar", 
     main = "Histogram of ResidualSugar")

# Chlorides, FreeSulfurDioxide, TotalSulfurDioxide, and Density
par(mfrow=c(2,2))
hist(completed.wine$Chlorides, col = "#A71930", xlab = "Chlorides", 
     main = "Histogram of Chlorides")
hist(completed.wine$FreeSulfurDioxide, col = "#09ADAD", xlab = "FreeSulfurDioxide", 
     main = "Histogram of FreeSulfurDioxide")
hist(completed.wine$TotalSulfurDioxide, col = "#A71930", xlab = "TotalSulfurDioxide", 
     main = "Histogram of TotalSulfurDioxide")
hist(completed.wine$Density, col = "#09ADAD", xlab = "Density", 
     main = "Histogram of Density")

# pH, Sulphates, Alcohol and LabelAppeal
par(mfrow=c(2,2))
hist(completed.wine$pH, col = "#A71930", xlab = "pH", 
     main = "Histogram of pH")
hist(completed.wine$Sulphates, col = "#09ADAD", xlab = "Sulphates", 
     main = "Histogram of Sulphates")
hist(completed.wine$Alcohol, col = "#A71930", xlab = "Alcohol", 
     main = "Histogram of Alcohol")
hist(completed.wine$LabelAppeal, col = "#09ADAD", xlab = "LabelAppeal", 
     main = "Histogram of LabelAppeal")

# AcidIndex and STARS
par(mfrow=c(1,2))
hist(completed.wine$AcidIndex, col = "#A71930", xlab = "AcidIndex", 
     main = "Histogram of AcidIndex")
hist(completed.wine$STARS, col = "#09ADAD", xlab = "STARS", 
     main = "Histogram of STARS")


##### Model Development #####

# Multiple Linear Regression

# model.1 - manual variable selection
# Fit Linear Regression model
model.1 <- lm(TARGET ~ VolatileAcidity + LabelAppeal + AcidIndex + 
                STARS, data=completed.wine)

# Display model summary
summary(model.1)

completed.wine$model.1Prediction <- predict(model.1, type = "response")

coefficients(model.1)
completed.wine$fittedLM <- fitted(model.1)

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
plot(completed.wine$VolatileAcidity,model.1$residuals,xlab='Predictor: VolatileAcidity',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$LabelAppeal,model.1$residuals,xlab='Predictor: LabelAppeal',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$AcidIndex,model.1$residuals,xlab='Predictor: AcidIndex',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$STARS,model.1$residuals,xlab='Predictor: STARS',
     ylab='Residual')
title('Residual vs Predictor')


# model.2 - stepwise (both directions) variable selection
model.2 <- lm(formula = TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + 
                ResidualSugar + Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + 
                Density + pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, 
              data = completed.wine, na.action=na.exclude)
stepwise.model.2 <- stepAIC(model.2, direction = "both")

# Display model summary
summary(stepwise.model.2)

completed.wine$model.2Prediction <- predict(stepwise.model.2, type = "response")

coefficients(stepwise.model.2)
completed.wine$fittedLM <- fitted(stepwise.model.2)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.2 <- mean(stepwise.model.2$residuals^2)
mae.2 <- mean(abs(stepwise.model.2$residuals))
mse.2
mae.2

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(stepwise.model.2$residuals)
qqline(stepwise.model.2$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$VolatileAcidity,stepwise.model.2$residuals,xlab='Predictor: VolatileAcidity',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$CitricAcid,stepwise.model.2$residuals,xlab='Predictor: CitricAcid',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Chlorides,stepwise.model.2$residuals,xlab='Predictor: Chlorides',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$FreeSulfurDioxide,stepwise.model.2$residuals,xlab='Predictor: FreeSulfurDioxide',
     ylab='Residual')
title('Residual vs Predictor')

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$TotalSulfurDioxide,stepwise.model.2$residuals,xlab='Predictor: TotalSulfurDioxide',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Density,stepwise.model.2$residuals,xlab='Predictor: Density',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$pH,stepwise.model.2$residuals,xlab='Predictor: pH',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Sulphates,stepwise.model.2$residuals,xlab='Predictor: Sulphates',
     ylab='Residual')
title('Residual vs Predictor')

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$Alcohol,stepwise.model.2$residuals,xlab='Predictor: Alcohol',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$LabelAppeal,stepwise.model.2$residuals,xlab='Predictor: LabelAppeal',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$AcidIndex,stepwise.model.2$residuals,xlab='Predictor: AcidIndex',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$STARS,stepwise.model.2$residuals,xlab='Predictor: STARS',
     ylab='Residual')
title('Residual vs Predictor')


# Poisson Regression

# model.3 - Poisson with manually-selected variables
model.3 <- glm(TARGET ~ VolatileAcidity + LabelAppeal + AcidIndex + 
                 STARS, family="poisson"(link="log"), data=completed.wine)

# Display model summary
summary(model.3)

completed.wine$model.3Prediction <- predict(model.3, type = "response")

coefficients(model.3)
completed.wine$fittedLM <- fitted(model.3)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.3 <- mean(model.3$residuals^2)
mae.3 <- mean(abs(model.3$residuals))
mse.3
mae.3

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.3$residuals)
qqline(model.3$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$VolatileAcidity,model.3$residuals,xlab='Predictor: VolatileAcidity',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$LabelAppeal,model.3$residuals,xlab='Predictor: LabelAppeal',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$AcidIndex,model.3$residuals,xlab='Predictor: AcidIndex',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$STARS,model.3$residuals,xlab='Predictor: STARS',
     ylab='Residual')
title('Residual vs Predictor')

# model.4 - Poisson with stepwise-selected variables
model.4 <- glm(TARGET ~ VolatileAcidity + CitricAcid + Chlorides + FreeSulfurDioxide + 
                 TotalSulfurDioxide + Density + pH + Sulphates + Alcohol + LabelAppeal + 
                 AcidIndex + STARS, family="poisson"(link="log"), data=completed.wine)

# Display model summary
summary(model.4)

completed.wine$model.4Prediction <- predict(model.4, type = "response")

coefficients(model.4)
completed.wine$fittedLM <- fitted(model.4)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.4 <- mean(model.4$residuals^2)
mae.4 <- mean(abs(model.4$residuals))
mse.4
mae.4

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.4$residuals)
qqline(model.4$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$VolatileAcidity,model.4$residuals,xlab='Predictor: VolatileAcidity',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$CitricAcid,model.4$residuals,xlab='Predictor: CitricAcid',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Chlorides,model.4$residuals,xlab='Predictor: Chlorides',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$FreeSulfurDioxide,model.4$residuals,xlab='Predictor: FreeSulfurDioxide',
     ylab='Residual')
title('Residual vs Predictor')

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$TotalSulfurDioxide,model.4$residuals,xlab='Predictor: TotalSulfurDioxide',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Density,model.4$residuals,xlab='Predictor: Density',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$pH,model.4$residuals,xlab='Predictor: pH',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Sulphates,model.4$residuals,xlab='Predictor: Sulphates',
     ylab='Residual')
title('Residual vs Predictor')

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$Alcohol,model.4$residuals,xlab='Predictor: Alcohol',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$LabelAppeal,model.4$residuals,xlab='Predictor: LabelAppeal',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$AcidIndex,model.4$residuals,xlab='Predictor: AcidIndex',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$STARS,model.4$residuals,xlab='Predictor: STARS',
     ylab='Residual')
title('Residual vs Predictor')


# Zero-Inflated Poisson Regression

# model.5 - Zero-Inflated Poisson with with manually-selected variables
model.5 <- zeroinfl(TARGET ~ VolatileAcidity + LabelAppeal + AcidIndex + 
                      STARS, data=completed.wine)

# Display model summary
summary(model.5)

completed.wine$ZIPphat5 <- predict(model.5, newdata=completed.wine, type="response")

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.5 <- mean(model.5$residuals^2)
mae.5 <- mean(abs(model.5$residuals))
mse.5
mae.5

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.5$residuals)
qqline(model.5$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$VolatileAcidity,model.5$residuals,xlab='Predictor: VolatileAcidity',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$LabelAppeal,model.5$residuals,xlab='Predictor: LabelAppeal',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$AcidIndex,model.5$residuals,xlab='Predictor: AcidIndex',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$STARS,model.5$residuals,xlab='Predictor: STARS',
     ylab='Residual')
title('Residual vs Predictor')

# model.6 - Zero-Inflated Poisson with with stepwise-selected variables
model.6 <- zeroinfl(TARGET ~ VolatileAcidity + CitricAcid + Chlorides + 
                      FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                      pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, 
                    data=completed.wine)

# Display model summary
summary(model.6)

completed.wine$ZIPphat6 <- predict(model.6, newdata=completed.wine, type="response")

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.6 <- mean(model.6$residuals^2)
mae.6 <- mean(abs(model.6$residuals))
mse.6
mae.6

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.6$residuals)
qqline(model.6$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$VolatileAcidity,model.6$residuals,xlab='Predictor: VolatileAcidity',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$CitricAcid,model.6$residuals,xlab='Predictor: CitricAcid',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Chlorides,model.6$residuals,xlab='Predictor: Chlorides',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$FreeSulfurDioxide,model.6$residuals,xlab='Predictor: FreeSulfurDioxide',
     ylab='Residual')
title('Residual vs Predictor')

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$TotalSulfurDioxide,model.6$residuals,xlab='Predictor: TotalSulfurDioxide',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Density,model.6$residuals,xlab='Predictor: Density',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$pH,model.6$residuals,xlab='Predictor: pH',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Sulphates,model.6$residuals,xlab='Predictor: Sulphates',
     ylab='Residual')
title('Residual vs Predictor')

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$Alcohol,model.6$residuals,xlab='Predictor: Alcohol',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$LabelAppeal,model.6$residuals,xlab='Predictor: LabelAppeal',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$AcidIndex,model.6$residuals,xlab='Predictor: AcidIndex',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$STARS,model.6$residuals,xlab='Predictor: STARS',
     ylab='Residual')
title('Residual vs Predictor')


# Negative Binomial Regression

# model.7 - Negative Binomial with with manually-selected variables
model.7 <- glm.nb(TARGET ~ VolatileAcidity + LabelAppeal + AcidIndex + STARS, 
                  data=completed.wine)

# Display model summary
summary(model.7)

completed.wine$NBRphat7 <- predict(model.7, newdata=completed.wine, type="response")

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.7 <- mean(model.7$residuals^2)
mae.7 <- mean(abs(model.7$residuals))
mse.7
mae.7

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.7$residuals)
qqline(model.7$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$VolatileAcidity,model.7$residuals,xlab='Predictor: VolatileAcidity',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$LabelAppeal,model.7$residuals,xlab='Predictor: LabelAppeal',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$AcidIndex,model.7$residuals,xlab='Predictor: AcidIndex',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$STARS,model.7$residuals,xlab='Predictor: STARS',
     ylab='Residual')
title('Residual vs Predictor')

# model.8 - Negative Binomial with with stepwise-selected variables
model.8 <- glm.nb(TARGET ~ VolatileAcidity + CitricAcid + Chlorides + 
                    FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                    pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS, 
                  data=completed.wine)

# Display model summary
summary(model.8)

completed.wine$NBRphat8 <- predict(model.8, newdata=completed.wine, type="response")

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.8 <- mean(model.8$residuals^2)
mae.8 <- mean(abs(model.8$residuals))
mse.8
mae.8

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.8$residuals)
qqline(model.8$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$VolatileAcidity,model.8$residuals,xlab='Predictor: VolatileAcidity',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$CitricAcid,model.8$residuals,xlab='Predictor: CitricAcid',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Chlorides,model.8$residuals,xlab='Predictor: Chlorides',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$FreeSulfurDioxide,model.8$residuals,xlab='Predictor: FreeSulfurDioxide',
     ylab='Residual')
title('Residual vs Predictor')

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$TotalSulfurDioxide,model.8$residuals,xlab='Predictor: TotalSulfurDioxide',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Density,model.8$residuals,xlab='Predictor: Density',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$pH,model.8$residuals,xlab='Predictor: pH',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Sulphates,model.8$residuals,xlab='Predictor: Sulphates',
     ylab='Residual')
title('Residual vs Predictor')

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$Alcohol,model.8$residuals,xlab='Predictor: Alcohol',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$LabelAppeal,model.8$residuals,xlab='Predictor: LabelAppeal',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$AcidIndex,model.8$residuals,xlab='Predictor: AcidIndex',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$STARS,model.8$residuals,xlab='Predictor: STARS',
     ylab='Residual')
title('Residual vs Predictor')


# Zero-Inflated Negative Binomial Regression

# model.9 - Zero-Inflated Negative Binomial with manually-selected variables
model.9 <- zeroinfl(TARGET ~ VolatileAcidity + LabelAppeal + AcidIndex + STARS,
                    data=completed.wine, dist="negbin", EM=TRUE)

# Display model summary
summary(model.9)

completed.wine$ZINBphat9 <- predict(model.9, newdata=completed.wine, type="response")

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.9 <- mean(model.9$residuals^2)
mae.9 <- mean(abs(model.9$residuals))
mse.9
mae.9

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.9$residuals)
qqline(model.9$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$VolatileAcidity,model.9$residuals,xlab='Predictor: VolatileAcidity',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$LabelAppeal,model.9$residuals,xlab='Predictor: LabelAppeal',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$AcidIndex,model.9$residuals,xlab='Predictor: AcidIndex',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$STARS,model.9$residuals,xlab='Predictor: STARS',
     ylab='Residual')
title('Residual vs Predictor')

# model.10 - Zero-Inflated Negative Binomial with stepwise-selected variables
model.10 <- zeroinfl(TARGET ~ VolatileAcidity + CitricAcid + Chlorides + 
                       FreeSulfurDioxide + TotalSulfurDioxide + Density + 
                       pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS,
                     data=completed.wine, dist="negbin", EM=TRUE)

# Display model summary
summary(model.10)

completed.wine$ZINBphat10 <- predict(model.10, newdata=completed.wine, type="response")

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.10 <- mean(model.10$residuals^2)
mae.10 <- mean(abs(model.10$residuals))
mse.10
mae.10

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.10$residuals)
qqline(model.10$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$VolatileAcidity,model.10$residuals,xlab='Predictor: VolatileAcidity',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$CitricAcid,model.10$residuals,xlab='Predictor: CitricAcid',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Chlorides,model.10$residuals,xlab='Predictor: Chlorides',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$FreeSulfurDioxide,model.10$residuals,xlab='Predictor: FreeSulfurDioxide',
     ylab='Residual')
title('Residual vs Predictor')

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$TotalSulfurDioxide,model.10$residuals,xlab='Predictor: TotalSulfurDioxide',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Density,model.10$residuals,xlab='Predictor: Density',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$pH,model.10$residuals,xlab='Predictor: pH',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$Sulphates,model.10$residuals,xlab='Predictor: Sulphates',
     ylab='Residual')
title('Residual vs Predictor')

# Make a scatterplot
par(mfrow = c(2, 2))
plot(completed.wine$Alcohol,model.10$residuals,xlab='Predictor: Alcohol',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$LabelAppeal,model.10$residuals,xlab='Predictor: LabelAppeal',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$AcidIndex,model.10$residuals,xlab='Predictor: AcidIndex',
     ylab='Residual')
title('Residual vs Predictor')
plot(completed.wine$STARS,model.10$residuals,xlab='Predictor: STARS',
     ylab='Residual')
title('Residual vs Predictor')


# Model Comparison using AIC and BIC

# AIC calculations
aic.model.1 <- AIC(model.1)
aic.model.1

aic.stepwise.model.2 <- AIC(stepwise.model.2)
aic.stepwise.model.2

aic.model.3 <- AIC(model.3)
aic.model.3

aic.model.4 <- AIC(model.4)
aic.model.4

aic.model.5 <- AIC(model.5)
aic.model.5

aic.model.6 <- AIC(model.6)
aic.model.6

aic.model.7 <- AIC(model.7)
aic.model.7

aic.model.8 <- AIC(model.8)
aic.model.8

aic.model.9 <- AIC(model.9)
aic.model.9

aic.model.10 <- AIC(model.10)
aic.model.10


# Calculate Log Likelhood for each model
ll.model.1 <- print(-2*logLik(model.1, REML = TRUE))
ll.model.1

ll.stepwise.model.2 <- print(-2*logLik(stepwise.model.2, REML = TRUE))
ll.stepwise.model.2

ll.model.3 <- print(-2*logLik(model.3, REML = TRUE))
ll.model.3

ll.model.4 <- print(-2*logLik(model.4, REML = TRUE))
ll.model.4

ll.model.5 <- print(-2*logLik(model.5, REML = TRUE))
ll.model.5

ll.model.6 <- print(-2*logLik(model.6, REML = TRUE))
ll.model.6

ll.model.7 <- print(-2*logLik(model.7, REML = TRUE))
ll.model.7

ll.model.8 <- print(-2*logLik(model.8, REML = TRUE))
ll.model.8

ll.model.9 <- print(-2*logLik(model.9, REML = TRUE))
ll.model.9

ll.model.10 <- print(-2*logLik(model.10, REML = TRUE))
ll.model.10


mse.1 <- mean(model.1$residuals^2)
mae.1 <- mean(abs(model.1$residuals))
mse.1
mae.1

mse.2 <- mean(stepwise.model.2$residuals^2)
mae.2 <- mean(abs(stepwise.model.2$residuals))
mse.2
mae.2

mse.3 <- mean(model.3$residuals^2)
mae.3 <- mean(abs(model.3$residuals))
mse.3
mae.3

mse.4 <- mean(model.4$residuals^2)
mae.4 <- mean(abs(model.4$residuals))
mse.4
mae.4

mse.5 <- mean(model.5$residuals^2)
mae.5 <- mean(abs(model.5$residuals))
mse.5
mae.5

mse.6 <- mean(model.6$residuals^2)
mae.6 <- mean(abs(model.6$residuals))
mse.6
mae.6

mse.7 <- mean(model.7$residuals^2)
mae.7 <- mean(abs(model.7$residuals))
mse.7
mae.7

mse.8 <- mean(model.8$residuals^2)
mae.8 <- mean(abs(model.8$residuals))
mse.8
mae.8

mse.9 <- mean(model.9$residuals^2)
mae.9 <- mean(abs(model.9$residuals))
mse.9
mae.9

mse.10 <- mean(model.10$residuals^2)
mae.10 <- mean(abs(model.10$residuals))
mse.10
mae.10


##### Model Scoring of Test Data #####
# Read in the csv file into an R data frame
path <- '/Users/derekhigham/Documents/school/MSDS 411/Unit 3/'
file <- paste(path, 'Wine_Test.csv', sep='')
test.wine <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)

head(test.wine)
str(test.wine)

# to address invalid negative values for chemistry variables,
# we first tranformed any negative values to NA (null) values
test.wine$FixedAcidity[test.wine$FixedAcidity < 0] <- NA
test.wine$VolatileAcidity[test.wine$VolatileAcidity < 0] <- NA
test.wine$CitricAcid[test.wine$CitricAcid < 0] <- NA
test.wine$ResidualSugar[test.wine$ResidualSugar < 0] <- NA
test.wine$Chlorides[test.wine$Chlorides < 0] <- NA
test.wine$FreeSulfurDioxide[test.wine$FreeSulfurDioxide < 0] <- NA
test.wine$TotalSulfurDioxide[test.wine$TotalSulfurDioxide < 0] <- NA
test.wine$Sulphates[test.wine$Sulphates < 0] <- NA
test.wine$Alcohol[test.wine$Alcohol < 0] <- NA

# check if the above transformations worked
summary(test.wine)

# now that negative values have been transformed to NA values,
# we will use predictive mean-matching and the mice function to
# impute missing values across the data set

impute.test.wine <- mice(data=test.wine, method="pmm", seed=300)
summary(impute.test.wine)

# summary of imputed dataset - include in write-up
completed.test.wine <- complete(impute.test.wine,1)

# round LabelAppeal and STARS variables to nearest whole number to comply
# with the intent of the variable structure
completed.test.wine$STARS <- round(completed.test.wine$STARS, digits = 0)

summary(completed.test.wine)

# write imputed (final) dataset to local drive
write.csv(completed.test.wine, '/Users/derekhigham/Documents/school/MSDS 411/Unit 3/completed_test_wine.csv')

# Stand Alone Scoring Process for TARGET using model.10
completed.test.wine$P_TARGET <- -1.9874947 + 
  0.3124926*completed.test.wine$VolatileAcidity -
  0.1365338*completed.test.wine$CitricAcid +
  0.3668048*completed.test.wine$Chlorides -
  0.0009071*completed.test.wine$FreeSulfurDioxide -
  0.0013142*completed.test.wine$TotalSulfurDioxide +
  0.4551938*completed.test.wine$Density + 
  0.1661724*completed.test.wine$pH + 
  0.1357568*completed.test.wine$Sulphates +
  0.0241385*completed.test.wine$Alcohol +
  0.6978938*completed.test.wine$LabelAppeal + 
  0.4412874*completed.test.wine$AcidIndex - 
  3.0277298*completed.test.wine$STARS

#subset of data set for the deliverable "Scored data file"
prediction_2 <- completed.test.wine[c("INDEX","P_TARGET")]

# Write prediction data set to directory
write.csv(prediction_2, '/Users/derekhigham/Documents/school/MSDS 411/Unit 3/prediction_2.csv')


