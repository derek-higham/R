# Derek Higham
# Logistic & Probit Regression

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


##### Read in the csv file into an R data frame #####
path <- '/Users/derekhigham/Documents/school/MSDS 411/Unit 2/'
file <- paste(path, 'logit_insurance.csv', sep='')
data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)

head(data)
str(data)


##### Transform $ values to numeric values #####
data$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$INCOME)))
data$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$OLDCLAIM)))
data$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$BLUEBOOK)))


###### Impute missing data items for AGE, CAR_AGE, HOME_VAL, INCOME, JOB, and YOJ
data$JOB[is.na(data$JOB)] <- median(data$JOB, na.rm=TRUE)
data$AGE[is.na(data$AGE)] <- mean(data$AGE, na.rm=TRUE)
data$YOJ <- na.aggregate(data$YOJ, data$JOB, mean, na.rm=TRUE)
data$INCOME <- na.aggregate(data$INCOME, data$JOB, mean, na.rm=TRUE)
data$HOME_VAL <- na.aggregate(data$HOME_VAL, data$JOB, mean, na.rm=TRUE)
data$CAR_AGE <- na.aggregate(data$CAR_AGE, data$CAR_TYPE, mean, na.rm=TRUE)


##### Data transformations #####
# Due to the discrete nature of a number of variables, we may want to explore the 
# transformation of existing variables and the creation of new variables 

# Read in factor variables as factors
data$TARGET_FLAG <- as.factor(data$TARGET_FLAG)
data$PARENT1 <- as.factor(ifelse(data$PARENT1=="Yes", 1, 0))
data$MSTATUS <- as.factor(ifelse(data$MSTATUS=="Yes", 1, 0))
data$RED_CAR <- as.factor(ifelse(data$RED_CAR=="yes", 1, 0))
data$REVOKED <- as.factor(ifelse(data$REVOKED=="Yes", 1, 0))
data$DO_KIDS_DRIVE <- as.factor(ifelse(data$KIDSDRIV > 0, 1, 0))

# SEX transformation
data$MALE_SEX <- as.factor(ifelse(data$SEX=="M", 1, 0))
data$FEMALE_SEX <- as.factor(ifelse(data$SEX=="z_F", 1, 0))

# EDUCATION transformation
data$L_HIGH_SCHOOL <- as.factor(ifelse(data$EDUCATION=="<High School", 1, 0))
data$HIGH_SCHOOL <- as.factor(ifelse(data$EDUCATION=="z_High School", 1, 0))
data$BACHELORS <- as.factor(ifelse(data$EDUCATION=="Bachelors", 1, 0))
data$MASTERS <- as.factor(ifelse(data$EDUCATION=="Masters", 1, 0))
data$PHD <- as.factor(ifelse(data$EDUCATION=="PhD", 1, 0))

# JOB transformation
data$CLERICAL <- as.factor(ifelse(data$JOB=="Clerical", 1, 0))
data$DOCTOR <- as.factor(ifelse(data$JOB=="Doctor", 1, 0))
data$HOME_MAKER <- as.factor(ifelse(data$JOB=="Home Maker", 1, 0))
data$LAWYER <- as.factor(ifelse(data$JOB=="Lawyer", 1, 0))
data$MANAGER <- as.factor(ifelse(data$JOB=="Manager", 1, 0))
data$PROFESSIONAL <- as.factor(ifelse(data$JOB=="Professional", 1, 0))
data$STUDENT <- as.factor(ifelse(data$JOB=="Student", 1, 0))
data$BLUE_COLLAR <- as.factor(ifelse(data$JOB=="z_Blue Collar", 1, 0))

# CAR_USE transformation
data$COMMERCIAL_USE <- as.factor(ifelse(data$CAR_USE=="Commercial", 1, 0))
data$PRIVATE_USE <- as.factor(ifelse(data$CAR_USE=="Private", 1, 0))

# CAR_TYPE transformation
data$MINIVAN <- as.factor(ifelse(data$CAR_TYPE=="Minivan", 1, 0))
data$PANEL_TRUCK <- as.factor(ifelse(data$CAR_TYPE=="Panel Truck", 1, 0))
data$PICKUP <- as.factor(ifelse(data$CAR_TYPE=="Pickup", 1, 0))
data$SPORTS_CAR <- as.factor(ifelse(data$CAR_TYPE=="Sports Car", 1, 0))
data$VAN <- as.factor(ifelse(data$CAR_TYPE=="Van", 1, 0))
data$SUV <- as.factor(ifelse(data$CAR_TYPE=="z_SUV", 1, 0))

# URBANICITY transformation
data$URBAN <- as.factor(ifelse(data$URBANICITY=="Highly Urban/ Urban", 1, 0))
data$RURAL <- as.factor(ifelse(data$URBANICITY=="z_Highly Rural/ Rural", 1, 0))

# INCOME bucketing
data$INCOME_BIN[data$INCOME >= 0 & data$INCOME < 30000] <- "LOW"
data$INCOME_BIN[data$INCOME >= 30000 & data$INCOME < 50000] <- "LOWER-MIDDLE"
data$INCOME_BIN[data$INCOME >= 50000 & data$INCOME < 100000] <- "MIDDLE"
data$INCOME_BIN[data$INCOME >= 100000 & data$INCOME < 350000] <- "UPPER-MIDDLE"
data$INCOME_BIN[data$INCOME >= 350000] <- "UPPER"

# Remove redundant/unnecessary variables from data set
data$KIDSDRIV <- NULL
data$SEX <- NULL
data$EDUCATION <- NULL
data$JOB <- NULL
data$CAR_USE <- NULL
data$CAR_TYPE <- NULL
data$URBANICITY <- NULL

# Check data set
str(data)

# Write completed training data set to local directory
write.csv(data, '/Users/derekhigham/Documents/school/MSDS 411/Unit 2/data.csv')


##### Exploratory Data Analysis #####

# Create Histogram and Boxplot pairings for each variable to include in the appendix.
# Wins - Use lower bound for lower outliers, upper bound for higher outliers.

# TARGET_FLAG & TARGET_AMT
par(mfrow=c(2,2))
hist(data$TARGET_FLAG, col = "#A71930", xlab = "TARGET_FLAG", main = "Histogram of TARGET_FLAG")
hist(data$TARGET_AMT, col = "#09ADAD", xlab = "TARGET_AMT", main = "Histogram of TARGET_AMT")
boxplot(data$TARGET_FLAG, col = "#A71930", main = "Boxplot of TARGET_FLAG")
boxplot(data$TARGET_AMT, col = "#09ADAD", main = "Boxplot of TARGET_AMT")

# AGE & BLUEBOOK
par(mfrow=c(2,2))
hist(data$AGE, col = "#A71930", xlab = "AGE", main = "Histogram of AGE")
hist(data$BLUEBOOK, col = "#09ADAD", xlab = "BLUEBOOK", main = "Histogram of BLUEBOOK")
boxplot(data$AGE, col = "#A71930", main = "Boxplot of AGE")
boxplot(data$BLUEBOOK, col = "#09ADAD", main = "Boxplot of BLUEBOOK")

# CAR_AGE & CLM_FREQ
par(mfrow=c(2,2))
hist(data$CAR_AGE, col = "#A71930", xlab = "CAR_AGE", main = "Histogram of CAR_AGE")
hist(data$CLM_FREQ, col = "#09ADAD", xlab = "CLM_FREQ", main = "Histogram of CLM_FREQ")
boxplot(data$CAR_AGE, col = "#A71930", main = "Boxplot of CAR_AGE")
boxplot(data$CLM_FREQ, col = "#09ADAD", main = "Boxplot of CLM_FREQ")

# INCOME & INCOME_BIN
par(mfrow=c(1,2))
hist(data$INCOME, col = "#A71930", xlab = "INCOME", main = "Histogram of INCOME")
boxplot(data$INCOME, col = "#A71930", main = "Boxplot of INCOME")

# MALE_SEX & FEMALE_SEX
par(mfrow=c(2,2))
hist(data$MALE_SEX, col = "#A71930", xlab = "MALE_SEX", main = "Histogram of MALE_SEX")
hist(data$FEMALE_SEX, col = "#09ADAD", xlab = "FEMALE_SEX", main = "Histogram of FEMALE_SEX")
boxplot(data$MALE_SEX, col = "#A71930", main = "Boxplot of MALE_SEX")
boxplot(data$FEMALE_SEX, col = "#09ADAD", main = "Boxplot of FEMALE_SEX")

# HOME_VAL & DO_KIDS_DRIVE
par(mfrow=c(1,2))
hist(data$HOME_VAL, col = "#A71930", xlab = "HOME_VAL", main = "Histogram of HOME_VAL")
boxplot(data$HOME_VAL, col = "#A71930", main = "Boxplot of HOME_VAL")

# L_HIGH_SCHOOL & HIGH_SCHOOL
par(mfrow=c(2,2))
hist(data$L_HIGH_SCHOOL, col = "#A71930", xlab = "L_HIGH_SCHOOL", main = "Histogram of L_HIGH_SCHOOL")
hist(data$HIGH_SCHOOL, col = "#09ADAD", xlab = "HIGH_SCHOOL", main = "Histogram of HIGH_SCHOOL")
boxplot(data$L_HIGH_SCHOOL, col = "#A71930", main = "Boxplot of L_HIGH_SCHOOL")
boxplot(data$HIGH_SCHOOL, col = "#09ADAD", main = "Boxplot of HIGH_SCHOOL")

# BACHELORS & MASTERS
par(mfrow=c(2,2))
hist(data$BACHELORS, col = "#A71930", xlab = "BACHELORS", main = "Histogram of BACHELORS")
hist(data$MASTERS, col = "#09ADAD", xlab = "MASTERS", main = "Histogram of MASTERS")
boxplot(data$BACHELORS, col = "#A71930", main = "Boxplot of BACHELORS")
boxplot(data$MASTERS, col = "#09ADAD", main = "Boxplot of MASTERS")

# PHD & CLERICAL
par(mfrow=c(2,2))
hist(data$PHD, col = "#A71930", xlab = "PHD", main = "Histogram of PHD")
hist(data$CLERICAL, col = "#09ADAD", xlab = "CLERICAL", main = "Histogram of CLERICAL")
boxplot(data$PHD, col = "#A71930", main = "Boxplot of PHD")
boxplot(data$CLERICAL, col = "#09ADAD", main = "Boxplot of CLERICAL")

# DOCTOR & HOME_MAKER
par(mfrow=c(2,2))
hist(data$DOCTOR, col = "#A71930", xlab = "DOCTOR", main = "Histogram of DOCTOR")
hist(data$HOME_MAKER, col = "#09ADAD", xlab = "HOME_MAKER", main = "Histogram of HOME_MAKER")
boxplot(data$DOCTOR, col = "#A71930", main = "Boxplot of DOCTOR")
boxplot(data$HOME_MAKER, col = "#09ADAD", main = "Boxplot of HOME_MAKER")

# LAWYER & MANAGER
par(mfrow=c(2,2))
hist(data$LAWYER, col = "#A71930", xlab = "LAWYER", main = "Histogram of LAWYER")
hist(data$MANAGER, col = "#09ADAD", xlab = "MANAGER", main = "Histogram of MANAGER")
boxplot(data$LAWYER, col = "#A71930", main = "Boxplot of LAWYER")
boxplot(data$MANAGER, col = "#09ADAD", main = "Boxplot of MANAGER")

# PROFESSIONAL & TIF
par(mfrow=c(1,2))
hist(data$TIF, col = "#09ADAD", xlab = "TIF", main = "Histogram of TIF")
boxplot(data$TIF, col = "#09ADAD", main = "Boxplot of TIF")

# MSTATUS & MVR_PTS
par(mfrow=c(1,2))
hist(data$MVR_PTS, col = "#09ADAD", xlab = "MVR_PTS", main = "Histogram of MVR_PTS")
boxplot(data$MVR_PTS, col = "#09ADAD", main = "Boxplot of MVR_PTS")

# OLDCLAIM & PARENT1
par(mfrow=c(1,2))
hist(data$OLDCLAIM, col = "#A71930", xlab = "OLDCLAIM", main = "Histogram of OLDCLAIM")
boxplot(data$OLDCLAIM, col = "#A71930", main = "Boxplot of OLDCLAIM")

# RED_CAR & REVOKED
par(mfrow=c(2,2))
hist(cdata$RED_CAR, col = "#A71930", xlab = "RED_CAR", main = "Histogram of RED_CAR")
hist(data$REVOKED, col = "#09ADAD", xlab = "REVOKED", main = "Histogram of TREVOKED")
boxplot(data$RED_CAR, col = "#A71930", main = "Boxplot of RED_CAR")
boxplot(data$REVOKED, col = "#09ADAD", main = "Boxplot of REVOKED")

# COMMERCIAL_USE & PRIVATE_USE
par(mfrow=c(2,2))
hist(data$COMMERCIAL_USE, col = "#A71930", xlab = "COMMERCIAL_USE", main = "Histogram of COMMERCIAL_USE")
hist(data$PRIVATE_USE, col = "#09ADAD", xlab = "PRIVATE_USE", main = "Histogram of PRIVATE_USE")
boxplot(data$COMMERCIAL_USE, col = "#A71930", main = "Boxplot of COMMERCIAL_USE")
boxplot(data$PRIVATE_USE, col = "#09ADAD", main = "Boxplot of PRIVATE_USE")

# TRAVTIME & YOJ
par(mfrow=c(2,2))
hist(data$TRAVTIME, col = "#A71930", xlab = "TRAVTIME", main = "Histogram of TRAVTIME")
hist(data$YOJ, col = "#A71930", xlab = "YOJ", main = "Histogram of YOJ")
boxplot(data$TRAVTIME, col = "#A71930", main = "Boxplot of TRAVTIME")
boxplot(data$YOJ, col = "#A71930", main = "Boxplot of YOJ")

# MINIVAN & PANEL_TRUCK
par(mfrow=c(2,2))
hist(data$MINIVAN, col = "#A71930", xlab = "MINIVAN", main = "Histogram of MINIVAN")
hist(data$PANEL_TRUCK, col = "#A71930", xlab = "PANEL_TRUCK", main = "Histogram of PANEL_TRUCK")
boxplot(data$MINIVAN, col = "#A71930", main = "Boxplot of MINIVAN")
boxplot(data$PANEL_TRUCK, col = "#A71930", main = "Boxplot of PANEL_TRUCK")

# PICKUP & SPORTS_CAR
par(mfrow=c(2,2))
hist(data$PICKUP, col = "#A71930", xlab = "PICKUP", main = "Histogram of PICKUP")
hist(data$SPORTS_CAR, col = "#A71930", xlab = "SPORTS_CAR", main = "Histogram of SPORTS_CAR")
boxplot(data$PICKUP, col = "#A71930", main = "Boxplot of PICKUP")
boxplot(data$SPORTS_CAR, col = "#A71930", main = "Boxplot of SPORTS_CAR")

# VAN & SUV
par(mfrow=c(2,2))
hist(data$VAN, col = "#A71930", xlab = "VAN", main = "Histogram of VAN")
hist(data$SUV, col = "#A71930", xlab = "SUV", main = "Histogram of SUV")
boxplot(data$VAN, col = "#A71930", main = "Boxplot of VAN")
boxplot(data$SUV, col = "#A71930", main = "Boxplot of SUV")

# URBAN & RURAL
par(mfrow=c(2,2))
hist(data$URBAN, col = "#A71930", xlab = "URBAN", main = "Histogram of URBAN")
hist(data$RURAL, col = "#A71930", xlab = "RURAL", main = "Histogram of RURAL")
boxplot(data$URBAN, col = "#A71930", main = "Boxplot of URBAN")
boxplot(data$RURAL, col = "#A71930", main = "Boxplot of RURAL")

# HOMEKIDS
par(mfrow=c(1,2))
hist(data$HOMEKIDS, col = "#A71930", xlab = "HOMEKIDS", main = "Histogram of HOMEKIDS")
boxplot(data$HOMEKIDS, col = "#A71930", main = "Boxplot of HOMEKIDS")


# Create scatterplots of each predictor variable against TARGET_FLAG to identify
# potential candidate predictors
par(mfrow = c(2, 2))
plot(data$AGE,data$TARGET_FLAG,
     xlab='AGE',ylab='TARGET_FLAG',main='AGE & TARGET_FLAG')
plot(data$BLUEBOOK,data$TARGET_FLAG,
     xlab='BLUEBOOK',ylab='TARGET_FLAG',main='BLUEBOOK & TARGET_FLAG')
plot(data$CAR_AGE,data$TARGET_FLAG,
     xlab='CAR_AGE',ylab='TARGET_FLAG',main='CAR_AGE & TARGET_FLAG')
plot(data$CLM_FREQ,data$TARGET_FLAG,
     xlab='CLM_FREQ',ylab='TARGET_FLAG',main='CLM_FREQ & TARGET_FLAG')

par(mfrow = c(2, 2))
plot(data$INCOME,data$TARGET_FLAG,
     xlab='INCOME',ylab='TARGET_FLAG',main='INCOME & TARGET_FLAG')
plot(data$INCOME_BIN,data$TARGET_FLAG,
     xlab='INCOME_BIN',ylab='TARGET_FLAG',main='INCOME_BIN & TARGET_FLAG')
plot(data$MALE_SEX,data$TARGET_FLAG,
     xlab='MALE_SEX',ylab='TARGET_FLAG',main='MALE & TARGET_FLAG')
plot(data$FEMALE_SEX,data$TARGET_FLAG,
     xlab='FEMALE_SEX',ylab='TARGET_FLAG',main='FEMALE & TARGET_FLAG')

par(mfrow = c(2, 2))
plot(data$HOME_VAL,data$TARGET_FLAG,
     xlab='HOME_VAL',ylab='TARGET_FLAG',main='HOME_VAL & TARGET_FLAG')
plot(data$DO_KIDS_DRIVE,data$TARGET_FLAG,
     xlab='DO_KIDS_DRIVE',ylab='TARGET_FLAG',main='DO_KIDS_DRIVE & TARGET_FLAG')
plot(data$L_HIGH_SCHOOL,data$TARGET_FLAG,
     xlab='L_HIGH_SCHOOL',ylab='TARGET_FLAG',main='L_HIGH_SCHOOL & TARGET_FLAG')
plot(data$HIGH_SCHOOL,data$TARGET_FLAG,
     xlab='HIGH_SCHOOL',ylab='TARGET_FLAG',main='HIGH_SCHOOL & TARGET_FLAG')

par(mfrow = c(2, 2))
plot(data$BACHELORS,data$TARGET_FLAG,
     xlab='BACHELORS',ylab='TARGET_FLAG',main='BACHELORS & TARGET_FLAG')
plot(data$MASTERS,data$TARGET_FLAG,
     xlab='MASTERS',ylab='TARGET_FLAG',main='MASTERS & TARGET_FLAG')
plot(data$PHD,data$TARGET_FLAG,
     xlab='PHD',ylab='TARGET_FLAG',main='PHD & TARGET_FLAG')
plot(data$CLERICAL,data$TARGET_FLAG,
     xlab='CLERICAL',ylab='TARGET_FLAG',main='CLERICAL & TARGET_FLAG')

par(mfrow = c(2, 2))
plot(data$DOCTOR,data$TARGET_FLAG,
     xlab='DOCTOR',ylab='TARGET_FLAG',main='DOCTOR & TARGET_FLAG')
plot(data$HOME_MAKER,data$TARGET_FLAG,
     xlab='HOME_MAKER',ylab='TARGET_FLAG',main='HOME_MAKER & TARGET_FLAG')
plot(data$LAWYER,data$TARGET_FLAG,
     xlab='LAWYER',ylab='TARGET_FLAG',main='LAWYER & TARGET_FLAG')
plot(data$MANAGER,data$TARGET_FLAG,
     xlab='MANAGER',ylab='TARGET_FLAG',main='MANAGER & TARGET_FLAG')

par(mfrow = c(2, 2))
plot(data$PROFESSIONAL,data$TARGET_FLAG,
     xlab='PROFESSIONAL',ylab='TARGET_FLAG',main='PROFESSIONAL & TARGET_FLAG')
plot(data$TIF,data$TARGET_FLAG,
     xlab='TIF',ylab='TARGET_FLAG',main='TIF & TARGET_FLAG')
plot(data$MSTATUS,data$TARGET_FLAG,
     xlab='MSTATUS',ylab='TARGET_FLAG',main='MSTATUS & TARGET_FLAG')
plot(data$MVR_PTS,data$TARGET_FLAG,
     xlab='MVR_PTS',ylab='TARGET_FLAG',main='MVR_PTS & TARGET_FLAG')

par(mfrow = c(2, 2))
plot(data$OLDCLAIM,data$TARGET_FLAG,
     xlab='OLDCLAIM',ylab='TARGET_FLAG',main='OLDCLAIM & TARGET_FLAG')
plot(data$PARENT1,data$TARGET_FLAG,
     xlab='PARENT1',ylab='TARGET_FLAG',main='PARENT1 & TARGET_FLAG')
plot(data$RED_CAR,data$TARGET_FLAG,
     xlab='RED_CAR',ylab='TARGET_FLAG',main='RED_CAR & TARGET_FLAG')
plot(data$REVOKED,data$TARGET_FLAG,
     xlab='REVOKED',ylab='TARGET_FLAG',main='REVOKED & TARGET_FLAG')

par(mfrow = c(2, 2))
plot(data$COMMERCIAL_USE,data$TARGET_FLAG,
     xlab='COMMERCIAL_USE',ylab='TARGET_FLAG',main='COMMERCIAL_USE & TARGET_FLAG')
plot(data$PRIVATE_USE,data$TARGET_FLAG,
     xlab='PRIVATE_USE',ylab='TARGET_FLAG',main='PRIVATE_USE & TARGET_FLAG')
plot(data$TRAVTIME,data$TARGET_FLAG,
     xlab='TRAVTIME',ylab='TARGET_FLAG',main='TRAVTIME & TARGET_FLAG')
plot(data$YOJ,data$TARGET_FLAG,
     xlab='YOJ',ylab='TARGET_FLAG',main='YOJ & TARGET_FLAG')

par(mfrow = c(2, 2))
plot(data$MINIVAN,data$TARGET_FLAG,
     xlab='MINIVAN',ylab='TARGET_FLAG',main='MINIVAN & TARGET_FLAG')
plot(data$PANEL_TRUCK,data$TARGET_FLAG,
     xlab='PANEL_TRUCK',ylab='TARGET_FLAG',main='PANEL_TRUCK & TARGET_FLAG')
plot(data$PICKUP,data$TARGET_FLAG,
     xlab='PICKUP',ylab='TARGET_FLAG',main='PICKUP & TARGET_FLAG')
plot(data$SPORTS_CAR,data$TARGET_FLAG,
     xlab='SPORTS_CAR',ylab='TARGET_FLAG',main='SPORTS_CAR & TARGET_FLAG')

par(mfrow = c(2, 2))
plot(data$VAN,data$TARGET_FLAG,
     xlab='VAN',ylab='TARGET_FLAG',main='VAN & TARGET_FLAG')
plot(data$SUV,data$TARGET_FLAG,
     xlab='SUV',ylab='TARGET_FLAG',main='SUV & TARGET_FLAG')
plot(data$URBAN,data$TARGET_FLAG,
     xlab='URBAN',ylab='TARGET_FLAG',main='URBAN & TARGET_FLAG')
plot(data$RURAL,data$TARGET_FLAG,
     xlab='RURAL',ylab='TARGET_FLAG',main='RURAL & TARGET_FLAG')

par(mfrow = c(1, 1))
plot(data$HOMEKIDS,data$TARGET_FLAG,
     xlab='HOMEKIDS',ylab='TARGET_FLAG',main='HOMEKIDS & TARGET_FLAG')


# Create scatterplots of each predictor variable against TARGET_AMT to identify
# potential candidate predictors
par(mfrow = c(2, 2))
plot(data$AGE,data$TARGET_AMT,
     xlab='AGE',ylab='TARGET_AMT',main='AGE & TARGET_AMT')
plot(data$BLUEBOOK,data$TARGET_AMT,
     xlab='BLUEBOOK',ylab='TARGET_AMT',main='BLUEBOOK & TARGET_AMT')
plot(data$CAR_AGE,data$TARGET_AMT,
     xlab='CAR_AGE',ylab='TARGET_AMT',main='CAR_AGE & TARGET_AMT')
plot(data$CLM_FREQ,data$TARGET_AMT,
     xlab='CLM_FREQ',ylab='TARGET_FLAG',main='CLM_FREQ & TARGET_FLAG')

par(mfrow = c(2, 2))
plot(data$INCOME,data$TARGET_AMT,
     xlab='INCOME',ylab='TARGET_AMT',main='INCOME & TARGET_AMT')
plot(data$INCOME_BIN,data$TARGET_AMT,
     xlab='INCOME_BIN',ylab='TARGET_AMT',main='INCOME_BIN & TARGET_AMT')
plot(data$MALE_SEX,data$TARGET_FAMT,
     xlab='MALE_SEX',ylab='TARGET_AMT',main='MALE & TARGET_AMT')
plot(data$FEMALE_SEX,data$TARGET_AMT,
     xlab='FEMALE_SEX',ylab='TARGET_AMT',main='FEMALE & TARGET_AMT')

par(mfrow = c(2, 2))
plot(data$HOME_VAL,data$TARGET_AMT,
     xlab='HOME_VAL',ylab='TARGET_AMT',main='HOME_VAL & TARGET_AMT')
plot(data$DO_KIDS_DRIVE,data$TARGET_AMT,
     xlab='DO_KIDS_DRIVE',ylab='TARGET_AMT',main='DO_KIDS_DRIVE & TARGET_AMT')
plot(data$L_HIGH_SCHOOL,data$TARGET_AMT,
     xlab='L_HIGH_SCHOOL',ylab='TARGET_AMT',main='L_HIGH_SCHOOL & TARGET_AMT')
plot(data$HIGH_SCHOOL,data$TARGET_AMT,
     xlab='HIGH_SCHOOL',ylab='TARGET_AMT',main='HIGH_SCHOOL & TARGET_AMT')

par(mfrow = c(2, 2))
plot(data$BACHELORS,data$TARGET_AMT,
     xlab='BACHELORS',ylab='TARGET_AMT',main='BACHELORS & TARGET_AMT')
plot(data$MASTERS,data$TARGET_AMT,
     xlab='MASTERS',ylab='TARGET_AMT',main='MASTERS & TARGET_AMT')
plot(data$PHD,data$TARGET_AMT,
     xlab='PHD',ylab='TARGET_AMT',main='PHD & TARGET_AMT')
plot(data$CLERICAL,data$TARGET_AMT,
     xlab='CLERICAL',ylab='TARGET_FAMT',main='CLERICAL & TARGET_AMT')

par(mfrow = c(2, 2))
plot(data$DOCTOR,data$TARGET_AMT,
     xlab='DOCTOR',ylab='TARGET_AMT',main='DOCTOR & TARGET_AMT')
plot(data$HOME_MAKER,data$TARGET_AMT,
     xlab='HOME_MAKER',ylab='TARGET_AMT',main='HOME_MAKER & TARGET_AMT')
plot(data$LAWYER,data$TARGET_AMT,
     xlab='LAWYER',ylab='TARGET_AMT',main='LAWYER & TARGET_AMT')
plot(data$MANAGER,data$TARGET_AMT,
     xlab='MANAGER',ylab='TARGET_AMT',main='MANAGER & TARGET_AMT')

par(mfrow = c(2, 2))
plot(data$PROFESSIONAL,data$TARGET_AMT,
     xlab='PROFESSIONAL',ylab='TARGET_AMT',main='PROFESSIONAL & TARGET_AMT')
plot(data$TIF,data$TARGET_AMT,
     xlab='TIF',ylab='TARGET_AMT',main='TIF & TARGET_AMT')
plot(data$MSTATUS,data$TARGET_AMT,
     xlab='MSTATUS',ylab='TARGET_AMT',main='MSTATUS & TARGET_AMT')
plot(data$MVR_PTS,data$TARGET_AMT,
     xlab='MVR_PTS',ylab='TARGET_AMT',main='MVR_PTS & TARGET_AMT')

par(mfrow = c(2, 2))
plot(data$OLDCLAIM,data$TARGET_AMT,
     xlab='OLDCLAIM',ylab='TARGET_AMT',main='OLDCLAIM & TARGET_AMT')
plot(data$PARENT1,data$TARGET_AMT,
     xlab='PARENT1',ylab='TARGET_AMT',main='PARENT1 & TARGET_AMT')
plot(data$RED_CAR,data$TARGET_AMT,
     xlab='RED_CAR',ylab='TARGET_AMT',main='RED_CAR & TARGET_AMT')
plot(data$REVOKED,data$TARGET_AMT,
     xlab='REVOKED',ylab='TARGET_AMT',main='REVOKED & TARGET_AMT')

par(mfrow = c(2, 2))
plot(data$COMMERCIAL_USE,data$TARGET_AMT,
     xlab='COMMERCIAL_USE',ylab='TARGET_AMT',main='COMMERCIAL_USE & TARGET_AMT')
plot(data$PRIVATE_USE,data$TARGET_AMT,
     xlab='PRIVATE_USE',ylab='TARGET_AMT',main='PRIVATE_USE & TARGET_AMT')
plot(data$TRAVTIME,data$TARGET_AMT,
     xlab='TRAVTIME',ylab='TARGET_AMT',main='TRAVTIME & TARGET_AMT')
plot(data$YOJ,data$TARGET_AMT,
     xlab='YOJ',ylab='TARGET_AMT',main='YOJ & TARGET_AMT')

par(mfrow = c(2, 2))
plot(data$MINIVAN,data$TARGET_FAMT,
     xlab='MINIVAN',ylab='TARGET_AMT',main='MINIVAN & TARGET_AMT')
plot(data$PANEL_TRUCK,data$TARGET_AMT,
     xlab='PANEL_TRUCK',ylab='TARGET_AMT',main='PANEL_TRUCK & TARGET_AMT')
plot(data$PICKUP,data$TARGET_AMT,
     xlab='PICKUP',ylab='TARGET_AMT',main='PICKUP & TARGET_AMT')
plot(data$SPORTS_CAR,data$TARGET_AMT,
     xlab='SPORTS_CAR',ylab='TARGET_AMT',main='SPORTS_CAR & TARGET_AMT')

par(mfrow = c(2, 2))
plot(data$VAN,data$TARGET_AMT,
     xlab='VAN',ylab='TARGET_AMT',main='VAN & TARGET_AMT')
plot(data$SUV,data$TARGET_AMT,
     xlab='SUV',ylab='TARGET_AMT',main='SUV & TARGET_AMT')
plot(data$URBAN,data$TARGET_AMT,
     xlab='URBAN',ylab='TARGET_AMT',main='URBAN & TARGET_AMT')
plot(data$RURAL,data$TARGET_AMT,
     xlab='RURAL',ylab='TARGET_AMT',main='RURAL & TARGET_AMT')

par(mfrow = c(1, 1))
plot(data$HOMEKIDS,data$TARGET_AMT,
     xlab='HOMEKIDS',ylab='TARGET_AMT',main='HOMEKIDS & TARGET_AMT')


##### Model Development #####

# model.1
# Fit Linear Regression model
model.1 <- glm(TARGET_FLAG ~ AGE + BLUEBOOK + CAR_AGE + CLM_FREQ + INCOME + INCOME_BIN +
                 MALE_SEX + FEMALE_SEX + HOME_VAL + DO_KIDS_DRIVE + L_HIGH_SCHOOL + 
                 HIGH_SCHOOL + BACHELORS + MASTERS + PHD + CLERICAL + DOCTOR + 
                 HOME_MAKER + LAWYER + MANAGER + PROFESSIONAL + TIF + MSTATUS + 
                 MVR_PTS + OLDCLAIM + PARENT1 + RED_CAR + REVOKED + COMMERCIAL_USE + 
                 PRIVATE_USE + TRAVTIME + YOJ + MINIVAN + PANEL_TRUCK + PICKUP + 
                 SPORTS_CAR + VAN + SUV + URBAN + RURAL + HOMEKIDS, data=data, 
               family=binomial(link="logit"))
summary(model.1)
data$model.1Prediction <- predict(model.1, type = "response")
anova(model.1, test="Chisq")
pR2(model.1)

# re-create model.1 with significant variables only
model.1b <- glm(TARGET_FLAG ~ BLUEBOOK + CLM_FREQ + INCOME + HOME_VAL + DO_KIDS_DRIVE + 
                  DOCTOR + MANAGER + TIF + MSTATUS + MVR_PTS + OLDCLAIM + PARENT1 + 
                  REVOKED + COMMERCIAL_USE + TRAVTIME + MINIVAN + PICKUP + SPORTS_CAR + 
                  URBAN, data=data, family=binomial(link="logit"))
summary(model.1b)
data$model.1bPrediction <- predict(model.1b, type = "response")
anova(model.1b, test="Chisq")
pR2(model.1b)


# model.2 - StepAIC with Logistic Regression
# Fit the regression model
model.2 <- glm(TARGET_FLAG ~ AGE + BLUEBOOK + CAR_AGE + CLM_FREQ + INCOME + INCOME_BIN +
                 MALE_SEX + FEMALE_SEX + HOME_VAL + DO_KIDS_DRIVE + L_HIGH_SCHOOL + 
                 HIGH_SCHOOL + BACHELORS + MASTERS + PHD + CLERICAL + DOCTOR + 
                 HOME_MAKER + LAWYER + MANAGER + PROFESSIONAL + TIF + MSTATUS + 
                 MVR_PTS + OLDCLAIM + PARENT1 + RED_CAR + REVOKED + COMMERCIAL_USE + 
                 PRIVATE_USE + TRAVTIME + YOJ + MINIVAN + PANEL_TRUCK + PICKUP + 
                 SPORTS_CAR + VAN + SUV + URBAN + RURAL + HOMEKIDS, data=data, 
               family=binomial(link="logit"))
stepwise.model.2 <- stepAIC(model.2, direction = "both")
summary(stepwise.model.2)
data$stepwise.model.2Prediction <- predict(stepwise.model.2, type = "response")
anova(stepwise.model.2, test="Chisq")
pR2(stepwise.model.2)


# model.3 - StepAIC with Probit Regression
# Fit the regression model
model.3 <- glm(TARGET_FLAG ~ AGE + BLUEBOOK + CAR_AGE + CLM_FREQ + INCOME + INCOME_BIN +
                 MALE_SEX + FEMALE_SEX + HOME_VAL + DO_KIDS_DRIVE + L_HIGH_SCHOOL + 
                 HIGH_SCHOOL + BACHELORS + MASTERS + PHD + CLERICAL + DOCTOR + 
                 HOME_MAKER + LAWYER + MANAGER + PROFESSIONAL + TIF + MSTATUS + 
                 MVR_PTS + OLDCLAIM + PARENT1 + RED_CAR + REVOKED + COMMERCIAL_USE + 
                 PRIVATE_USE + TRAVTIME + YOJ + MINIVAN + PANEL_TRUCK + PICKUP + 
                 SPORTS_CAR + VAN + SUV + URBAN + RURAL + HOMEKIDS, data=data, 
               family=binomial(link="probit"))
stepwise.model.3 <- stepAIC(model.3, direction = "both")
summary(stepwise.model.3)
data$stepwise.model.3Prediction <- predict(stepwise.model.3, type = "response")
anova(stepwise.model.3, test="Chisq")
pR2(stepwise.model.3)


# model.4 - StepAIC Linear Regression for TARGET_AMT Response Variable
model.4 <- lm(TARGET_AMT ~ AGE + BLUEBOOK + CAR_AGE + CLM_FREQ + INCOME + INCOME_BIN +
                MALE_SEX + FEMALE_SEX + HOME_VAL + DO_KIDS_DRIVE + L_HIGH_SCHOOL + 
                HIGH_SCHOOL + BACHELORS + MASTERS + PHD + CLERICAL + DOCTOR + 
                HOME_MAKER + LAWYER + MANAGER + PROFESSIONAL + TIF + MSTATUS + 
                MVR_PTS + OLDCLAIM + PARENT1 + RED_CAR + REVOKED + COMMERCIAL_USE + 
                PRIVATE_USE + TRAVTIME + YOJ + MINIVAN + PANEL_TRUCK + PICKUP + 
                SPORTS_CAR + VAN + SUV + URBAN + RURAL + HOMEKIDS, data=data)
stepwise.model.4 <- stepAIC(model.4, direction = "both")
summary(stepwise.model.4)


##### Model Comparison #####

# Calculate AIC values for each model
aic.model.1b <- AIC(model.1b)
aic.stepwise.model.2 <- AIC(stepwise.model.2)
aic.stepwise.model.3 <- AIC(stepwise.model.3)

aic.model.1b
aic.stepwise.model.2
aic.stepwise.model.3 

# Calculate BIC values for each model
bic.model.1b <- BIC(model.1b)
bic.stepwise.model.2 <- BIC(stepwise.model.2)
bic.stepwise.model.3 <- BIC(stepwise.model.3)

bic.model.1b
bic.stepwise.model.2
bic.stepwise.model.3

# Calculate Log Likelhood for each model
ll.model.1b <- print(-2*logLik(model.1b, REML = TRUE))
ll.stepwise.model.2 <- print(-2*logLik(stepwise.model.2, REML = TRUE))
ll.stepwise.model.3 <- print(-2*logLik(stepwise.model.3, REML = TRUE))

ll.model.1b
ll.stepwise.model.2
ll.stepwise.model.3

# Calculate Ks-Statistic for each model
kss.model.1b <- ks_stat(actuals=data$TARGET_FLAG, 
                        predictedScores=data$model.1bPrediction)
kss.stepwise.model.2 <- ks_stat(actuals=data$TARGET_FLAG, 
                                predictedScores=data$stepwise.model.2Prediction)
kss.stepwise.model.3 <- ks_stat(actuals=data$TARGET_FLAG, 
                                predictedScores=data$stepwise.model.3Prediction)

kss.model.1b
kss.stepwise.model.2
kss.stepwise.model.3

# Generate ROC curves and AUC values for each model
# ROC curve for model.1b
pred.1b <- prediction(data$model.1bPrediction, data$TARGET_FLAG)
perf.1b <- performance(pred.1b, "tpr", "fpr")
plot(perf.1b, main="model.1b ROC Curve")

# AUC for model.1b
auc.1b <- performance(pred.1b, "tpr", "fpr", measure="auc")
auc.1b <- auc@y.values[[1]]
auc.1b

# ROC curve for stepwise.model.2
pred.2 <- prediction(data$stepwise.model.2Prediction, data$TARGET_FLAG)
perf.2 <- performance(pred.1b, "tpr", "fpr")
plot(perf.2, main="stepwise.model.2 ROC Curve")

# AUC for stepwise.model.2
auc.2 <- performance(pred.2, "tpr", "fpr", measure="auc")
auc.2 <- auc@y.values[[1]]
auc.2


# ROC curve for stepwise.model.3
pred.3 <- prediction(data$stepwise.model.3Prediction, data$TARGET_FLAG)
perf.3 <- performance(pred.1b, "tpr", "fpr")
plot(perf.3, main="stepwise.model.3 ROC Curve")

# AUC for stepwise.model.3
auc.3 <- performance(pred.3, "tpr", "fpr", measure="auc")
auc.3 <- auc@y.values[[1]]
auc.3


##### Model Scoring of Test Data #####
# Read in the csv file into an R data frame
path <- '/Users/derekhigham/Documents/school/MSDS 411/Unit 2/'
file <- paste(path, 'logit_insurance_test.csv', sep='')
test.data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)

head(test.data)
str(test.data)


##### Transform $ values to numeric values #####
test.data$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test.data$INCOME)))
test.data$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test.data$OLDCLAIM)))
test.data$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test.data$HOME_VAL)))
test.data$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test.data$BLUEBOOK)))


###### Impute missing data items for AGE, CAR_AGE, HOME_VAL, INCOME, JOB, and YOJ
test.data$JOB[is.na(test.data$JOB)] <- median(test.data$JOB, na.rm=TRUE)
test.data$AGE[is.na(test.data$AGE)] <- mean(test.data$AGE, na.rm=TRUE)
test.data$YOJ <- na.aggregate(test.data$YOJ, test.data$JOB, mean, na.rm=TRUE)
test.data$INCOME <- na.aggregate(test.data$INCOME, test.data$JOB, mean, na.rm=TRUE)
test.data$HOME_VAL <- na.aggregate(test.data$HOME_VAL, test.data$JOB, mean, na.rm=TRUE)
test.data$CAR_AGE <- na.aggregate(test.data$CAR_AGE, test.data$CAR_TYPE, mean, na.rm=TRUE)


##### Data transformations #####
# Due to the discrete nature of a number of variables, we may want to explore the 
# transformation of existing variables and the creation of new variables 

# Read in factor variables as factors
test.data$TARGET_FLAG <- as.factor(test.data$TARGET_FLAG)
test.data$PARENT1 <- ifelse(test.data$PARENT1=="Yes", 1, 0)
test.data$MSTATUS <- ifelse(test.data$MSTATUS=="Yes", 1, 0)
test.data$RED_CAR <- ifelse(test.data$RED_CAR=="yes", 1, 0)
test.data$REVOKED <- ifelse(test.data$REVOKED=="Yes", 1, 0)
test.data$DO_KIDS_DRIVE <- ifelse(test.data$KIDSDRIV > 0, 1, 0)

# SEX transformation
test.data$MALE_SEX <- ifelse(test.data$SEX=="M", 1, 0)
test.data$FEMALE_SEX <- ifelse(test.data$SEX=="z_F", 1, 0)

# EDUCATION transformation
test.data$L_HIGH_SCHOOL <- ifelse(test.data$EDUCATION=="<High School", 1, 0)
test.data$HIGH_SCHOOL <- ifelse(test.data$EDUCATION=="z_High School", 1, 0)
test.data$BACHELORS <- ifelse(test.data$EDUCATION=="Bachelors", 1, 0)
test.data$MASTERS <- ifelse(test.data$EDUCATION=="Masters", 1, 0)
test.data$PHD <- ifelse(test.data$EDUCATION=="PhD", 1, 0)

# JOB transformation
test.data$CLERICAL <- ifelse(test.data$JOB=="Clerical", 1, 0)
test.data$DOCTOR <- ifelse(test.data$JOB=="Doctor", 1, 0)
test.data$HOME_MAKER <- ifelse(test.data$JOB=="Home Maker", 1, 0)
test.data$LAWYER <- ifelse(test.data$JOB=="Lawyer", 1, 0)
test.data$MANAGER <- ifelse(test.data$JOB=="Manager", 1, 0)
test.data$PROFESSIONAL <- ifelse(test.data$JOB=="Professional", 1, 0)
test.data$STUDENT <- ifelse(test.data$JOB=="Student", 1, 0)
test.data$BLUE_COLLAR <- ifelse(test.data$JOB=="z_Blue Collar", 1, 0)

# CAR_USE transformation
test.data$COMMERCIAL_USE <- ifelse(test.data$CAR_USE=="Commercial", 1, 0)
test.data$PRIVATE_USE <- ifelse(test.data$CAR_USE=="Private", 1, 0)

# CAR_TYPE transformation
test.data$MINIVAN <- ifelse(test.data$CAR_TYPE=="Minivan", 1, 0)
test.data$PANEL_TRUCK <- ifelse(test.data$CAR_TYPE=="Panel Truck", 1, 0)
test.data$PICKUP <- ifelse(test.data$CAR_TYPE=="Pickup", 1, 0)
test.data$SPORTS_CAR <- ifelse(test.data$CAR_TYPE=="Sports Car", 1, 0)
test.data$VAN <- ifelse(test.data$CAR_TYPE=="Van", 1, 0)
test.data$SUV <- ifelse(test.data$CAR_TYPE=="z_SUV", 1, 0)

# URBANICITY transformation
test.data$URBAN <- ifelse(test.data$URBANICITY=="Highly Urban/ Urban", 1, 0)
test.data$RURAL <- ifelse(test.data$URBANICITY=="z_Highly Rural/ Rural", 1, 0)

# INCOME bucketing
test.data$INCOME_BIN[test.data$INCOME >= 0 & test.data$INCOME < 30000] <- "LOW"
test.data$INCOME_BIN[test.data$INCOME >= 30000 & test.data$INCOME < 50000] <- "LOWER-MIDDLE"
test.data$INCOME_BIN[test.data$INCOME >= 50000 & test.data$INCOME < 100000] <- "MIDDLE"
test.data$INCOME_BIN[test.data$INCOME >= 100000 & test.data$INCOME < 350000] <- "UPPER-MIDDLE"
test.data$INCOME_BIN[test.data$INCOME >= 350000] <- "UPPER"

# Remove redundant/unnecessary variables from data set
test.data$KIDSDRIV <- NULL
test.data$SEX <- NULL
test.data$EDUCATION <- NULL
test.data$JOB <- NULL
test.data$CAR_USE <- NULL
test.data$CAR_TYPE <- NULL
test.data$URBANICITY <- NULL

# Check data set
str(test.data)

# Write completed training data set to local directory
write.csv(test.data, '/Users/derekhigham/Documents/school/MSDS 411/Unit 2/test.data.csv')


# Stand Alone Scoring Process for P_TARGET_FLAG using stepwise.model.2
test.data$P_TARGET_FLAG <- -3.15 + 
  0.0000253*test.data$BLUEBOOK +
  0.1970000*test.data$CLM_FREQ -
  0.0000040*test.data$INCOME -
  0.0000012*test.data$HOME_VAL +
  0.6310000*test.data$DO_KIDS_DRIVE + 
  0.4210000*test.data$L_HIGH_SCHOOL +
  0.4330000*test.data$HIGH_SCHOOL +
  0.1270000*test.data$CLERICAL -
  0.4150000*test.data$DOCTOR -
  0.7220000*test.data$MANAGER -
  0.0558000*test.data$TIF -
  0.5320000*test.data$MSTATUS +
  0.1150000*test.data$MVR_PTS -
  0.0000143*test.data$OLDCLAIM +
  0.3460000*test.data$PARENT1 +
  0.8960000*test.data$REVOKED +
  0.7690000*test.data$COMMERCIAL_USE +
  0.0146000*test.data$TRAVTIME -
  0.6740000*test.data$MINIVAN -
  0.1380000*test.data$PICKUP +
  0.2830000*test.data$SPORTS_CAR +
  2.3800000*test.data$URBAN +
  0.0521000*test.data$HOMEKIDS


# Stand Alone Scoring Process for P_TARGET_AMT using stepwise.model.4
test.data$P_TARGET_AMT <- 357.00000 - 
  32.20000*test.data$CAR_AGE +
  141.00000*test.data$CLM_FREQ -
  0.00495*test.data$INCOME +
  178.00000*test.data$MALE_SEX +
  701.00000*test.data$DO_KIDS_DRIVE - 
  183.00000*test.data$BACHELORS -
  487.00000*test.data$DOCTOR -
  815.00000*test.data$MANAGER -
  47.80000*test.data$TIF -
  620.00000*test.data$MSTATUS +
  177.00000*test.data$MVR_PTS -
  0.01070*test.data$OLDCLAIM +
  596.00000*test.data$PARENT1 +
  559.00000*test.data$REVOKED +
  746.00000*test.data$COMMERCIAL_USE +
  12.00000*test.data$TRAVTIME -
  575.00000*test.data$MINIVAN -
  232.00000*test.data$PICKUP +
  285.00000*test.data$SPORTS_CAR +
  1650.00000*test.data$URBAN


#subset of data set for the deliverable "Scored data file"
prediction <- test.data[c("INDEX","P_TARGET_FLAG","P_TARGET_AMT")]

# Write prediction data set to directory
write.csv(prediction, '/Users/derekhigham/Documents/school/MSDS 411/Unit 2/prediction.csv')
