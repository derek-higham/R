# Derek Higham
# MSDS 410 - Assignment 5
# Ames Data EDA

##### Sample Definition #####

# Read in the csv file into an R data frame
path.name <- '/Users/derekhigham/Documents/school/MSDS_410/'
file.name <- paste(path.name, 'ames_housing_data.csv', sep='')
ames.df <- read.csv(file.name, header=TRUE, stringsAsFactors=FALSE)

# Create a waterfall of drop conditions
# Work the data frame as a 'table' like you would in SAS or SQL
ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
                                ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
                                       ifelse(ames.df$Street!='Pave','03: Street Not Paved',
                                              ifelse(ames.df$YearBuilt <1950,'04: Built Pre-1950',
                                                     ifelse(ames.df$TotalBsmtSF <1,'05: No Basement',
                                                            ifelse(ames.df$GrLivArea <800,'06: LT 800 SqFt',
                                                                   ifelse(ames.df$Zoning=='A (agr)','07: Agriculture',
                                                                          ifelse(ames.df$Zoning=='C (all)','08: Commercial',
                                                                                 ifelse(ames.df$Zoning=='FV','09: Floating Village',
                                                                                        ifelse(ames.df$Zoning=='I (all)','10: Industrial',
                                                                                               '99: Eligible Sample')
                                                                                 )))))))))

table(ames.df$dropCondition)

# Save the table
waterfall <- table(ames.df$dropCondition)

# Format the table as a column matrix for presentation
as.matrix(waterfall,7,1)

# Eliminate all observation that are not part of the eligible sample population
eligible.population <- subset(ames.df,dropCondition=='99: Eligible Sample')

# Check that all remaining observation are eligible
table(eligible.population$dropCondition)

# Create a list of interesting predictor variables
str(ames.df)

# How do I restrict my data frame to just these columns that interest me?
# Make a vector of the names

keep.vars <- c('SID','PID','LotFrontage','LotArea','LotConfig','Neighborhood',
               'HouseStyle','OverallQual','OverallCond','YearBuilt','YearRemodel','Exterior1',
               'BsmtFinSF1','BsmtFinSF2','CentralAir','GrLivArea','BsmtFullBath','BsmtHalfBath',
               'FullBath','HalfBath','BedroomAbvGr','TotRmsAbvGrd','Fireplaces','GarageCars',
               'GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','ThreeSsnPorch',
               'ScreenPorch','PoolArea','MoSold','YrSold','SaleCondition','SalePrice'
)

# Note that the R data frame is a (rectabgular) list object which means that it can be 
# accessed in two ways - as a matrix or as a list
# Note that the keep.vars are the COLUMNS that we want to keep, not the rows

skinny.df <- eligible.population[,keep.vars]

# Use the structure command to view the contents of the data frame
str(skinny.df)

# Delete observations with missing values 
sample.df <- na.omit(skinny.df)

# Check the change in dimension
dim(skinny.df)
dim(sample.df)

dim(skinny.df)-dim(sample.df)

# Define total square footage
sample.df$TotalSqftCalc <- sample.df$BsmtFinSF1+sample.df$BsmtFinSF2+sample.df$GrLivArea

# Define total bathrooms
sample.df$TotalBathCalc <- sample.df$BsmtFullBath + 0.5*sample.df$BsmtHalfBath +
  + sample.df$FullBath + 0.5*sample.df$HalfBath

# Corner lot indicator
# ifelse(condition,valueTrue,valueFalse)
sample.df$CornerLotInd <- ifelse(sample.df$LotConfig=='Corner',1,0)

# Check how the indicator is assigned
table(sample.df$CornerLotInd,sample.df$LotConfig)

# Define two indicators for fire places
table(sample.df$Fireplaces)

# Intercept Adjustment for a single fireplace
sample.df$FireplaceInd1 <- ifelse((sample.df$Fireplaces>0)&(sample.df$Fireplaces<2),1,0)
table(sample.df$FireplaceInd1,sample.df$Fireplaces)

# Intercept Adjustment for 2 or more fireplaces
sample.df$FireplaceInd2 <- ifelse((sample.df$Fireplaces>1),1,0)
table(sample.df$FireplaceInd2,sample.df$Fireplaces)

# Additive Intercept Adjustment for a single fireplace
sample.df$FireplaceAdder1 <- ifelse((sample.df$Fireplaces>0),1,0)
table(sample.df$FireplaceAdder1,sample.df$Fireplaces)

# Additive Intercept Adjustment for 2 or more fireplaces
sample.df$FireplaceAdder2 <- ifelse((sample.df$Fireplaces>1),1,0)
table(sample.df$FireplaceAdder2,sample.df$Fireplaces)

# Central Air Indicator
sample.df$CentralAirInd <- ifelse(sample.df$CentralAir=='Y',1,0)
table(sample.df$CentralAirInd) 
# Looks like this is not useful since almost all homes have central air

# Exterior Siding Type
sample.df$BrickInd <- ifelse(sample.df$Exterior1=='BrkFace',1,0)
sample.df$VinylSidingInd <- ifelse(sample.df$Exterior1=='VinylSd',1,0)

# Pool Indicator
sample.df$PoolInd <- ifelse(sample.df$PoolArea>0,1,0)

# Wood Deck Indicator
sample.df$WoodDeckInd <- ifelse(sample.df$WoodDeckSF>0,1,0)

# Porch Indicator - Open Porch OR Screen Porch
sample.df$PorchInd <- ifelse((sample.df$OpenPorchSF>0)||(sample.df$ScreenPorch>0),1,0)

# Quality Index
sample.df$QualityIndex <- sample.df$OverallQual*sample.df$OverallCond
table(sample.df$QualityIndex)

# Year Sold Indicators
sample.df$I2006 <- ifelse(sample.df$YrSold==2006,1,0)
sample.df$I2007 <- ifelse(sample.df$YrSold==2007,1,0)
sample.df$I2008 <- ifelse(sample.df$YrSold==2008,1,0)
sample.df$I2009 <- ifelse(sample.df$YrSold==2009,1,0)
sample.df$I2010 <- ifelse(sample.df$YrSold==2010,1,0)

table(sample.df$YrSold)
table(sample.df$I2006)
table(sample.df$I2007)
table(sample.df$I2008)
table(sample.df$I2009)
table(sample.df$I2010)

# Let's create a family of indicator variables;
# We will take the baseline category to be 0;
# What does this mean?  Why am I creating three indicator variables?
# Should I be creating an indicator variable for 0?
sample.df$garage1 <- ifelse(sample.df$GarageCars==1,1,0);
sample.df$garage2 <- ifelse(sample.df$GarageCars==2,1,0);
sample.df$garage3 <- ifelse(sample.df$GarageCars>=3,1,0);

# define logSalePrice variable
sample.df$logSalePrice <- log(sample.df$SalePrice)

# List out sample.df
str(sample.df)


##### The Train/Test Split #####

# Set the seed on the random number generator so you get the same split every time
# that you run the code
set.seed(123)
sample.df$u <- runif(n=dim(sample.df)[1],min=0,max=1)

# Define this variable for later use
sample.df$QualityIndex <- sample.df$OverallQual*sample.df$OverallCond

# Create train/test split
train.df <- subset(sample.df, u<0.70)
test.df <- subset(sample.df, u>=0.70)

# Check your data split. The sum of the parts should equal the whole
# Do your totals add up?
dim(sample.df)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]


##### Model Identification and In-Sample Model Fit #####

# Create a drop list of variables to be excluded from automated variable selection
drop.list <- c('SID','PID','LotConfig','Neighborhood','HouseStyle','YearBuilt', 
               'YearRemodel','Exterior1','BsmtFinSF1','BsmtFinSF2','CentralAir',
               'YrSold','MoSold','SaleCondition','u','train','I2010','BsmtFullBath',
               'BsmtHalfBath','FullBath','HalfBath','FireplaceInd1','FireplaceInd2',
               'OverallQual','OverallCond','PoolArea','GrLivArea','logSalePrice',
               'Fireplaces','garage3','GarageCars','FireplaceAdder1','garage2')

train.clean <- train.df[,!(names(sample.df) %in% drop.list)]
str(train.clean)

# Load the MASS library
library(MASS)

# Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ .,data=train.clean)
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1,data=train.clean)

# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean)
summary(sqft.lm)

# Forward variable selection
forward.lm <- stepAIC(object=lower.lm, direction=c('forward'),
                      scope=list(upper=formula(upper.lm), lower=formula(lower.lm)))

summary(forward.lm)

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(forward.lm$residuals)
qqline(forward.lm$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))

plot(train.clean$TotalSqftCalc,forward.lm$residuals,xlab='Predictor: TotalSqftCalc',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$garage3,forward.lm$residuals,xlab='Predictor: garage3',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$QualityIndex,forward.lm$residuals,xlab='Predictor: QualityIndex',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$VinylSidingInd,forward.lm$residuals,xlab='Predictor: VinylSidingInd',ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))

plot(train.clean$LotArea,forward.lm$residuals,xlab='Predictor: LotArea',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$TotRmsAbvGrd,forward.lm$residuals,xlab='Predictor: TotRmsAbvGrd',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$BedroomAbvGr,forward.lm$residuals,xlab='Predictor: BedroomAbvGr',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$garage1,forward.lm$residuals,xlab='Predictor: garage1',ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))

plot(train.clean$FireplaceAdder2,forward.lm$residuals,xlab='Predictor: FireplaceAdder2',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$WoodDeckInd,forward.lm$residuals,xlab='Predictor: WoodDeckInd',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$OpenPorchSF,forward.lm$residuals,xlab='Predictor: OpenPorchSF',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$PoolInd,forward.lm$residuals,xlab='Predictor: PoolInd',ylab='Residual')
title('Residual vs Predictor')


# Backward variable selection
backward.lm <- stepAIC(object=upper.lm, direction=c('backward'))

summary(backward.lm)

# Use the Base R functon qqplot() to assess the normality of the residuals
par("mar")
par(mar=c(1,1,1,1))

qqnorm(backward.lm$residuals)
qqline(backward.lm$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))

plot(train.clean$LotArea,backward.lm$residuals,xlab='Predictor: LotArea',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$BedroomAbvGr,backward.lm$residuals,xlab='Predictor: BedroomAbvGr',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$TotRmsAbvGrd,backward.lm$residuals,xlab='Predictor: TotRmsAbvGrd',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$Fireplaces,backward.lm$residuals,xlab='Predictor: Fireplaces',ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))

plot(train.clean$GarageCars,backward.lm$residuals,xlab='Predictor: GarageCars',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$GarageArea,backward.lm$residuals,xlab='Predictor: GarageArea',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$OpenPorchSF,backward.lm$residuals,xlab='Predictor: OpenPorchSF',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$TotalSqftCalc,backward.lm$residuals,xlab='Predictor: TotalSqftCalc',ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))

plot(train.clean$TotalBathCalc,backward.lm$residuals,xlab='Predictor: TotalBathCalc',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$FireplaceAdder1,backward.lm$residuals,xlab='Predictor: FireplaceAdder1',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$FireplaceAdder2,backward.lm$residuals,xlab='Predictor: FireplaceAdder2',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$VinylSidingInd,backward.lm$residuals,xlab='Predictor: VinylSidingInd',ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(3, 2))

plot(train.clean$PoolInd,backward.lm$residuals,xlab='Predictor: PoolInd',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$WoodDeckInd,backward.lm$residuals,xlab='Predictor: WoodDeckInd',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$QualityIndex,backward.lm$residuals,xlab='Predictor: QualityIndex',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$garage2,backward.lm$residuals,xlab='Predictor: garage2',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$garage3,backward.lm$residuals,xlab='Predictor: garage3',ylab='Residual')
title('Residual vs Predictor')


# Stepwise variable selection
stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'))
summary(stepwise.lm)

qqnorm(stepwise.lm$residuals)
qqline(stepwise.lm$residuals)

# Make a scatterplot
par(mfrow = c(2, 2))

plot(train.clean$TotalSqftCalc,stepwise.lm$residuals,xlab='Predictor: TotalSqftCalc',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$garage3,stepwise.lm$residuals,xlab='Predictor: garage3',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$QualityIndex,stepwise.lm$residuals,xlab='Predictor: QualityIndex',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$VinylSidingInd,stepwise.lm$residuals,xlab='Predictor: VinylSidingInd',ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))

plot(train.clean$LotArea,stepwise.lm$residuals,xlab='Predictor: LotArea',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$TotRmsAbvGrd,stepwise.lm$residuals,xlab='Predictor: TotRmsAbvGrd',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$BedroomAbvGr,stepwise.lm$residuals,xlab='Predictor: BedroomAbvGr',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$garage1,stepwise.lm$residuals,xlab='Predictor: garage1',ylab='Residual')
title('Residual vs Predictor')

par(mfrow = c(2, 2))

plot(train.clean$FireplaceAdder2,stepwise.lm$residuals,xlab='Predictor: FireplaceAdder2',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$WoodDeckInd,stepwise.lm$residuals,xlab='Predictor: WoodDeckInd',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$OpenPorchSF,stepwise.lm$residuals,xlab='Predictor: OpenPorchSF',ylab='Residual')
title('Residual vs Predictor')

plot(train.clean$PoolInd,stepwise.lm$residuals,xlab='Predictor: PoolInd',ylab='Residual')
title('Residual vs Predictor')


##### Model Comparison #####
# Compute the junk model
junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea +
                TotalSqftCalc, data=train.df)
summary(junk.lm)

sort(vif(junk.lm),decreasing=TRUE)

# Compute the VIF values for the variable selection methods
library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)

# Calculate AIC for all 4 models
AIC(forward.lm)
AIC(backward.lm)
AIC(stepwise.lm)
AIC(junk.lm)

# Calculate BIC for all 4 models
BIC(forward.lm)
BIC(backward.lm)
BIC(stepwise.lm)
BIC(junk.lm)

# Calculate MSE for all 4 models
mse.f <- mean(forward.lm$residuals^2)
mse.f

mse.b <- mean(backward.lm$residuals^2)
mse.b

mse.s <- mean(stepwise.lm$residuals^2)
mse.s

mse.j <- mean(junk.lm$residuals^2)
mse.j

# Calculate MAE for all 4 models
mae.f <- mean(abs(forward.lm$residuals))
mae.f

mae.b <- mean(abs(backward.lm$residuals))
mae.b

mae.s <- mean(abs(stepwise.lm$residuals))
mae.s

mae.j <- mean(abs(junk.lm$residuals))
mae.j


##### Predictive Accuracy #####
# Define models using test.df as data source
# Create a drop list of variables to be excluded from automated variable selection
drop.list <- c('SID','PID','LotConfig','Neighborhood','HouseStyle','YearBuilt', 
               'YearRemodel','Exterior1','BsmtFinSF1','BsmtFinSF2','CentralAir',
               'YrSold','MoSold','SaleCondition','u','train','I2010','BsmtFullBath',
               'BsmtHalfBath','FullBath','HalfBath','FireplaceInd1','FireplaceInd2',
               'OverallQual','OverallCond','PoolArea','GrLivArea','logSalePrice',
               'Fireplaces','garage3','GarageCars','FireplaceAdder1','garage2')

test.clean <- test.df[,!(names(sample.df) %in% drop.list)]
str(test.clean)

# Load the MASS library
library(MASS)

# Define the upper model as the FULL model
upper.test.lm <- lm(SalePrice ~ .,data=test.clean)
summary(upper.test.lm)

# Define the lower model as the Intercept model
lower.test.lm <- lm(SalePrice ~ 1,data=test.clean)

# Need a SLR to initialize stepwise selection
sqft.test.lm <- lm(SalePrice ~ TotalSqftCalc,data=test.clean)
summary(sqft.test.lm)

# Forward variable selection
forward.test.lm <- stepAIC(object=lower.test.lm, direction=c('forward'),
                      scope=list(upper=formula(upper.test.lm), lower=formula(lower.test.lm)))

summary(forward.test.lm)

# Calculate MSE
mse.f.t <- mean(forward.test.lm$residuals^2)
mse.f.t

# Calculate MAE
mae.f.t <- mean(abs(forward.test.lm$residuals))
mae.f.t

# Backward variable selection
backward.test.lm <- stepAIC(object=upper.test.lm, direction=c('backward'))

summary(backward.test.lm)

# Calculate MSE
mse.b.t <- mean(backward.test.lm$residuals^2)
mse.b.t

# Calculate MAE
mae.b.t <- mean(abs(backward.test.lm$residuals))
mae.b.t

# Stepwise variable selection
stepwise.test.lm <- stepAIC(object=sqft.test.lm,scope=list(upper=formula(upper.test.lm),lower=~1),
                       direction=c('both'))
summary(stepwise.test.lm)

# Calculate MSE
mse.s.t <- mean(stepwise.test.lm$residuals^2)
mse.s.t

# Calculate MAE
mae.s.t <- mean(abs(stepwise.test.lm$residuals))
mae.s.t

# Compute the junk model
junk.test.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea +
                TotalSqftCalc, data=test.df)
summary(junk.test.lm)

# Calculate MSE
mse.j.t <- mean(junk.test.lm$residuals^2)
mse.j.t

# Calculate MAE
mae.j.t <- mean(abs(junk.test.lm$residuals))
mae.j.t


##### Operational Validation #####
# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice

# Assign Prediction Grades
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                          'Grade 4: (0.25+]')))

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)

# Test Data
# Abs Pct Error
forward.testPCT <- abs(forward.test.lm$residuals)/test.df$SalePrice
backward.testPCT <- abs(backward.test.lm$residuals)/test.df$SalePrice
stepwise.testPCT <- abs(junk.test.lm$residuals)/test.df$SalePrice

# Assign PRediction Grades
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')))

forward.testTable <- table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)
