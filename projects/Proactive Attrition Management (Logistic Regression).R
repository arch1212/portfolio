# Essential libraries
library(caret)
library(e1071)
library(corrplot)
library(MASS)
library(dplyr)
library(mice)
library(ROCR)

# Importing the data
pam <- read.csv("Proactive Attrition Management-Logistic Regression Case Study.csv")

# Exploring the data
str(pam)
summary(pam)

#################################################
# Fixing Variables' Data types
#################################################

# Convert all variables to Numeric
for(i in 1:ncol(pam)){
  pam[,i] <- as.numeric(pam[,i])
}
str(pam)

# Convert categorical features to factors
fac <- c(22,26,33:68,72,76)
for(i in fac){
  pam[,i] <- as.factor(pam[,i])
}
str(pam)

#################################################
# Data Preparation
#################################################

# Missing values in income and setprc
length(which(pam$INCOME==0)) #17750
length(which(pam$SETPRC==0)) #40249

pam$INCOME[pam$INCOME == 0] <- NA
pam$SETPRC[pam$SETPRC == 0] <- NA

# Removing redundant features
pam <- pam %>% select(-INCMISS, -SETPRCM, -CUSTOMER, -MARRYNO, -NEWCELLN, -CHURNDEP)

# Save CALIBRAT to a vector and remove it

CALIBRAT <- pam$CALIBRAT
pam <- pam %>% select(-CALIBRAT)

#################################################
# Separate out Predictors and Outcome Variable
#################################################
pamPred <- pam %>% select(-CHURN)
pamOut <- pam %>% select(CHURN)

#################################################
# Dealing with missing Values
#################################################

# Number of Missing values
numMissing <- apply(pam, 2, function(x){sum(is.na(x))})
numMissing

# Removing SETPRC variable
pamPred <- pamPred %>% select(-SETPRC)

# Imputing values

n <- which(names(pamPred) == "CSA")
imputed <- complete(mice(pamPred[,-n]))
str(imputed)

# Replacing missing values

pamPred$REVENUE <- imputed$REVENUE
pamPred$MOU <- imputed$MOU
pamPred$RECCHRGE <- imputed$RECCHRGE
pamPred$DIRECTAS <- imputed$DIRECTAS
pamPred$OVERAGE <- imputed$OVERAGE
pamPred$ROAM <- imputed$ROAM
pamPred$CHANGEM <- imputed$CHANGEM
pamPred$CHANGER <- imputed$CHANGER
pamPred$PHONES <- imputed$PHONES
pamPred$MODELS <- imputed$MODELS
pamPred$EQPDAYS <- imputed$EQPDAYS
pamPred$AGE1 <- imputed$AGE1
pamPred$AGE2 <- imputed$AGE2
pamPred$INCOME <- imputed$INCOME

# Write this data to a file

pamImputed <- cbind(pamPred, CALIBRAT)
pamImputed <- cbind(pamImputed, pamOut)
write.csv(pamImputed, "pamImputed.csv", row.names = FALSE)

#################################################
# Fixing Skewed Predictors
#################################################

# Determining the skewness of continuous predictors
pamContPred <- pamPred[,!sapply(pamPred, is.factor)]

skewness <- apply(pamContPred, 2, skewness)
summary(skewness)

# Using Box-Cox Transformation to transform skewed predictors
boxcoxtrans <- preProcess(pamPred, method = "BoxCox")
boxcoxtrans

# 27 continuous predictors were not transformed due to zero or negative values 
# 5 predictors were transformed with lambda values between -1 and 1 

pamPred <- predict(boxcoxtrans, pamPred)

#################################################
# Two sets of Predictors: fullSet and reducedSet
#################################################

# full set: pamPred
fullSet <- cbind(pamPred, CHURN = pamOut)

#################################################
# Preparing reducedSet
#################################################

# Checking for multicollinearity

# Using PCA to detect multi-collinearity
pamContPred <- pamPred[,!sapply(pamPred, is.factor)]
pcaObject <- prcomp(pamContPred, center = TRUE, scale. = TRUE)
ls(pcaObject)
percentVariance <- pcaObject$sdev^2/sum(pcaObject$sdev^2)*100
percentVariance[1:3]
plot(percentVariance, xlab = "Components", ylab = "Cumulative Percent of Variance", type = "b")

# The amount of variability summarized by components drop sharply, 
# with no one component accounting for more than 27% of the variance. 
# This profile indicates that the structure of the data is contained 
# in a much smaller number of dimensions than the number of dimensions of the original space. 

# This is often due to a large number of collinearities among the predictors.

# Correlation between Predictors
correlation <- cor(pamContPred)
corrplot(correlation, order = "hclust")

# There are many strong positive correlations (indicated by the large, dark blue circles)

# Removing Strongly piecewise-correlated predictors
pwcp <- findCorrelation(correlation, cutoff = 0.8)
names(pamContPred[pwcp])
pamContPred <- pamContPred[,-pwcp]

# Merging with Categorical predictors
pamCatPred <- pamPred[,sapply(pamPred, is.factor)]

reducedSet <- cbind(pamContPred, pamCatPred)

# Removing Near-Zero Variance Predictors
nzvp <- nearZeroVar(reducedSet)
names(reducedSet[nzvp])
reducedSet <- reducedSet[,-nzvp]

reducedSet <- cbind(reducedSet, CHURN = pamOut)

fullSet$CHURN <- factor(fullSet$CHURN, labels = c("No", "Yes"))
reducedSet$CHURN <- factor(reducedSet$CHURN, labels = c("No", "Yes"))

#################################################
# Splitting the data into Training and Test set
#################################################

# adding CALIBRAT
fullSetC <- cbind(fullSet, CALIBRAT)
reducedSetC <- cbind(reducedSet, CALIBRAT)

fullCal <- fullSetC %>% filter(CALIBRAT == 1) %>% select(-CALIBRAT)
fullVal <- fullSetC %>% filter(CALIBRAT == 0) %>% select(-CALIBRAT)

redCal <- reducedSetC %>% filter(CALIBRAT == 1) %>% select(-CALIBRAT)
redVal <- reducedSetC %>% filter(CALIBRAT == 0) %>% select(-CALIBRAT)


#################################################
# Model Training
#################################################

levels(fullCal$CHURN)
# "No" "Yes"
# The glm function treats the second factor level as the event of interest.
# so, the probability output from glm function will give the probability of CHURN

######################### Model 1: Baseline Model
prop.table(table(fullVal$CHURN))
# Accuracy: 0.9803

######################### Model 2: All Predictors
glmAllPredictors <- glm(CHURN~., data = fullCal, family = binomial)
summary(glmAllPredictors)
# AIC: 54400

######################### Model 3: Reduced Number of Predictors
glmRedPred <- glm(CHURN~., data = reducedCal, family = binomial)
summary(glmRedPred)
# AIC: 54397

######################### Model 4: Using Stepwise Selection to select the Best Model

# Model with all predictors except CSA 
glmAllFull2 <- glm(CHURN~.-CSA, data = fullCal, family = binomial)
glmAllFull2
# AIC: 53920

# Using Step function to find the best model
glmFit1 <- stepAIC(glmAllFull2, direction = "both")
glmFit1
# AIC: 53880

# Adding CSA variable to the final model
glmFit2 <- glm(formula = CHURN ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + 
      CHANGEM + CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE + 
      THREEWAY + OUTCALLS + INCALLS + PEAKVCE + MONTHS + UNIQSUBS + 
      ACTVSUBS + PHONES + MODELS + EQPDAYS + AGE1 + CHILDREN + 
      CREDITA + CREDITC + CREDITDE + CREDITGY + PRIZMRUR + PRIZMUB + 
      REFURB + WEBCAP + OCCHMKR + MARRYUN + MAILRES + RETCALLS + 
      RETACCPT + NEWCELLY + MCYCLE + CREDITAD + CSA, family = binomial, 
      data = fullCal)
glmFit2
# AIC: 54360


##################################################
# Choosing the Model
##################################################

# Final Model
model <- glmFit1

# Cross Validation Estimate of Performance

# Using 10-fold Cross Validation
ctrl <- trainControl(method = "cv", number =  10,
                     classProbs = TRUE)

trFit <- train(CHURN ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + 
                 CHANGEM + CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE + 
                 THREEWAY + OUTCALLS + INCALLS + PEAKVCE + MONTHS + UNIQSUBS + 
                 ACTVSUBS + PHONES + MODELS + EQPDAYS + AGE1 + CHILDREN + 
                 CREDITA + CREDITC + CREDITDE + CREDITGY + PRIZMRUR + PRIZMUB + 
                 REFURB + WEBCAP + OCCHMKR + MARRYUN + MAILRES + RETCALLS + 
                 RETACCPT + NEWCELLY + MCYCLE + CREDITAD, data = fullCalCSA,
                 method = "glm", trControl = ctrl)
trFit

# Accuracy  Kappa   
# 0.583775  0.16755

##################################################
# Select optimal threshold setting using ROC Curve
##################################################

ROCRpred <- prediction(glmPred, fullVal$CHURN)
ROCRperf <- performance(ROCRpred, "sens", "spec")
plot(ROCRperf, colorize = TRUE)

##################################################
# Choosing the Threshold to make Class Predictions
##################################################

# using 0.23 as to correctly predict all the churners in the test set. 

##################################################
# Make final predictions over Validation set
##################################################

glmPred <- predict(model, newdata = fullVal, type = "response")

# Confusion matrix
cm1 <- table(Prediction = glmPred > 0.23, Observed = fullVal$CHURN)
dimnames(cm1)[[1]] = c("No","Yes")
cm <- confusionMatrix(cm1)
cm

##################################################
# Calculate AUC
##################################################

ROCRpredVal <- prediction(glmPred, fullVal$CHURN)
auc = as.numeric(performance(ROCRpredVal, "auc")@y.values)
auc 

# 0.61

# Interpretation of AUC

# Given a random customer from the dataset who actually churned, and 
# a random customer from the dataset who actually didn't churn,
# 62 % of the time our model will classify which is which correctly. 

#################################################
# Determining variable imortance
################################################# 

importance <- varImp(trFit, scale=FALSE)

# summarize importance
print(importance)

# plot importance
plot(importance)




