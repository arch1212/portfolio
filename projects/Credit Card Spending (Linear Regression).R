# Essential Libraries
library(caret)
library(e1071)
library(corrplot)
library(MASS)
library(lattice)
library(leaps)

# Importing the Prepared data
cc <- read.csv("creditcard.csv")

#################################################
# Dealing with missing Values
#################################################

# Number of Missing values
numMissing <- apply(cc, 2, function(x){sum(is.na(x))})
numMissing

# Removing observations with missing values
cc <- cc[complete.cases(cc),]

#################################################
# Separate out Predictors and Outcome Variable
#################################################
ccPred <- cc[,-ncol(cc)]
ccOut <- cc[,ncol(cc)]

#################################################
# Fixing Skewed Predictors
#################################################

# Determining the skewness of continuous predictors
ccContPred <- ccPred[,sapply(ccPred, is.numeric)]

skewness <- apply(ccContPred, 2, skewness)
summary(skewness)

# Using Box-Cox Transformation to transform skewed predictors
boxcoxtrans <- preProcess(ccPred, method = "BoxCox")
boxcoxtrans

# 29 continuous predictors were not transformed due to zero or negative values 
# 8 predictors were transformed with lambda values between -1 and 1 

ccPred <- predict(boxcoxtrans, ccPred)

#################################################
# Two sets of Predictors: fullSet and reducedSet
#################################################

# full set: ccPred
fullSet <- ccPred

#################################################
# Preparing reducedSet
#################################################

# Checking for multicollinearity

# Using PCA to detect multi-collinearity
ccContPred <- ccPred[,sapply(ccPred, is.numeric)]
pcaObject <- prcomp(ccContPred, center = TRUE, scale. = TRUE)
ls(pcaObject)
percentVariance <- pcaObject$sdev^2/sum(pcaObject$sdev^2)*100
percentVariance[1:3]
plot(percentVariance, xlab = "Components", ylab = "Cumulative Percent of Variance", type = "b")

# The amount of variability summarized by components drop sharply, 
# with no one component accounting for more than 21% of the variance. 
# This profile indicates that the structure of the data is contained 
# in a much smaller number of dimensions than the number of dimensions of the original space. 

# This is often due to a large number of collinearities among the predictors.

# Correlation between Predictors
correlation <- cor(ccContPred)
corrplot(correlation, order = "hclust")

# There are many strong positive correlations (indicated by the large, dark blue circles)

# Removing Strongly piecewise-correlated predictors
pwcp <- findCorrelation(correlation, cutoff = 0.9)
names(ccContPred[pwcp])
ccContPred <- ccContPred[,-pwcp]

# Merging with Categorical predictors
ccCatPred <- ccPred[,sapply(ccPred, is.factor)]

reducedSet <- cbind(ccContPred, ccCatPred)

# Removing Near-Zero Variance Predictors
nzvp <- nearZeroVar(reducedSet)
names(reducedSet[nzvp])
reducedSet <- reducedSet[,-nzvp]

#################################################
# Splitting the data into Training and Test set
#################################################

fullData <- cbind(fullSet, total.card.spend = ccOut)
reducedData <- cbind(reducedSet, total.card.spend = ccOut)

# Using Simple Random Sampling for the Split
set.seed(144)
train <- sample(4995, 3500)

fulltrain <- fullData[train,]
fulltrainPred <- fulltrain[,-ncol(fulltrain)]
fulltrainOut <- fulltrain[,ncol(fulltrain)]

fulltest <- fullData[-train,]
fulltestPred <- fulltest[,-ncol(fulltest)]
fulltestOut <- fulltest[,ncol(fulltest)]

reducedtrain <- reducedData[train,]
redtrainPred <- reducedtrain[,-ncol(reducedtrain)]
redtrainOut <- reducedtrain[,ncol(reducedtrain)]

reducedtest <- reducedData[-train,]
redtestPred <- reducedtest[,-ncol(reducedtest)]
redtestOut <- reducedtest[,ncol(reducedtest)]

#################################################
# Model Training
#################################################

########## Model 1: All Predictors
set.seed(144)
lmFitAllPred <- train(total.card.spend ~.,
                      data = fulltrain,
                      method = "lm",
                      trControl = trainControl(method = "cv", number = 10))
lmFitAllPred

# 10-Fold Cross Validation Estimates
# RMSE = 255.12
# Rsquared = 0.50


########## Model 2: Reduced Data
set.seed(144)
lmFitRedPred <- train(total.card.spend ~.,
                      data = reducedtrain,
                      method = "lm",
                      trControl = trainControl(method = "cv", number = 10))
lmFitRedPred

# 10-Fold Cross Validation Estimates
# RMSE = 254.74
# Rsquared = 0.50

########## Model 4: Using Stepwise selection to select the best model

lmFitAllPred <- lm(total.card.spend~., data = fulltrain)
lmFit2 <- stepAIC(lmFitAllPred, direction = "both")
summary(lmFit2)

# 10-Fold Cross Validation Estimates of Performance
trFit <- train(total.card.spend ~ ed + income + debtinc + creddebt + 
                 othdebt + hometype + carvalue + carbought + reason + card + 
                 cardbenefit + card2 + card2fee + carditems + card2items + 
                 equipten + voice + forward + ownpc + churn, data = fulltrain,
                 method = "lm", trControl = trainControl(method = "cv", number = 10))
trFit
#RMSE      Rsquared 
#247.46    0.53

########## Model 5: Removing Influential Observations

w <- abs(rstudent(lmFit2)) > 3 | abs(cooks.distance(lmFit2)) > 4/nrow(lmFit2$model)
InfObs <- which(w)
InfObs
fulltrainUpdated <- fulltrain[-InfObs,]

# 10-Fold Cross Validation Estimates of Performance
trFit2 <- train(total.card.spend ~ ed + income + debtinc + creddebt + 
                  othdebt + hometype + carvalue + carbought + reason + card + 
                  cardbenefit + card2 + card2fee + carditems + card2items + 
                  equipten + voice + forward + ownpc + churn, data = fulltrainUpdated,
                  method = "lm", trControl = trainControl(method = "cv", number = 10))
trFit2
#RMSE      Rsquared 
#163.37    0.61

#################################################
# Model Selection
#################################################

#Comparing the Cross-validation Performance, trFit is selected

# Training on complete training Data
lmFit3 <- lm(total.card.spend ~ ed + income + debtinc + creddebt + 
               othdebt + hometype + carvalue + carbought + reason + card + 
               cardbenefit + card2 + card2fee + carditems + card2items + 
               equipten + voice + forward + ownpc + churn, data = fulltrainUpdated)
summary(lmFit3)
# Adjusted R-squared:  0.6139

model <- lmFit3

#################################################
# Test Set Prediction and Results
################################################# 

lmPred <- predict(model, newdata = fulltestPred)
defaultSummary(data.frame(obs = fulltestOut, pred = lmPred))

# RMSE = 229.26
# Rsquared = 0.53

#################################################
# Checking model Assumptions using Visualizations
#################################################

# Observed vs Predicted Values
xyplot(fulltrainUpdated[,c("total.card.spend")] ~ predict(model),
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Observed")

# Residual Plots
xyplot(resid(model) ~ predict(model),
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Residuals")

#################################################
# Determining variable imortance
################################################# 

importance <- varImp(trFit2, scale=FALSE)

# summarize importance
print(importance)

# plot importance
plot(importance)
