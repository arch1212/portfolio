# Objective: Using Unsupervised Learning algorithms k-means to create Customer Segments from Credit Card data 
# for 9000 customers.


#------------------------------Importing the Data
cc <- read.csv("CC GENERAL.csv")
str(cc)
summary(cc)

#------------------------------Data Preparation

#---------------Removing Customer ID column
cc <- cc[,-1]
str(cc)

#---------------Adding new Variables

cc$monthly.avg.purchase <- cc$PURCHASES/12
cc$limit.usage <- cc$BALANCE/cc$CREDIT_LIMIT
cc$pmp.ratio <- cc$PAYMENTS/cc$MINIMUM_PAYMENTS
str(cc)

#---------------Dealing with missing values
apply(cc, 2, function(x){mean(is.na(x))})

library(VIM)
matrixplot(cc)
# Values are missing at Random

# Imputing missing values
library(mice)

set.seed(144)
imputed <- complete(mice(cc))
summary(imputed)

cc$CREDIT_LIMIT <- imputed$CREDIT_LIMIT
cc$MINIMUM_PAYMENTS <- imputed$MINIMUM_PAYMENTS
cc$limit.usage <- imputed$limit.usage
cc$pmp.ratio <- imputed$pmp.ratio

summary(cc)
#------------------------------Factor Analysis

# Correlation matrix
corrm <- cor(cc)

library(psych)
library(GPArotation)

# Deciding Number of Factors using Scree Plot & Kaiser Test(NUMBER OF EIGEN VALUES OVER 1)

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) 
eigen(corrm)$values                                                     ### EIGEN VALUES

# Calculating Variance, Cumulative Variance 
library(dplyr)
eigen.values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  

# exporting Eigen value summary
write.csv(eigen.values, "eigenvalues.csv") 

# No of factors = 7

# Carry out Factor Analysis
faca <- fa(r = corrm, nfactors = 7, rotate = "varimax", fm = "minres") 

# Sorting the Loadings
sortd <- fa.sort(faca)
sortd
ls(sortd)                                                 
sortd$loadings

# Finding the Eigen Values from the results
sortd$e.values

# Capturing the loading into a dataframe
loadings <- data.frame(sortd$loadings[1:ncol(cc),]) 

# Saving it into a file
write.csv(loadings, "loadings.csv") 


#------------------------------- Preparing the data for Clustering

#----------------------Checking the Outliers
cc1 <- cc

# No of Outliers in each variable
n.outlier <- function(x){
  a <- mean(x) - 3*sd(x)
  b <- mean(x) + 3*sd(x)
  sum(x < a | x > b)
}
apply(cc1, 2, n.outlier)

# Remove observations where at least one of the variable's value is an outlier
y <- list()
for(i in 1:ncol(cc1)){
  x <- cc1[,i]
  y[[i]] <- which(x <= mean(x) - 3*sd(x) | x >= mean(x) + 3*sd(x))
}
y

outliers <- unique(unlist(y))
length(outliers)

# Removing the Observations with Outliers
cc1 <- cc1[-outliers,]

summary(cc1)

#----------------------Centering and Scaling the Values

trans <- preProcess(cc1, method = c("center","scale"))
cc1.t <- predict(trans, cc1)

summary(cc1.t)

#--------------------------------------Clustering

df <- cc1.t

set.seed(144)
km.out.3 = kmeans(df, 3, nstart = 50)
km.out.4 = kmeans(df, 4, nstart = 50)
km.out.5 = kmeans(df, 5, nstart = 50, iter.max = 50)
km.out.6 = kmeans(df, 6, nstart = 50, iter.max = 50) 

cc.new <- cbind(cc1.t, km.clust.3 = km.out.3$cluster,
                km.clust.4 = km.out.4$cluster,
                km.clust.5 = km.out.5$cluster,
                km.clust.6 = km.out.6$cluster)

View(cc.new)

#--------------------------------------- Profiling
install.packages("tables")
library(tables)


tt <- cbind(tabular(1 + factor(km.clust.3) + factor(km.clust.4) + factor(km.clust.5) + factor(km.clust.6) 
                    ~ Heading()*length*All(df["BALANCE_FREQUENCY"]), data = cc.new),
            tabular(1 + factor(km.clust.3) + factor(km.clust.4) + factor(km.clust.5) +factor(km.clust.6)
                    ~ Heading()*mean*All(df), data=cc.new))

tt1 <- as.data.frame.matrix(tt)
View(tt1)

rownames(tt1)<-c("All", "KM3_1" ,"KM3_2" ,"KM3_3", "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,"KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6")
colnames(tt1)<-c("Segment_Size", names(df))
ClusterProfiling<-t(tt1)

write.csv(ClusterProfiling, "ClusterProfiling.csv") 


