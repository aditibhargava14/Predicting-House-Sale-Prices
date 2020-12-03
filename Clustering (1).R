#Loading the libraries 

library(Hmisc) 
library(VIM)
library(dplyr)
library(plyr)
library(ggplot2) 
library(RColorBrewer) 
library(lubridate)

# Loading the Dataset  
list.files("../input")
train<-read.csv("/Users/tejaswininutalapati/Documents/Multivariate Analysis/Project/DataSet/train.csv")

# Missing Imputation and Missing Indicators Creating
#Before everything, we should deal with missing values.  
sort(sapply(train, function(x) sum(is.na(x))), decreasing = TRUE)

#From the table, we found some variables that has amount of missing values: Alley(1369), PoolQC(1453), Fence(1179)
#Before we dump those variables, we need to creat missing indicator first. For example, the missing indicators of the PoolQC will tell us if the house has a pool or not. 
train <- train %>% mutate(PoolQC_imp = if_else(is.na(PoolQC), 1, 0), 
                          MiscFeature_imp = if_else(is.na(MiscFeature), 1, 0), 
                          Alley_imp = if_else(is.na(Alley), 1, 0), 
                          Fence_imp = if_else(is.na(Fence), 1, 0))
drop1 <- names(train) %in% c("PoolQC", "MiscFeature", "Alley", "Fence", "Id")
train <- train[!drop1]

#Using kNN imputation for missing imputation.
train <- kNN(train, k = 5)

#Now we need to drop some unnessary missing indicators.
missing_ind <- train[81:160]
drop2 <- c("PoolQC_imp_imp", "MiscFeature_imp_imp", "Alley_imp_imp", "Fence_imp_imp")
for (i in 1:80) 
{
  if (sum(missing_ind[i] == "TRUE") < 20)
  {
    drop2 <- append(drop2, names(missing_ind)[i])
  }
  else
  {
    next
  }
}
drop2a <- names(train) %in% drop2
train1 <- train[!drop2a]

#As clustering can only recognise the numerical vectors, we have to transfer all the factors to integers.
train1$MSZoning = as.integer(train1$MSZoning)
train1$Street = as.integer(train1$Street)
train1$LotShape = as.integer(train1$LotShape)
train1$LandContour = as.integer(train1$LandContour)
train1$Utilities = as.integer(train1$Utilities)
train1$LotConfig = as.integer(train1$LotConfig)
train1$LandSlope = as.integer(train1$LandSlope)
train1$Neighborhood = as.integer(train1$Neighborhood)
train1$Condition1 = as.integer(train1$Condition1)
train1$Condition2 = as.integer(train1$Condition2)
train1$BldgType = as.integer(train1$BldgType)
train1$HouseStyle = as.integer(train1$HouseStyle)
train1$RoofStyle = as.integer(train1$RoofStyle)
train1$RoofMatl = as.integer(train1$RoofMatl)
train1$Exterior1st = as.integer(train1$Exterior1st)
train1$Exterior2nd = as.integer(train1$Exterior2nd)
train1$MasVnrType = as.integer(train1$MasVnrType)
train1$ExterQual = as.integer(train1$ExterQual)
train1$ExterCond = as.integer(train1$ExterCond)
train1$Foundation = as.integer(train1$Foundation)
train1$BsmtQual = as.integer(train1$BsmtQual)
train1$BsmtCond = as.integer(train1$BsmtCond)
train1$BsmtExposure = as.integer(train1$BsmtExposure)
train1$BsmtFinType1 = as.integer(train1$BsmtFinType1)
train1$BsmtFinType2 = as.integer(train1$BsmtFinType2)
train1$Heating = as.integer(train1$Heating)
train1$HeatingQC = as.integer(train1$HeatingQC)
train1$CentralAir = as.integer(train1$CentralAir)
train1$Electrical = as.integer(train1$Electrical)
train1$KitchenQual = as.integer(train1$KitchenQual)
train1$Functional = as.integer(train1$Functional)
train1$FireplaceQu = as.integer(train1$FireplaceQu)
train1$GarageType = as.integer(train1$GarageType)
train1$GarageFinish = as.integer(train1$GarageFinish)
train1$GarageQual = as.integer(train1$GarageQual)
train1$GarageQual = as.integer(train1$GarageQual)
train1$GarageCond = as.integer(train1$GarageCond)
train1$PavedDrive = as.integer(train1$PavedDrive)
train1$SaleType = as.integer(train1$SaleType)
train1$SaleCondition = as.integer(train1$SaleCondition)
str(train1)

# Variable Clustering
clus_var <- varclus(as.matrix(train1), similarity = "spearman", minlev = 0.05)
plot(clus_var, cex = 0.5)

clus_var2 <- varclus(as.matrix(train1), similarity = "hoeffding", minlev = 0.05)
plot(clus_var2, cex = 0.5)

#Non-hirerarchical Method
#K-Means
# Centers (k's) are numbers thus, 10 random sets are chosen 
(kmeans2.dataset <- kmeans(train1,2,nstart = 10))
# Computing the percentage of variation accounted for. Two clusters
perc.var.2 <- round(100*(1 - kmeans2.dataset$betweenss/kmeans2.dataset$totss),1) 
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

Distance <- dist(train1, method="euclidean")
Distance

#Hirerarchical Methods

#1. Single Linkage Method
# Invoking hclust command (cluster analysis by single linkage method) 
clus_sales_prediction.nn <- hclust(Distance, method = "single") 
clus_sales_prediction.nn
#Plotting of dendrogram using Single Linkage method
plot(as.dendrogram(clus_sales_prediction.nn),
     ylab="Distance",
     main="Dendrogram using Single Linkage method")

#2. Average Linkage Method
clus_sales_prediction.avl <- hclust(Distance, method = "average") 
clus_sales_prediction.avl
#Plotting of dendrogram using Average Linkage method
plot(as.dendrogram(clus_sales_prediction.avl),
     ylab="Distance between patients",
     main="Dendrogram using Average Linkage method")

#3. Complete Linkage Method
clus_sales_prediction.fn <- hclust(Distance) 
clus_sales_prediction.fn
plot(as.dendrogram(clus_sales_prediction.fn),
     ylab="Distance between patients",
     main="Dendrogram  using Complete Linkage method")



