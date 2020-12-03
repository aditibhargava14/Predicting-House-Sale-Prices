#loading the libraries 

library(reshape2)
library(dplyr)
library(ggplot2) 
library(lattice)
library(caret)
library(scales)
library(dummies)
library(fmsb)
library(randomForest)
library(DescTools)
library(outliers)
library(VIM)
library(GGally)
library(corrplot)

# Loading the dataset

list.files("../input")

Train<-read.csv("/Users/tejaswininutalapati/Documents/Multivariate Analysis/Project/DataSet/train.csv")
Test<-read.csv("/Users/tejaswininutalapati/Documents/Multivariate Analysis/Project/DataSet/test.csv")

# Add sale price new column in test dataset 
Test["SalePrice"] <- NA

# Let's explore the structure of the data
dim(Train)
str(Train)
dim(Test)
str(Test)
#The categorical variables are stored as factors in our dataframe. 

# Combining the dataset
Test$SalePrice <- -1
df <- rbind(Train,Test)
str(df)
summary(df)

#finding how many variables with missing values are in the dataset
options(repr.plot.width=6, repr.plot.height=5)
cMiss = function(x){sum(is.na(x))}
CM <- sort(apply(df,2,cMiss),decreasing=T);
barplot(CM[CM!=0],
        las=2,
        cex.names=0.6,
        ylab="Count",
        ylim=c(0,3000),
        horiz=F,
        col="#AFC0CB",
        main=paste(toString(sum(CM!=0)), "variables with missing values in dataset"))

dfClean <-function(df)
{
  # Pool Variable: If PoolQC = NA and PoolArea = 0 , assign factor NoPool
  df$PoolQC <- as.character(df$PoolQC)
  df$PoolQC[df$PoolArea %in% c(0,NA) & is.na(df$PoolQC)] <- "NoPool"
  df$PoolQC <- as.factor(df$PoolQC)
  
  # MiscFeature Variable: If MiscFeature = NA and MiscVal = 0, assign factor None
  df$MiscFeature <- as.character(df$MiscFeature)
  df$MiscFeature[df$MiscVal %in% c(0,NA) & is.na(df$MiscFeature)] <- "None"
  df$MiscFeature <- as.factor(df$MiscFeature)
  
  # Alley Variable: If Alley = NA, assign factor NoAccess
  df$Alley <- as.character(df$Alley)
  df$Alley[is.na(df$Alley)] <- "NoAccess"
  df$Alley <- as.factor(df$Alley)    
  
  # Fence Variable: If Fence = NA, assign factor NoFence
  df$Fence <- as.character(df$Fence)
  df$Fence[is.na(df$Fence)] <- "NoFence"
  df$Fence <- as.factor(df$Fence)
  
  # FireplaceQu Variable: If FireplaceQu = NA and Fireplaces = 0 , assign factor NoFirePlace
  df$FireplaceQu <- as.character(df$FireplaceQu)
  df$FireplaceQu[df$Fireplaces %in% c(0,NA) & is.na(df$FireplaceQu)] <- "NoFirePlace"
  df$FireplaceQu <- as.factor(df$FireplaceQu)   
  
  # GarageYrBlt Variable: If GarageYrBlt = NA and GarageArea = 0 assign factor NoGarage    
  df$GarageYrBlt <- as.character(df$GarageYrBlt)
  df$GarageYrBlt[df$GarageArea %in% c(0,NA) & is.na(df$GarageYrBlt)] <- "NoGarage"
  df$GarageYrBlt <- as.factor(df$GarageYrBlt)
  
  # GarageFinish Variable: If GarageFinish = NA and GarageArea = 0 assign factor NoGarage    
  df$GarageFinish <- as.character(df$GarageFinish)
  df$GarageFinish[df$GarageArea %in% c(0,NA) & is.na(df$GarageFinish)] <- "NoGarage"
  df$GarageFinish <- as.factor(df$GarageFinish)
  
  # GarageQual Variable: If GarageQual = NA and GarageArea = 0 assign factor NoGarage    
  df$GarageQual <- as.character(df$GarageQual)
  df$GarageQual[df$GarageArea %in% c(0,NA) & is.na(df$GarageQual)] <- "NoGarage"
  df$GarageQual <- as.factor(df$GarageQual)
  
  # GarageCond Variable: If GarageCond = NA and GarageArea = 0 assign factor NoGarage    
  df$GarageCond <- as.character(df$GarageCond)
  df$GarageCond[df$GarageArea %in% c(0,NA) & is.na(df$GarageCond)] <- "NoGarage"
  df$GarageCond <- as.factor(df$GarageCond)
  
  # GarageType Variable: If GarageType = NA and GarageArea = 0 assign factor NoGarage    
  df$GarageType <- as.character(df$GarageType)
  df$GarageType[df$GarageArea %in% c(0,NA) & is.na(df$GarageType)] <- "NoGarage"
  df$GarageType <- as.factor(df$GarageType)
  df$GarageArea[is.na(df$GarageArea) & df$GarageCars %in% c(0,NA)] <- 0
  df$GarageCars[is.na(df$GarageCars) & df$GarageArea %in% c(0,NA)] <- 0    
  
  # BsmtFullBath Variable: If BsmtFullBath = NA and TotalBsmtSF = 0 assign 0    
  df$BsmtFullBath[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtFullBath)] <- 0
  
  # BsmtHalfBath Variable: If BsmtHalfBath = NA and TotalBsmtSF = 0 assign 0   
  df$BsmtHalfBath[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtHalfBath)] <- 0
  
  # BsmtFinSF1 Variable: If BsmtFinSF1 = NA and TotalBsmtSF = 0 assign 0    
  df$BsmtFinSF1[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtFinSF1)] <- 0
  
  # BsmtFinSF2 Variable: If BsmtFinSF2 = NA and TotalBsmtSF = 0 assign 0   
  df$BsmtFinSF2[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtFinSF2)] <- 0
  
  # BsmtUnfSF Variable: If BsmtUnfSF = NA and TotalBsmtSF = 0 assign 0    
  df$BsmtUnfSF[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtUnfSF)] <- 0
  
  # TotalBsmtSF Variable: If TotalBsmtSF = NA and TotalBsmtSF = 0 assign 0   
  df$TotalBsmtSF[df$TotalBsmtSF %in% c(0,NA) & is.na(df$TotalBsmtSF)] <- 0
  
  # BsmtQual Variable: If BsmtQual = NA and TotalBsmtSF = 0 assign factor NoBasement    
  df$BsmtQual <- as.character(df$BsmtQual)
  df$BsmtQual[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtQual)] <- "NoBasement"
  df$BsmtQual <- as.factor(df$BsmtQual)
  
  # BsmtFinType1 Variable: If BsmtFinType1 = NA and TotalBsmtSF = 0 assign factor NoBasement    
  df$BsmtFinType1 <- as.character(df$BsmtFinType1)
  df$BsmtFinType1[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtFinType1)] <- "NoBasement"
  df$BsmtFinType1 <- as.factor(df$BsmtFinType1)    
  
  # BsmtFinType2 Variable: If BsmtFinType2 = NA and TotalBsmtSF = 0 assign factor NoBasement    
  df$BsmtFinType2 <- as.character(df$BsmtFinType2)
  df$BsmtFinType2[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtFinType2)] <- "NoBasement"
  df$BsmtFinType2 <- as.factor(df$BsmtFinType2)
  
  # BsmtExposure Variable: If BsmtExposure = NA and TotalBsmtSF = 0 assign factor NoBasement    
  df$BsmtExposure <- as.character(df$BsmtExposure)
  df$BsmtExposure[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtExposure)] <- "NoBasement"
  df$BsmtExposure <- as.factor(df$BsmtExposure)
  
  # BsmtCond Variable: If BsmtCond = NA and TotalBsmtSF = 0 assign factor NoBasement    
  df$BsmtCond <- as.character(df$BsmtCond)
  df$BsmtCond[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtCond)] <- "NoBasement"
  df$BsmtCond <- as.factor(df$BsmtCond)  
  return(df)    
}
df <- dfClean(df)

PM <- sort(apply(df,2,cMiss),decreasing=T);
barplot(PM[PM!=0],
        las=2,
        cex.names=0.6,
        ylab="Count",
        ylim=c(0,500),
        horiz=F,
        col="#AFC0CB",
        main=paste(toString(sum(PM!=0)), "variables with missing values in dataset"))

#That certainly helped a little bit. Let's see if there's a pattern to the remaining missing data.
data = df[, names(PM[PM!=0])];
aggr_plot <- aggr(data,
                  col=c('navyblue','red'),
                  bars=T,
                  numbers=T,
                  combined = T,
                  labels=names(data),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Pattern"),
                  cex.numbers=0.74)

#MasVnrType and MasVnrArea
plot(df[,c("MasVnrType","MasVnrArea")],
     pch=16,
     notch=TRUE,
     main="MasVnrArea vs MasVnrType boxplots",
     col="#AFC0CB")
df[ (is.na(df$MasVnrType) | is.na(df$MasVnrArea)) ,c("MasVnrType","MasVnrArea")]
summary(df[ !(is.na(df$MasVnrType) | is.na(df$MasVnrArea)) ,c("MasVnrType","MasVnrArea")])
df$MasVnrType <- as.character(df$MasVnrType)
df$MasVnrType[is.na(df$MasVnrType)] <- "None"
df$MasVnrType <- as.factor(df$MasVnrType)  
df$MasVnrArea[is.na(df$MasVnrArea)] <- 0

#MSZoning
plot(df$MSZoning,
     col="#AFC0CB",
     xlab="Zoning Classification",
     ylab = "Count",
     main = "Barplot for zoning classifications")
df[ is.na(df$MSZoning) ,c("MSZoning","MSSubClass")]
ZoneClassTable <- table(df[ ,c("MSZoning","MSSubClass")])
ZoneClassTable
mosaicplot(ZoneClassTable,
           main="Mosaic Plot of MSZoning VS MSSubClass",
           las=1,
           color=T,
           shade=T)
GTest(ZoneClassTable)
Table<-table(df[ df$MSSubClass %in% c(30,70) ,c("MSZoning","MSSubClass")])
Table <- Table[ , colSums(Table != 0) > 0 ]
Table
mosaicplot(Table,
           main="Mosaic Plot of MSZoning VS MSSubClass (30,70)",
           las=1,
           color=T,
           shade=T)
Test1<-GTest(Table)
Test1
paste("At a 95% confidence level, since the p-value =", as.character(round(Test1$p.value,2)),
      "> 0.05, we cannot reject the null hypothesis that MSZoning and MSSubClass are independent when MSSubClass = 30 or 70.")
df$MSZoning <- as.character(df$MSZoning)
df$MSZoning[is.na(df$MSZoning)] <- "RL"
df$MSZoning <- as.factor(df$MSZoning)

#Basement
MissBsmt = c('BsmtCond','BsmtExposure','BsmtQual','BsmtFinType2')
df[!complete.cases(df[,names(df) %in% MissBsmt]),names(df) %in% names(df)[which(grepl("Bsmt",names(df)))]]

#BsmtExposure
df$BsmtExposure <- as.character(df$BsmtExposure)
df$BsmtExposure[is.na(df$BsmtExposure)]<-"No"
df$BsmtExposure <- as.factor(df$BsmtExposure)

#BsmtFinType2
BsmtFinQuality<-table(df[ !(df$BsmtFinType2 %in% c("NoBasement","Unf") | df$BsmtFinType1 %in% c("NoBasement","Unf")) ,c("BsmtFinType2","BsmtFinType1")])
BsmtFinQuality<-BsmtFinQuality[rowSums(BsmtFinQuality != 0) > 0 , colSums(BsmtFinQuality != 0) > 0]
BsmtFinQuality
mosaicplot(BsmtFinQuality,
           main="Mosaic Plot of BsmtFinType",
           las=1,
           color=T,
           shade=T)
TestQ<-GTest(BsmtFinQuality)
TestQ
plot(df[df$BsmtFinType2 %in% c("ALQ","LwQ", "Rec"),c("BsmtFinType2","BsmtFinSF2")],
     pch=16,
     notch=TRUE,
     main="BsmtFinSF2 vs BsmtFinType2 boxplots",
     col="#AFC0CB")
abline(h=df[is.na(df$BsmtFinType2) ,c("BsmtFinSF2")])
df$BsmtFinType2 <- as.character(df$BsmtFinType2)
df$BsmtFinType2[is.na(df$BsmtFinType2)]<-"ALQ"
df$BsmtFinType2 <- as.factor(df$BsmtFinType2)

#BsmtQual
BsmtQualUnf<-table(df$BsmtQual[df$BsmtUnfSF==df$TotalBsmtSF & df$TotalBsmtSF>0],df$HouseStyle[df$BsmtUnfSF==df$TotalBsmtSF & df$TotalBsmtSF>0])
BsmtQualUnf<-BsmtQualUnf[rowSums(BsmtQualUnf != 0) > 0 , colSums(BsmtQualUnf != 0) > 0]
BsmtQualUnf
mosaicplot(BsmtQualUnf,
           main="Mosaic Plot of Basement Quality",
           las=1,
           color=T,
           shade=T)
TestQ2<-GTest(BsmtQualUnf)
TestQ2
df$HouseStyle[is.na(df$BsmtQual)]
df$BsmtQual <- as.character(df$BsmtQual)
df$BsmtQual[is.na(df$BsmtQual) & df$HouseStyle == "2Story"]<-"Gd"
df$BsmtQual[is.na(df$BsmtQual) & df$HouseStyle == "1.5Fin"]<-"TA"
df$BsmtQual <- as.factor(df$BsmtQual)

#BsmtCond
TableBsmtCond<-table(df$HouseStyle,df$BsmtCond)
TableBsmtCond<-TableBsmtCond[rowSums(TableBsmtCond != 0) > 0 , colSums(TableBsmtCond != 0) > 0]
TableBsmtCond
mosaicplot(TableBsmtCond,
           main="Mosaic Plot of Basement Quality",
           las=1,
           color=T,
           shade=T)
TestQ2<-GTest(TableBsmtCond)
TestQ2
df$HouseStyle[is.na(df$BsmtCond)]
df$BsmtCond <- as.character(df$BsmtCond)
df$BsmtCond[is.na(df$BsmtCond)]<-"TA"
df$BsmtCond <- as.factor(df$BsmtCond)

PM <- sort(apply(df,2,cMiss),decreasing=T);
barplot(PM[PM!=0],
        las=2,
        cex.names=0.6,
        ylab="Count",
        ylim=c(0,500),
        horiz=F,
        col="#AFC0CB",
        main=paste(toString(sum(PM!=0)), "variables with missing values in dataset"))
data = df[, names(PM[PM!=0])];
aggr_plot <- aggr(data,
                  col=c('navyblue','red'),
                  bars=T,
                  numbers=T,
                  combined = T,
                  labels=names(data),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Pattern"),
                  cex.numbers=0.74)

#The rest
fillMiss<- function(x)
{
  ux <- unique(x[!is.na(x)])
  x <- as.character(x)
  mode <- ux[which.max(tabulate(match(x[!is.na(x)], ux)))]
  x[is.na(x)] <- as.character(mode)
  x <- as.factor(x)
  return(x)
}
df[,sapply(df,function(x){!(is.numeric(x))}) ]<-as.data.frame(apply(df[,sapply(df,function(x){!(is.numeric(x))}) ],2,fillMiss))
PM <- sort(apply(df,2,cMiss),decreasing=T);
barplot(PM[PM!=0],
        las=2,
        cex.names=0.6,
        ylab="Count",
        ylim=c(0,500),
        horiz=F,
        col="#AFC0CB",
        main=paste(toString(sum(PM!=0)), "variables with missing values in dataset"))
data = df[, names(PM[PM!=0])];
aggr_plot <- aggr(data,
                  col=c('navyblue','red'),
                  bars=T,
                  numbers=T,
                  combined = T,
                  labels=names(data),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Pattern"),
                  cex.numbers=0.74)

#LotFrontage Imputation
#Let's investigate this variable further. Maybe we could use a regression imputation technique to impute the missing LotFrontage variables.
#Since LotFrontage is defined as "Linear feet of street connected to property", we would suspect that this variable would be related to quantities like "LotArea", "Street", "LotShape", "LandContour", "LotConfig", "LandSlope", "Neighborhood", "BldgType".
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) 
{
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) 
  {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) 
  {
    print(plots[[1]])
  } 
  else 
  {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) 
    {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
p1<-ggplot(df, aes(LotArea, LotFrontage)) + geom_point() + geom_smooth(method = "lm", se = T) 
p2<-ggplot(df, aes(log(LotArea), LotFrontage)) + geom_point() + geom_smooth(method = "lm", se = T)
p3<-ggplot(df, aes(log(LotArea), log(LotFrontage))) + geom_point() + geom_smooth(method = "lm", se = T)
p4<-ggplot(df, aes(sqrt(LotArea), LotFrontage)) + geom_point() + geom_smooth(method = "lm", se = T)
multiplot(p1, p2, p3, p4, cols=2)

#To check outliers
chisq.out.test(df$LotArea,opposite=F)
chisq.out.test(df$LotFrontage,opposite=F)
chisq.out.test(df$LotArea,opposite=T)
chisq.out.test(df$LotFrontage,opposite=T)
grubbs.test(df$LotArea,type=11)
grubbs.test(df$LotFrontage,type=11)

p1<-ggplot(df  , aes(LotArea, LotFrontage)) + geom_point() + geom_smooth(method = "lm", se = T) 
p2<-ggplot(df, aes(log(LotArea), LotFrontage)) + geom_point() + geom_smooth(method = "lm", se = T)
p3<-ggplot(df, aes(log(LotArea), log(LotFrontage))) + geom_point() + geom_smooth(method = "lm", se = T)
p4<-ggplot(df, aes(sqrt(LotArea), LotFrontage)) + geom_point() + geom_smooth(method = "lm", se = T)
multiplot(p1, p2, p3, p4, cols=2)

cor(as.numeric(df$LotArea),as.numeric(df$LotFrontage),use="complete.obs")
cor(log(as.numeric(df$LotArea)),log(as.numeric(df$LotFrontage)),use="complete.obs")
cor(log(as.numeric(df$LotArea)),as.numeric(df$LotFrontage),use="complete.obs")
cor(sqrt(as.numeric(df$LotArea)),as.numeric(df$LotFrontage),use="complete.obs")

str(df)

#splitting back to Test and Train
Traindata<-df[1:1460,]
Testdata<-df[(1461):nrow(df),]
#Testdata<- testdata[ , -which(names(Testdata) %in% c("SalePrice"))]

str(Testdata)
str(Traindata)
# We have cleaned all of the data


## Exploratory Data Analysis


# Here are the steps that we will take to understand the data and variables to get a better understanding of our 
#data. 
#Let's look at each individual variables carefully to understand the meaning of the variable and it's importance

#Analyze the dependent variable sale price. 

ggplot(Traindata, aes(factor(OverallQual),YearBuilt)) + geom_boxplot() +xlab("Overall Quality")
#Most recently built homes have better overall quality. Overall quality rates the material and finish of homes. 

ggplot(Traindata, aes(factor(OverallCond),YearBuilt)) + geom_boxplot() +xlab("Overall Condition")

#The recently built homes have better Qverall Quality, but the Overall condition of these recently built homes is worse than
#the old homes. Newer built homes are of mediocre quality. 

#Let's plot the correlation matrix of numeric variables in the dataset 

train_num <- Traindata[sapply(Traindata,is.numeric)]

correlations <- cor(na.omit(train_num))
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

#Let's make some scatter plot for some of the high correlation variables. High correlation variables: 

#OverallQual: Rates the overall material and finish of the house 1-10. 
#YearBuilt: Year house was built
#MasVnrArea: Masonary veener area in square feet
#TotalBsmtSF: Total Square feet of basement Area
#X1stFlrSF: First floor Square feet
#GrLivArea: Ground Living Area
#FullBath: Full Bathrooms above grade
#TotRmsAbvGrd: Total rooms above grade (doesn't include bathrooms)
#GarageCars: Size of garage in car capacity
#GarageArea: Size of garage in square feet

makeScatterplots <- function(dataframe,x.variable, y.variable, xlabel, ylabel)
{
  p = ggplot(dataframe, aes_string(x=x.variable,y= y.variable)) + 
    geom_point() + 
    geom_smooth(method=lm, se=FALSE) + 
    ylab(ylabel) + 
    xlab(paste(xlabel,'\n', 'R-Squared:', round(cor(x.variable, y.variable), 2))) + 
    theme_light() + 
    scale_x_continuous(labels = comma) + 
    scale_y_continuous(labels = comma)
  return(p)
}
makeScatterplots(train_num, train_num$YearBuilt, train_num$SalePrice, "Year Built", "Sale Price") 

#There are a few recently built homes that are outliers and have much higher sale price. 
makeScatterplots(train_num, train_num$X1stFlrSF, train_num$SalePrice, "First floor Square feet", "Sale Price")

#A majority of homes are under $200,000 and Average square foot of first floor in homes is ~1163 sq.feet. 
makeScatterplots(train_num, train_num$GrLivArea, train_num$SalePrice, "Ground Living Area", "Sale Price")

#Living areas tend to be around an average of 1500 sq.feet for most homes. 
makeScatterplots(train_num, train_num$FullBath, train_num$SalePrice, "Full Bath", "Sale Price") 
makeScatterplots(train_num, train_num$TotRmsAbvGrd, train_num$SalePrice, "Total rooms above grade", "Sale Price")
makeScatterplots(train_num, train_num$GarageArea, train_num$SalePrice, "Garage Area", "Sale Price")

#The sale prices of home is higher for garage areas between 750 to 1000 sq.feet. However, there are a few outliers where sale price drops for homes where garage area is greater than ~1000 sq.feet. 
ggplot(Traindata, aes(x=YrSold, y=SalePrice)) + stat_summary(fun.y="mean", geom="bar")


#Notice the drop in average sale of home price in year 2008, the housing market bubble crashed when Case-Shiller home price index reported it's largest price drop. 
#Categorical Variables 
# Let's make some barplots for categorical variables to get a deeper insight / understanding of our data. 
makeBarplots <- function(dataframe,x.variable, xlabel, ylabel)
  {
  p = ggplot(dataframe, aes(x=factor(x.variable))) + 
  geom_bar(stat = "count", width=0.7, fill="steelblue") + 
  ylab(ylabel) + xlab(xlabel) + 
  theme_light()
  return(p)
}
makeBarplots(Traindata, Traindata$MSZoning, "MSZoning", "Count")

# An overwhelming majority of homes are in Residential Low Density zone. 
makeBarplots(Traindata, Traindata$Street, "Street", "Count")
makeBarplots(Traindata, Traindata$Alley, "Alley", "Count")
makeBarplots(Traindata, Traindata$LotShape, "Lot Shape", "Count")
makeBarplots(Traindata, Traindata$LandContour, "Land Contour", "Count")

ggplot(Traindata, aes(x=factor(Neighborhood))) + 
  geom_bar(stat = "count", width=0.7, fill="steelblue") + 
  ylab("Neighborhoods") + xlab("Count") + 
  theme_light() + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplot(Traindata, aes(x=factor(Neighborhood),y=SalePrice)) + 
  stat_summary(fun.y="mean", geom="bar", fill="steelblue") + 
  ylab("Neighborhoods") + 
  xlab("Avg. Sale Price") + 
  theme_light() + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  scale_y_continuous(labels = comma)

# Area around the Iowa University, neighborhoods of College Creek and just N. Ames, just north of the university have high concentration of homes. The more affluent neighborhoods are NorthRidge, NorthRidge Heights and Stone Brook. 
makeBarplots(Traindata, Traindata$BldgType, "Building Type", "Count")
makeBarplots(Traindata, Traindata$HouseStyle, "House Style", "Count")
makeBarplots(Traindata, Traindata$SaleCondition, "Sale Condition", "Count")

# Let's plot our dependent variable sales price 
summary(Traindata$SalePrice)
ggplot(data=Traindata, aes(SalePrice)) + 
  geom_histogram(col = "white") + 
  theme_light() + 
  xlim(20000, 800000) + 
  xlab("Sale Price")

# Sale Price appears to be heavily skewed. 
#We will log transform the variable to obtain a normal distribution of our dependent variable. 
# This is to maintain positivity of the sale price variable, in all likelihood, sale price of a home will never be a negative value. 

ggplot(data=Traindata, aes(log(SalePrice))) + 
  geom_histogram(col = "white") + 
  theme_light() + 
  xlab("Sale Price") 


## Linear Regression
model <- lm(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF, data = Traindata)
summary(model)
plot(model)


