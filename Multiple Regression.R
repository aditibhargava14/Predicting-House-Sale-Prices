Train<-read.csv("/Users/tejaswininutalapati/Documents/Multivariate Analysis/Project/DataSet/train.csv")
Test<-read.csv("/Users/tejaswininutalapati/Documents/Multivariate Analysis/Project/DataSet/test.csv")

Test$SalePrice <- -1
df <- rbind(Train,Test)

str(df)

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
library(VIM);
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

library(DescTools)
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

library(ggplot2)
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

library(outliers)
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

PredModel <- ~-1+log(LotArea)+Street+LotShape+LandContour+LotConfig+LandSlope+Neighborhood+BldgType
dpredict <- xgb.DMatrix(data = sparse.model.matrix(PredModel,data=df[is.na(df$LotFrontage),]))

str(df)

#We have cleaned the dataset, imputed the missing values

library(car)
library(lubridate) 
library(ggplot2)
library(GGally)
library(qqplotr)

fit <- lm (SalePrice~MSZoning+I(LotArea^2)+Street+
              LotConfig+LandSlope+Condition1+OverallQual+OverallCond+
              RoofStyle+RoofMatl+MasVnrArea+ExterQual+BsmtQual+BsmtExposure+BsmtFinSF2 +
              BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+KitchenQual+GarageQual+
              ScreenPorch+PoolArea+PoolQC,data=df) 
summary(fit)
coefficients(fit)

confint(fit,level=0.95)
# Predicted Values
fitted(fit)
residuals(fit)
#Anova Table
anova(fit)
vcov(fit)
cov2cor(vcov(fit))
temp <- influence.measures(fit)
temp
View(temp)
#diagnostic plots
plot(fit)
# Assessing Outliers
outlierTest(fit)
qqPlot(fit, main="QQ Plot")
# leverage plots
leveragePlots(fit) 
# Influential Observations
# added variable plots
avPlots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(df)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
#Non-constant Error Variance
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)
#Nonlinearity
# component + residual plot
crPlots(fit)
# Ceres plots
###########ceresPlots(fit)
#Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit)
# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)
fit
summary(fit)
fit1 <- fit

#show the results
fit2 <- lm (SalePrice~MSZoning+I(LotArea^2)+Street+
             RoofStyle+RoofMatl+MasVnrArea+ExterQual+BsmtQual+BsmtExposure+BsmtFinSF2 +
             BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+KitchenQual+GarageQual+
             ScreenPorch+PoolArea+PoolQC,data=df) 
# compare models
anova(fit1, fit2)
step <- stepAIC(fit, direction="both")
step$anova # display results
library(leaps)
leaps<-regsubsets(SalePrice~MSZoning+I(LotArea^2)+Street+
                    RoofStyle+RoofMatl+MasVnrArea+ExterQual+BsmtQual+BsmtExposure+BsmtFinSF2 +
                    BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+KitchenQual+GarageQual+
                    ScreenPorch+PoolArea+PoolQC,data=df,nbest=10)
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps)
plot(leaps,scale="r2")
subsets(leaps, statistic="rsq")
# All Subsets Regression
plot(leaps,scale="bic")
summary(leaps)
View(leaps)
leaps
coef(leaps,1:5)
# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),rela=TRUE)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit, b = 1000, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result
summary(fit)
predict.lm(fit, data.frame(wt =3.2 ,drat=3.9,hp=130,disp=150) )



library(knitr)
