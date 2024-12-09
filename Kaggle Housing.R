setwd("~/R/Queens/MMA867/Assignment 1/house-prices-advanced-regression-techniques")
Packages <- c("tidyverse", "mice", "readxl", "MASS","caret","car","estimatr","scales","lubridate","tidyr","visdat","glmnet","corrplot","ggrepel")
lapply(Packages, library, character.only = TRUE)
rm(Packages)

install.packages("ggrepel" )
library("ggrepel" )
# Load & Bind -----------------------------------------------------------

##Load the data
train <- read.csv("train.csv",header=TRUE, sep=",", stringsAsFactors = FALSE)
submission <- read.csv("test.csv",header=TRUE, sep=",", stringsAsFactors = FALSE)
head(train)
head(submission)

##Bind the test/train set for cleaning
submission$SalePrice <-"NA" #testset is missing SalePrice
total <- rbind(train,submission)  #bind the data set after matching number of columns

total$SalePrice <- as.numeric(total$SalePrice)


# Clean --------------------------------------------------------


total[total$PoolArea>0 & total$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]


vis_dat(total)
na_columns <- which(colSums(is.na(total)) > 0)
sort(colSums(sapply(total[na_columns], is.na)), decreasing = TRUE) #main columns that have Na's are with the garage and basement, will need to further investigate what is happening here
total.clean <- total # creating a working dataset


## Cleaning out NAs
na_columns <- which(colSums(is.na(total.clean)) > 0)
sort(colSums(sapply(total[na_columns], is.na)), decreasing = TRUE) #main columns that have Na's are with t

total.clean$Alley[which(is.na(total.clean$Alley))] <- "None"
total.clean$BsmtCond[which(is.na(total.clean$BsmtCond))] <- "None"
total.clean$BsmtQual[which(is.na(total.clean$BsmtQual))] <- "None"
total.clean$BsmtExposure [which(is.na(total.clean$BsmtExposure ))] <- "None"
total.clean$BsmtFinType1[which(is.na(total.clean$BsmtFinType1   ))] <- "None"
total.clean$BsmtFinType2[which(is.na(total.clean$BsmtFinType2))] <- "None"
total.clean$FireplaceQu[which(is.na(total.clean$FireplaceQu))] <- "None"
total.clean$GarageQual[which(is.na(total.clean$GarageQual))] <- "None"
total.clean$GarageCond[which(is.na(total.clean$GarageCond))] <- "None"
total.clean$GarageType[which(is.na(total.clean$GarageType))] <- "None"
total.clean$GarageFinish[which(is.na(total.clean$GarageFinish))] <- "None"
total.clean$Fence[which(is.na(total.clean$Fence))] <- "None"
total.clean$PoolQC[which(is.na(total.clean$PoolQC))] <- "None"
total.clean$MiscFeature[which(is.na(total.clean$MiscFeature))] <- "None"


## Cleaning true missing values
total.clean$LotFrontage[which(is.na(total.clean$LotFrontage))] <- 0
total.clean$MasVnrType[which(is.na(total.clean$MasVnrType))] <- "None"
total.clean$MasVnrArea[which(is.na(total.clean$MasVnrArea))] <- 0
total.clean$BsmtFullBath[which(is.na(total.clean$BsmtFullBath))] <- 0
total.clean$BsmtHalfBath[which(is.na(total.clean$BsmtHalfBath))] <- 0
total.clean$Exterior2nd[which(is.na(total.clean$Exterior2nd))] <- "VinylSd"
total.clean$BsmtFinSF1[which(is.na(total.clean$BsmtFinSF1))] <- 0
total.clean$BsmtFinSF2[which(is.na(total.clean$BsmtFinSF2))] <- 0
total.clean$BsmtUnfSF[which(is.na(total.clean$BsmtUnfSF))] <- 0
total.clean$TotalBsmtSF[which(is.na(total.clean$TotalBsmtSF))] <- 0
total.clean$KitchenQual[which(is.na(total.clean$KitchenQual))] <- "TA"
total.clean$GarageCars[which(is.na(total.clean$GarageCars))] <- 0
total.clean$GarageArea[which(is.na(total.clean$GarageArea))] <- 0
total.clean$SaleType[which(is.na(total.clean$SaleType))] <- "WD"
total.clean$Functional[which(is.na(total.clean$Functional))] <- "Typ"
total.clean$Exterior1st[which(is.na(total.clean$Exterior1st))] <- "VinylSd"
total.clean$GarageYrBlt[which(is.na(total.clean$GarageYrBlt))] <- total.clean$YearBuilt[which(is.na(total.clean$GarageYrBlt))]
total.clean$Electrical[which(is.na(total.clean$Electrical))] <- "SBrkr"
total.clean$MSZoning[which(is.na(total.clean$MSZoning))] <- "RL"
total.clean$Utilities[which(is.na(total.clean$Utilities))] <- "AllPub"

# Check to be sure of no missing data
vis_dat(total.clean) 

#Drop untilities
total.clean <- subset(total.clean,select=-c(Utilities))

#Convert numerics to intergers
total.clean[sapply(total.clean, is.numeric)] <- lapply(total.clean[sapply(total.clean, is.numeric)], as.integer)

#convert Sale price to int
total.clean$SalePrice <- as.integer(total.clean$SalePrice)

#Convert to factorsz
total.clean[sapply(total.clean, is.character)] <- lapply(total.clean[sapply(total.clean, is.character)], as.factor)

# Explore --------------------------------------------------------

## Look at Sales Price
ggplot(data=total[!is.na(total$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)


## Check for Correlations
numericVars <- which(sapply(total, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables'
    total_numVar <- total[, numericVars]
    cor_numVar <- cor(total_numVar, use="pairwise.complete.obs") 
    cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
    #select only high corelations
    CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
    cor_numVar <- cor_numVar[CorHigh, CorHigh]
    corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
    
# Main variables appear to be Overall Quality/GRLiveArea/GarageCars/GarageArea/TotalBsmntSF/X1stFlrSF/FUllBath/TotalRmsAbvGrd/YrBuild/Yrremod
    
    
## Overall Quality
ggplot(data=total[!is.na(total$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
      geom_boxplot(col='blue') + labs(x='Overall Quality') +
      scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

##Above Grade Living Area

ggplot(data=total.clean[!is.na(total.clean$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='forest green') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
  

# Break into Test/Train ---------------------------------------------------


#Break clean total into clean test and train
train.clean <- subset(total.clean,Id<=1460)
vis_dat(train.clean)

submission.clean <- subset(total.clean,Id>=1461)
vis_dat(submission.clean)

#split into test and train
prediction.train.set <- subset(train.clean, Id<=1000)
prediction.test.set <- subset(train.clean, Id>1000)
vis_dat(prediction.train.set)
vis_dat(prediction.test.set)

#To see which data points need log() in the model matrix
qplot(LotFrontage, data = total.clean, bins = 50, main = "To check for skewness") #Yes
qplot(LotArea, data = total.clean, bins = 50, main = "To check for skewness") #Yes
qplot(GrLivArea, data = total.clean, bins = 50, main = "To check for skewness") #Yes
qplot(MasVnrArea, data = total.clean, bins = 50, main = "To check for skewness") #Yes
qplot(TotalBsmtSF, data = total.clean, bins = 50, main = "To check for skewness") #No
qplot(GarageArea, data = total.clean, bins = 50, main = "To check for skewness") #Yes
qplot(Neighborhood, data = total.clean, bins = 50, main = "To check for skewness") #No

qplot(SalePrice, data = total.clean, bins = 1000, main = "To check for skewness") #No
qplot(log(SalePrice), data = total.clean, bins = 50, main = "To check for skewness") #No
train.clean %>% 
  ggplot(aes(x = SalePrice)) +
  geom_histogram()


# Base regression ---------------------------------------------------------
reg.house <- lm(log(SalePrice)~MSSubClass +	MSZoning +	LotFrontage +
                  log(LotArea) +	Street + Alley +LotShape + LandContour +	
                  LotConfig +	LandSlope +	Neighborhood + Condition1 +	
                  Condition2 +	BldgType + HouseStyle +	OverallQual +	OverallCond +	
                  YearBuilt +	YearRemodAdd +	RoofMatl +		
                  MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	
                  Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	
                  BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +
                  HeatingQC +	CentralAir	+Electrical +	X1stFlrSF +	X2ndFlrSF	+ LowQualFinSF	+ log(GrLivArea)+
                  BsmtFullBath	+ BsmtHalfBath +	FullBath +	HalfBath	+ BedroomAbvGr +
                  KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd	+ Functional	+ Fireplaces	+ 
                  FireplaceQu +	GarageType +	GarageYrBlt +	GarageFinish +	GarageCars +	
                  GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +
                  OpenPorchSF +	EnclosedPorch +	X3SsnPorch	+ ScreenPorch +	PoolArea	+
                  Fence +	MiscFeature +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition,data=train.clean)
summary(reg.house)

submission.reg <- exp(predict(reg.house,submission.clean))
write.csv(submission.reg,file = "Upload1.csv")

# Final Model -----------------------------------------------------------------

set.seed(50)
y<-log(prediction.train.set$SalePrice)
X <- model.matrix(Id~log(GrLivArea)*Neighborhood+OverallQual*Neighborhood*LotArea
                  +OverallQual*LotArea+OverallQual*MSSubClass*Neighborhood+
                    +YearBuilt*Neighborhood +GarageArea*KitchenQual +
                    MSSubClass +	MSZoning + log(LotArea) +	LotShape + LandContour +	
                    LotConfig +	LandSlope +	Neighborhood + Condition1 +	Condition2 + 
                    HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +
                    RoofMatl +  ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	
                    BsmtFinType1 + BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	
                    TotalBsmtSF +	Heating +  HeatingQC +	CentralAir	+Electrical +	X1stFlrSF +	
                    X2ndFlrSF	+ LowQualFinSF	+ log(GrLivArea)+  BsmtFullBath	+ BsmtHalfBath +	
                    FullBath +	HalfBath	+ BedroomAbvGr +  KitchenQual +	TotRmsAbvGrd	+ 
                    Functional	+ Fireplaces	+ FireplaceQu +	GarageType +	GarageYrBlt +	
                    GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	
                    WoodDeckSF +  OpenPorchSF +	X3SsnPorch	+ ScreenPorch +	PoolArea	+
                    PoolQC	+ Fence +	MiscFeature +	MiscVal +	MoSold +	YrSold +	SaleType +	
                    SaleCondition,total.clean)[,-1]
X<-cbind(total.clean$Id,X)


# split X into testing, trainig/holdout and prediction as before
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>=1461)

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
#plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
#log(penalty.lasso) #see where it was on the graph
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
#coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
mean(abs(lasso.testing-prediction.test.set$SalePrice)/prediction.test.set$SalePrice*100) #calculate and display MAPE

RMSE(lasso.testing,prediction.test.set$SalePrice)
RMSE(log(lasso.testing),log(prediction.test.set$SalePrice))

predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
write.csv(predicted.prices.log.i.lasso, file = "Final_Model_LASSO.csv") # export the predicted prices into a CSV file



# Ridge ---------------------------------------------------------------------


#ridge (alpha=0)
ridge.fit<-glmnet(x = X.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
mean(abs(ridge.testing-prediction.test.set$SalePrice)/prediction.test.set$SalePrice*100) 


# comparing the performance on the testing set, LASSO is better, so use it for prediction
predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
write.csv(predicted.prices.log.i.lasso, file = "Final_Model_Ridge.csv") # export the predicted prices into a CSV file


