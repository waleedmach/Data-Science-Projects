library(readr) #for reading csv etc.
library(ggplot2) #for data visualization, very populat package
library(plyr) #for data manipulation
library(dplyr) #for data manipulation
library(reshape2) #for data manipulation / reshaping / transposing etc.
library(sqldf) #to write sql-like queries in in R
library(Hmisc) #for binning numeric features & many other handy tools
library(corrplot) #for correlation plots
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(mice) #for imputation
library(VIM) #for visualizing missing data


training=read_csv(file.choose(), col_name=TRUE,na=c("","NA","#NA"));
#testing=read_csv(file.choose(), col_name=TRUE,na=c("","NA","#NA"));
str(training)


hist(training$TARGET)
table(training$TARGET)

#check number of char variable
charVars <- which(sapply(training, is.character)) #index vector char variables
charVarNames <- names(charVars) #saving names vector for later use
cat('There are', length(charVars), 'char variables')

#no missing values
library(VIM)
aggr_plot = aggr(training, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#checking correlation
# Remove Zero and Near Zero-Variance Predictors
nzv <- nearZeroVar(training[,-371])
dat2 <- training[, -nzv]
dim(dat2)
numericData <- dat2[sapply(dat2, is.numeric)]

# Calculate correlation matrix
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)
summary(descrCor[upper.tri(descrCor)])

# Check Correlation Plot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))


#random forest to check feature's importance
training$TARGET <- as.factor(training$TARGET)
library(randomForest)
library(ggplot2)
quick_RF <- randomForest(x=training[1:76020,-371], y=training$TARGET[1:76020], ntree=10,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")


##reduce columns##

library(caret)
library(corrplot)
library(plyr)

# Remove Zero and Near Zero-Variance Predictors
nzv <- nearZeroVar(training[,-371])
dat2 <- training[, -nzv]
dim(dat2)

# Identifying numeric variables
dat2$TARGET <-ifelse(dat2$TARGET==0,print("Satisfied"),print ("Unsatisfied"))
  
numericData <- dat2[sapply(dat2, is.numeric)]

# Calculate correlation matrix
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)
summary(descrCor[upper.tri(descrCor)])

# Check Correlation Plot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# Identifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]

# Print highly correlated attributes
highlyCorCol

# Remove highly correlated variables and create a new dataset
dat3 <- dat2[, -which(colnames(dat2) %in% highlyCorCol)]
dim(dat3)



##split data train vs testing
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071")
set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
dat3$TARGET <- as.factor(dat3$TARGET)
inTrain <- createDataPartition(y = dat3$TARGET,
                               p = 0.8, list = FALSE)
training_split <- dat3[ inTrain,]
testing_split <- dat3[ -inTrain,]
# hist(training_split$TARGET)
# table(training_split$TARGET)

#write.csv(training_split, file = 'training_split.csv', row.names = F)

##Imbalanced data on training data only
#install.packages("ROSE")
library(ROSE)

training_split_over <- ovun.sample(TARGET ~ ., data = training_split, method = "over", N=85000)$data

# table(training_split_over$TARGET)

###logistic model###

model_logistic<-glm(TARGET~ ., data=training_split_over, family="binomial"(link="logit"))

summary(model_logistic) 


##The model clearly has too many variables, most of which are insignificant 

## Stepwise regressions. There are three aproaches to runinng stepwise regressions: backward, forward and "both"
## In either approach we need to specify criterion for inclusion/exclusion. Most common ones: based on information criterion (e.g., AIC) or based on significance  
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1)
summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

###Finding predictions: probabilities and classification
logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=testing_split,type="response") #Predict probabilities
logistic_classification<-rep("1",500)
logistic_classification[logistic_probabilities<0.70]="0" 
logistic_classification<-as.factor(logistic_classification)


####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing_split$TARGET)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#####Random forest

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","randomForest") #Check, and if needed install the necessary packages

# hyperparameter
# Number of trees in the forest=500
# Number of random columns to grow=50
# Min number of data points on the leaf of each tree=20
# Maximum number of leafs of a tree=20
# How the voting works:  (0.7, 0.3) means majority vote

model_forest <- randomForest(TARGET~ ., data=training_split_over, 
                             type="classification",
                             importance=TRUE,
                             ntree = 500,           
                             mtry = 50,             
                             nodesize = 20,        
                             maxnodes = 20,        
                             cutoff = c(0.7, 0.3)   
) 

plot(model_forest)  # plots error as a function of number of trees in the forest; use print(model_forest) to print the values on the plot

varImpPlot(model_forest) # plots variable importances; use importance(model_forest) to print the values


###Finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest,newdata=testing_split,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
forest_classification<-rep("1",500)
forest_classification[forest_probabilities[,2]<0.70]="0" #Predict classification using 0.7 threshold. Why 0.7 and not 0.6073? Use the same as in cutoff above
forest_classification<-as.factor(forest_classification)

#confusionMatrix(forest_classification,testing_split$TARGET, positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%

#There is also a "shortcut" forest_prediction<-predict(model_forest,newdata=testing, type="response") 
#But it by default uses threshold of 50%: actually works better (more accuracy) on this data


####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing_split$TARGET) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value


##xgboost

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","xgboost") #Check, and if needed install the necessary packages



STCdata_A_matrix <- model.matrix(TARGET~ ., data = dat3)[,-1]

x_train <- STCdata_A_matrix[ inTrain,]
x_test <- STCdata_A_matrix[ -inTrain,]

y_train <-as.factor(training_split$TARGET)
length(y_train)
y_test <-as.factor(testing_split$TARGET)
y_train
as.numeric(as.factor(y_train))

# Hyperparameters
# Learning rate=0.1
# Size of a tree in each boosting iteration=20
# Number of boosting iterations=50

model_XGboost<-xgboost(data = data.matrix(x_train), 
                       label =as.numeric(as.factor(y_train))-1, 
                       eta = 0.1,        
                       max_depth = 20,  
                       nround=50,      
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost,newdata=x_test, type="response") #Predict classification (for confusion matrix)
#confusionMatrix(as.factor(ifelse(XGboost_prediction>0.7,1,0)),y_test,positive="1") #Display confusion matrix

####ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, y_test) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value



plot(logistic_ROC, col = 1, lty = 1, lwd=5, main = "ROC")
# to add to the same graph: add=TRUE
plot(forest_ROC, col = 2, lty = 1, lwd=5, add = TRUE)
plot(XGboost_ROC_testing, col = 3, lty = 1, lwd=5, add = TRUE)
