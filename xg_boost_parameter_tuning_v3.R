# XGBoost

#Define the source file directory, where the file is located.
# Two conditions: 1) dataset with remove outliers and 2) dataset keep outliers
setwd("D:/")

# Condition 1: Training and testing the model using dataset remove outliers
# Importing the datasets for train, test and out-of-sample
# same codes used for dataset with outliers

train_RE = read.csv('train_remove_outlier.csv')
test_RE = read.csv('test_remove_outlier.csv')
OOS_RE = read.csv('oos_remove_outlier.csv')


# Fitting XGBoost model to the Train set for model training process
# Install.packages('xgboost') and import XGboost
# Install.packages('xgboost')
library(xgboost)


# Model 1: Vanilla xgboost with No of Trees = 100
classifier = xgboost(data = as.matrix(train_RE[-1]), label = train_RE$signal, nrounds = 80)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(OOS_RE[-1]))
y_pred = (y_pred >= 0.5)

# Making the Confusion Matrix
cm = table(OOS_RE[, 1], y_pred)
cm

# Classification Rate
#1) Accuracy:

accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy  

#2) Recall

Recall = cm[1,1]/(CM[1,1] + cm[1,2])

#3) Precision

Precision = cm[1,1]/(CM[1,1] + cm[2,1])

#4) F score
F_Score = ((2*Recall)*(Precision))/(Recall+Precision)
F_Score  

###########################################################################################

#Condition 2) Vanilla xgboost with No of Trees = 300
classifier1 = xgboost(data = as.matrix(train_RE[-1]), label = train_RE$signal, nrounds = 300)

# Predicting the Test set results
y_pred1 = predict(classifier1, newdata = as.matrix(test_RE[-1]))
y_pred1 = (y_pred1 >= 0.5)

# Making the Confusion Matrix
cm = table(test_RE[, 1], y_pred1)
cm

accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy  

##########################################################################################

# Condition 3) Xgboost parameters with parameter tuning learning rate=0.1


xgb_params <- list(colsample_bytree = 0.7, #how many variables to consider for each tree
                   subsample = 0.7, #how much of the data to use for each tree
                   booster = "gbtree",
                   max_depth = 5, #how many levels in the tree
                   eta = 0.1, #shrinkage rate to control overfitting through conservative approach
                   eval_metric = "AUC", 
                   objective = "binary:logistic",
                   gamma = 0)

###########################################################################################

# Conditon 4) Xgboost parameters with parameter tuning learning rate=0.3
xgb_params <- list(colsample_bytree = 0.7, #how many variables to consider for each tree
                   subsample = 0.7, #how much of the data to use for each tree
                   booster = "gbtree",
                   max_depth = 5, #how many levels in the tree
                   eta = 0.1, #shrinkage rate to control overfitting through conservative approach
                   eval_metric = "AUC", 
                   objective = "binary:logistic",
                   gamma = 0)

#Training the model

gb_dt = xgboost(xgb_params,data = as.matrix(train_RE[-1]), label = train_RE$signal,nrounds = 80)
gb_dt$params

#Predict using test set

y_pred1 = predict(gb_dt, newdata = as.matrix(OOS_RE[-1]))
y_pred1 = (y_pred1 >= 0.5)

cm = table(OOS_RE[, 1], y_pred1)
cm

#Classification Rate
#1) Accuracy:
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy  

################################# predicting the best performing model using OOS accuracy

#Predict using OOS set with remode outliers

y_pred1 = predict(gb_dt, newdata = as.matrix(OOS_RE[-1]))
y_pred1 = (y_pred1 >= 0.5)

cm = table(OOS_RE[, 1], y_pred1)
cm

#Classification Rate
#1) Accuracy:
accuracy1 = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy1  


#2) Recall

Recall = cm[1,1]/(CM[1,1] + cm[1,2])

#3) Precision

Precision = cm[1,1]/(CM[1,1] + cm[2,1])

#4) F score
F_Score = ((2*Recall)*(Precision))/(Recall+Precision)
F_Score

##################################################### DATASET KEEP OUTLIERS ############################################################################
###################################################################################################################################################
#################################################################################################################################################

# Importing the dataset that contains outliers

train_KE = read.csv('train_keep_outlier.csv')
test_KE = read.csv('test_keep_outlier.csv')
OOS_KE = read.csv('oos_keep_outlier.csv')


# Fitting XGBoost to the Training set
# install.packages('xgboost')
library(xgboost)

#Condition 1) Vanilla xgboost( without any hyperparameter tuning)
classifier = xgboost(data = as.matrix(train_KE[-1]), label = train_KE$signal, nrounds = 80)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_KE[-1]))
y_pred = (y_pred >= 0.5)

# Making the Confusion Matrix
cm = table(test_KE[, 1], y_pred)
cm

accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy  

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(OOS_KE[-1]))
y_pred = (y_pred >= 0.5)

# Making the Confusion Matrix
cm = table(OOS_KE[, 1], y_pred)
cm

accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy

#################################################################################################################

#Condition 2) Vanilla xgboost with No of Trees = 300
classifier1 = xgboost(data = as.matrix(train_KE[-1]), label = train_RE$signal, nrounds = 200)

# Predicting the Test set results
y_pred1 = predict(classifier1, newdata = as.matrix(test_KE[-1]))
y_pred1 = (y_pred1 >= 0.5)

# Making the Confusion Matrix
cm = table(test_RE[, 1], y_pred1)
cm

accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy  

####################################################################################################################

#Condition 3) Xgboost with parameter tuning
library(caret)

## 3.1) xgboost parameters (increase the no. of trees to 500)
xgb_params <- list(colsample_bytree = 0.7, #how many variables to consider for each tree
                   subsample = 0.7, #how much of the data to use for each tree
                   booster = "gbtree",
                   max_depth = 5, #how many levels in the tree
                   eta = 0.1, #shrinkage rate to control overfitting through conservative approach (learning rate)
                   eval_metric = "AUC", 
                   objective = "binary:logistic",
                   gamma = 0)#default at 0,controls regularization to avoid overfitting

##3.2) xgboost parameters (increase the no. of trees to 500) with learning rate =0.3
xgb_params <- list(colsample_bytree = 0.7, #how many variables to consider for each tree
                   subsample = 0.7, #how much of the data to use for each tree
                   booster = "gbtree",
                   max_depth = 5, #how many levels in the tree
                   eta = 0.1, #shrinkage rate to control overfitting through conservative approach (learning rate)
                   eval_metric = "AUC", 
                   objective = "binary:logistic",
                   gamma = 0)#default at 0,controls regularization to avoid overfitting

#train data
gb_dt = xgboost(xgb_params,data = as.matrix(train_KE[-1]), label = train_KE$signal,nrounds = 80)
gb_dt$params

#Predict using test set
y_pred1 = predict(gb_dt, newdata = as.matrix(test_KE[-1]))
y_pred1 = (y_pred1 >= 0.5)

cm = table(test_KE[, 1], y_pred1)
cm

accuracy1 = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy1 


##################################### Predict the model accuarcy on the OOS dataset (keep outliers)

#Predict using test set
y_pred1 = predict(gb_dt, newdata = as.matrix(OOS_KE[-1]))
y_pred1 = (y_pred1 >= 0.5)

cm = table(OOS_KE[, 1], y_pred1)
cm

accuracy1 = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy1 
