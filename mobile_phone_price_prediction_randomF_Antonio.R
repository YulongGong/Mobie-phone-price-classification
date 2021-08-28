#Project Beginnings

#import the necessary packages

library(data.table)
library(ggplot2)
library(glmnet)
library(MLmetrics)
library(nlme)
library(mltools)
library(tidyverse)
library(scales)
library(randomForest)

#import the data
location <- "C:/Users/anton/OneDrive/MSBA/BA810/train.csv"
setwd("C:/Users/anton/OneDrive/MSBA")
mobile_data <- fread(location, stringsAsFactors = T)
set.seed(430)
#set price_range as factor for one hot encoding
mobile_data$price_range <- as.factor(mobile_data$price_range)

#one hot encode the data for price range
mobile_data_one = one_hot(mobile_data,cols='price_range')

#split the data into training and test
mobile_data_one[, test:=0]
mobile_data_one[sample(nrow(mobile_data_one), 300), test:=1] # take 300 random rows and stick them in the test set
# now split
mobile_data_one_test <- mobile_data_one[test==1]
mobile_data_one_train <- mobile_data_one[test==0]

### Train data for each price level randomForest model, setting target variable as a factor
mobile_train_0 <- mobile_data_one_train %>% select(-(price_range_1:test))
mobile_train_0$price_range_0 <- as.factor(mobile_train_0$price_range_0)

mobile_train_1 <- mobile_data_one_train %>% select(-c(price_range_0,(price_range_2:test)))
mobile_train_1$price_range_1 <- as.factor(mobile_train_1$price_range_1)

mobile_train_2 <- mobile_data_one_train %>% select(-c((price_range_0:price_range_1),(price_range_3:test)))
mobile_train_2$price_range_2 <- as.factor(mobile_train_2$price_range_2)

mobile_train_3 <- mobile_data_one_train %>% select(-c((price_range_0:price_range_2),test))
mobile_train_3$price_range_3 <- as.factor(mobile_train_3$price_range_3)

###Test data 
mobile_predictors_test <- mobile_data_one_test %>% select(-(price_range_0:test))
#instantiate test Ys
price_0_test <- mobile_data_one_test %>% select(price_range_0)
price_0_test_f <- as.factor(price_0_test$price_range_0)

price_1_test <- mobile_data_one_test %>% select(price_range_1)
price_1_test_f <- as.factor(price_1_test$price_range_1)

price_2_test <- mobile_data_one_test %>% select(price_range_2)
price_2_test_f <- as.factor(price_2_test$price_range_2)

price_3_test <- mobile_data_one_test %>% select(price_range_3)
price_3_test_f <- as.factor(price_3_test$price_range_3)

########################################################################
####Code below did not work for the random Forest model but 
###could be used for other applications

#cross validation (?)

#separate X (predictors)
mobile_predictors_train <- mobile_data_one_train %>% select(-(price_range_0:test))

#instantiate each individual train Ys and obtain the vector of the values
price_0_train <- mobile_data_one_train %>% select(price_range_0)
y_0_train <- price_0_train$price_range_0

price_1_train <- mobile_data_one_train %>% select(price_range_1)
y_1_train <- price_1_train$price_range_1

price_2_train <- mobile_data_one_train %>% select(price_range_2)
y_2_train <- price_2_train$price_range_2

price_3_train <- mobile_data_one_train %>% select(price_range_3)
y_3_train <- price_3_train$price_range_3


##########################################################################
#fit the models for each price level
#Random Forest Classifier for price range 0
fit.rndfor_0 <- randomForest(price_range_0 ~.,
                           data = mobile_train_0,
                           importance = TRUE,
                           xtest = mobile_predictors_test,
                           ytest = price_0_test_f)
#Random Forest Classifier for price range 1
fit.rndfor_1 <- randomForest(price_range_1 ~.,
                             data = mobile_train_1,
                             importance=TRUE,
                             xtest = mobile_predictors_test,
                             ytest = price_1_test_f)
#Random Forest Classifier for price range 2
fit.rndfor_2 <- randomForest(price_range_2 ~.,
                             data = mobile_train_2,
                             importance=TRUE,
                             xtest = mobile_predictors_test,
                             ytest = price_2_test_f)
#Random Forest Classifier for price range 3
fit.rndfor_3 <- randomForest(price_range_3 ~.,
                             data = mobile_train_3,
                             importance=TRUE,
                             xtest = mobile_predictors_test,
                             ytest = price_3_test_f)

#Analyze the results
# Price Range 0 train
y_hat_0 <- fit.rndfor_0$predicted
price_0_acc <- Accuracy(y_hat_0,y_0_train)
# Price Range 0 test
y_test_hat_0 <- fit.rndfor_0$test$predicted
price_0_acc_test <- Accuracy(y_test_hat_0,price_0_test$price_range_0)

#Price Range 1 train
y_hat_1 <- fit.rndfor_1$predicted
price_1_acc <- Accuracy(y_hat_1,y_1_train)
# Price Range 1 test
y_test_hat_1 <- fit.rndfor_1$test$predicted
price_1_acc_test <- Accuracy(y_test_hat_1,price_1_test$price_range_1)

#Price Range 2 train
y_hat_2 <- fit.rndfor_2$predicted
price_2_acc <- Accuracy(y_hat_2,y_2_train)
# Price Range 2 test
y_test_hat_2 <- fit.rndfor_2$test$predicted
price_2_acc_test <- Accuracy(y_test_hat_2,price_2_test$price_range_2)

#Price Range 3 train
y_hat_3 <- fit.rndfor_3$predicted
price_3_acc <- Accuracy(y_hat_3,y_3_train)
# Price Range 3 test
y_test_hat_3 <- fit.rndfor_3$test$predicted
price_3_acc_test <- Accuracy(y_test_hat_3,price_3_test$price_range_3)

################################################################################################
#Code below has the purpose of combining all 4 previous models

#Building a model for a prediction with all models
#set up y_test as a 4 level factor
price_levels <- mobile_data_one_test %>% select((price_range_0:price_range_3))
colnames(price_levels) <- c("0","1","2","3")
w <- which(price_levels==1,arr.ind = T)
mobile_data_one_test$price_level <- toupper(names(price_levels)[w[order(w[,1]),2]])
#Add these values into a data table
prediction_dt <- data.table("0" = fit.rndfor_0$test$votes[,2],
                            "1" = fit.rndfor_1$test$votes[,2],
                            "2" = fit.rndfor_2$test$votes[,2],
                            "3" = fit.rndfor_3$test$votes[,2])

label_rf <- apply(prediction_dt,1,which.max)-1
decision_dt <- data.table("predicted values"= label_rf)

decision_dt$actualvalues <- mobile_data_one_test$price_level



#Evaluate decisions
y_test <- as.numeric(decision_dt$actualvalues)
predictions <- as.numeric(decision_dt$`predicted values`)
analysis_table <- table(y_test,predictions)

diag = diag(analysis_table) # number of correctly classified instances per class 
rowsums = apply(analysis_table, 1, sum) # number of instances per class
colsums = apply(analysis_table, 2, sum) # number of predictions per class

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)

df_rf <- data.table(rowsums)
df_rf$pred <- colsums
df_rf$price_range <- c("0",'1','2','3')
colnames(df_rf) <- c('actual','predicted','price_range')
ggplot(data = decision_dt,aes(x=`predicted values`,y=actualvalues,palette(n=4,'Alphabet')))+
  geom_point(aes(jitter(decision_dt$`predicted values`,factor=1,)))+
  theme_bw()

###############################################################
#Boosting
library(gbm)

#gbm needs the target variable to be numeric
#transforming price range to numeric
mobile_train_0b <- mobile_data_one_train %>% select(-(price_range_1:test))

mobile_train_1b <- mobile_data_one_train %>% select(-c(price_range_0,(price_range_2:test)))

mobile_train_2b <- mobile_data_one_train %>% select(-c((price_range_0:price_range_1),(price_range_3:test)))

mobile_train_3b <- mobile_data_one_train %>% select(-c((price_range_0:price_range_2),test))

#fit the models for each price level
#Boosted Classifier for price range 0
fit.boostrndfor_0 <- gbm(price_range_0 ~.,
                         data = mobile_train_0b,
                         distribution = "bernoulli",
                         shrinkage = 0.1)
#Boosted Classifier for price range 1
fit.boostrndfor_1 <- gbm(price_range_1 ~.,
                         data = mobile_train_1b,
                         distribution =  "bernoulli",
                         shrinkage = 0.1)
#Boosted Classifier for price range 2
fit.boostrndfor_2 <- gbm(price_range_2 ~.,
                         data = mobile_train_2b,
                         distribution =  "bernoulli",
                         shrinkage = 0.1)
#Boosted Classifier for price range 3
fit.boostrndfor_3 <- gbm(price_range_3 ~.,
                         data = mobile_train_3b,
                         distribution =  "bernoulli",
                         shrinkage = 0.1)

#Predictions
#price range 0
y_boosted_pred_0 <- predict.gbm(fit.boostrndfor_0,mobile_predictors_test,type = 'response')
boosted.class_0 <- ifelse(y_boosted_pred_0<0.5,'0','1')
boosted_0_acc <- Accuracy(boosted.class_0,price_0_test$price_range_0)
#price range 1
y_boosted_pred_1 <- predict.gbm(fit.boostrndfor_1,mobile_predictors_test,type = 'response')
boosted.class_1 <- ifelse(y_boosted_pred_1<0.5,'0','1')
boosted_1_acc <- Accuracy(boosted.class_1,price_1_test$price_range_1)
#price range 2
y_boosted_pred_2 <- predict.gbm(fit.boostrndfor_2,mobile_predictors_test,type = 'response')
boosted.class_2 <- ifelse(y_boosted_pred_2<0.5,'0','1')
boosted_2_acc <- Accuracy(boosted.class_2,price_2_test$price_range_2)
#price range 3
y_boosted_pred_3 <- predict.gbm(fit.boostrndfor_3,mobile_predictors_test,type = 'response')
boosted.class_3 <- ifelse(y_boosted_pred_3<0.5,'0','1')
boosted_3_acc <- Accuracy(boosted.class_3,price_3_test$price_range_3)

#Evaluating
#Building a model for a prediction with all models

#Add these values into a data table
boost_prediction_dt <- data.table("0" =y_boosted_pred_0,
                            "1" = y_boosted_pred_1,
                            "2" = y_boosted_pred_2,
                            "3" = y_boosted_pred_3)

label_boost <- apply(boost_prediction_dt,1,which.max)-1
boost_decision_dt <- data.table("predicted values"= label_boost)

boost_decision_dt$actualvalues <- mobile_data_one_test$price_level

#Evaluate decisions
predictions_boost <- boost_decision_dt$`predicted values`
analysis_table_boost <- table(y_test,predictions_boost)

diag_boost = diag(analysis_table_boost) # number of correctly classified instances per class 
rowsums_boost = apply(analysis_table_boost, 1, sum) # number of instances per class
colsums_boost = apply(analysis_table_boost, 2, sum) # number of predictions per class

precision_boost = diag_boost / colsums_boost 
recall_boost = diag_boost / rowsums_boost 
f1_boost = 2 * precision_boost * recall_boost / (precision_boost + recall_boost)

df <- data.table(rowsums_boost)
df$pred <- colsums_boost
df$price_range <- c("0",'1','2','3')
colnames(df) <- c('actual','predicted')
ggplot(NULL,aes(x=price_range,y=actual))+
  geom_bar(aes(fill="actual"), data= df, stat = 'identity',position = "dodge",alpha=0.3)+
  geom_bar(aes(y=predicted,fill="predicted"),data=df,stat = 'identity',position = "dodge",alpha = 0.3)+
  ylab("Count of Occurrence")+
  theme_tufte()
library(ggthemes)
  
df_rf <- data.table(rowsums)
df_rf$pred <- colsums
df_rf$price_range <- c("0",'1','2','3')
colnames(df_rf) <- c('actual','predicted','price_range')
ggplot(NULL,aes(x=price_range,y=actual))+
  geom_bar(aes(fill="actual"), data= df_rf, stat = 'identity',position = "dodge",alpha=0.3)+
  geom_bar(aes(y=predicted,fill="predicted"),data=df_rf,stat = 'identity',position = "dodge",alpha = 0.3)+
  ylab("Count of Occurrence")+
  theme_tufte()
