set.seed(430)
library(data.table)
library(boot)
mo <- fread('~/R/mobile/processed_train.csv')



# train_test split
mo_obs <- nrow(mo)
mo_idx <- sample(mo_obs, size = trunc(0.7 * mo_obs))
mo_trn <- mo[mo_idx, ]
mo_test <- mo[-mo_idx, ]

Y_train <- mo_trn[,price_binary]
X_train <- mo_trn[, 1:20]
X_train[,price_binary := Y_train]                  # training set - first binary (low-high) model
X_train01 <- mo_trn[price_range < 2, c(1:20,24) ]  # training set - second layer binary model (0,1) 
X_train23 <- mo_trn[price_range > 1, c(1:20,26) ]  # training set - second layer binary model (2,3)


Y_test <- mo_test[,price_binary]
X_test <- mo_test[, 1:20]
X_test[, price_binary := Y_test]
X_test01 <- mo_test[price_range < 2, c(1:20,24) ]
X_test23 <- mo_test[price_range > 1, c(1:20,26) ]

# fitting models
glm.fit <- glm(price_binary ~ ., data = X_train, family = binomial) # binomial for logistic regression

glm.fit01 <- glm(p1 ~ ., data = X_train01, family = binomial)
glm.fit23 <- glm(p3 ~ ., data = X_train23, family = binomial)

######################
# 1st Prediction Layer

#test predictions
Y_test_hat <- predict(glm.fit, newdata = X_test, type = "response")
Y_test_hat <- data.table(Y_test_hat > 0.5)
Y_test_hat <- transform(Y_test_hat, V1 = as.numeric(V1))[,V1]

binary1_accuracy <- mean(Y_test == Y_test_hat)

x2 <- data.table(mo_test)
x2[, hi_lo_prediction := Y_test_hat]

low_table <- x2[hi_lo_prediction == 0, c(1:20, 24, 21)]
high_table <-  x2[hi_lo_prediction == 1, c(1:20, 26, 21)] # split based on prediction from first layer 

######################
# 2nd Prediction Layer

# if first layer predicted low, second layer predicts 0 or 1
# if first layer predicted high, second layer predicts 2 or 3

#test predictions
Y_test_hat_low <- predict(glm.fit01, newdata = low_table[, 1:21], type = "response")
Y_test_hat_low <- data.table(Y_test_hat_low > 0.5)
Y_test_hat_low <- transform(Y_test_hat_low, V1 = as.numeric(V1))[,V1]

test_accuracy_low <- mean(low_table$price_range == Y_test_hat_low) # price_range column - not used as input, just for checking results

Y_test_hat_high <- predict(glm.fit23, newdata = high_table[, 1:21], type = "response")
Y_test_hat_high <- data.table(Y_test_hat_high > 0.5)
Y_test_hat_high <- Y_test_hat_high + 2
Y_test_hat_high <- transform(Y_test_hat_high, V1 = as.numeric(V1))[,V1]

test_accuracy_high <- mean(high_table$price_range == Y_test_hat_high)  # price_range column - not used as input, just for checking results

# Combining Results
matrix1 <- data.table(low_table)
matrix1[, prediction := Y_test_hat_low]
matrix1[, p1 := NULL]
matrix2 <- data.table(high_table)
matrix2[, prediction := Y_test_hat_high]
matrix2[, p3 := NULL]

results <- rbind(matrix1, matrix2)

# Evaluation

test_accuracy <- mean(results$price_range == results$prediction)

cm <- table(results$price_range, results$prediction) 

diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

scores <- data.frame(precision, recall, f1) 

binary1_accuracy
test_accuracy_low
test_accuracy_high
test_accuracy  #overall accuracy for final output through both layers
cm 
scores 


