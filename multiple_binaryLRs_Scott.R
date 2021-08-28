set.seed(430)
library(data.table)
library(boot)
mo <- fread('~/R/mobile/processed_train.csv')

# train_test split
mo_obs <- nrow(mo)
mo_idx <- sample(mo_obs, size = trunc(0.70 * mo_obs))
mo_trn <- mo[mo_idx, ]
mo_test <- mo[-mo_idx, ]
  
# 4 separate train and test sets
X_train <- mo_trn[,1:20]
Y_train <- mo_trn[,price_range]
Y_train0 <- mo_trn[,p0]
Y_train1 <- mo_trn[,p1]
Y_train2 <- mo_trn[,p2]
Y_train3 <- mo_trn[,p3]
X_train0 <- data.table(X_train)
X_train0[,p0 := Y_train0]
X_train1 <- data.table(X_train)
X_train1[,p1 := Y_train1]
X_train2 <- data.table(X_train)
X_train2[,p2 := Y_train2]
X_train3 <- data.table(X_train)
X_train3[,p3 := Y_train3]

X_test <- mo_test[,1:20]
Y_test <- mo_test[,price_range]
Y_test0 <- mo_test[,p0]
Y_test1 <- mo_test[,p1]
Y_test2 <- mo_test[,p2]
Y_test3 <- mo_test[,p3]
X_test0 <- data.table(X_test)
X_test0[,p0 := Y_test0]
X_test1 <- data.table(X_test)
X_test1[,p1 := Y_test1]
X_test2 <- data.table(X_test)
X_test2[,p2 := Y_test2]
X_test3 <- data.table(X_test)
X_test3[,p3 := Y_test3]

# fitting models
glm.fit0 <- glm(p0 ~ ., data = X_train0, family = binomial)
glm.fit1 <- glm(p1 ~ ., data = X_train1, family = binomial)
glm.fit2 <- glm(p2 ~ ., data = X_train2, family = binomial)
glm.fit3 <- glm(p3 ~ ., data = X_train3, family = binomial) # binomial for logistic regression

# train predictions
Y_train_hat0 <- predict(glm.fit0, newdata = X_train0, type = "response")
Y_train_hat1 <- predict(glm.fit1, newdata = X_train1, type = "response")
Y_train_hat2 <- predict(glm.fit2, newdata = X_train2, type = "response")
Y_train_hat3 <- predict(glm.fit3, newdata = X_train3, type = "response")

Y_train_hat_df <- data.table("0" = Y_train_hat0, "1" = Y_train_hat1, '2' = Y_train_hat2, "3" = Y_train_hat3)

Y_train_hat <- data.table(colnames(Y_train_hat_df)[max.col(Y_train_hat_df,ties.method="first")])
Y_train_hat <- lapply(Y_train_hat[,], as.numeric)

train_accuracy <- mean(Y_train == Y_train_hat$V1)

#test predictions
Y_test_hat0 <- predict(glm.fit0, newdata = X_test0, type = "response")
Y_test_hat1 <- predict(glm.fit1, newdata = X_test1, type = "response")
Y_test_hat2 <- predict(glm.fit2, newdata = X_test2, type = "response")
Y_test_hat3 <- predict(glm.fit3, newdata = X_test3, type = "response")

Y_test_hat_df <- data.table("0" = Y_test_hat0, "1" = Y_test_hat1, '2' = Y_test_hat2, "3" = Y_test_hat3)

Y_test_hat <- data.table(colnames(Y_test_hat_df)[max.col(Y_test_hat_df,ties.method="first")])
Y_test_hat <- lapply(Y_test_hat[,], as.numeric)

test_accuracy <- mean(Y_test == Y_test_hat$V1)

# Evaluation
cm <- table(Y_test, Y_test_hat$V1) 

diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

scores <- data.frame(precision, recall, f1) 
train_accuracy
test_accuracy  
cm 
scores # has trouble distinguishing between 1 and 2
