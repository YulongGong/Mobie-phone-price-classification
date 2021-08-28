
set.seed(430)
library(data.table)
library(boot)
mo <- fread('~/R/mobile/processed_train.csv')

# train_test split
mo_obs <- nrow(mo)
mo_idx <- sample(mo_obs, size = trunc(0.70 * mo_obs))
mo_trn <- mo[mo_idx, ]
mo_test <- mo[-mo_idx, ]

Y_train <- mo_trn[,price_binary]
X_train <- mo_trn[,1:20]
X_train[,price_binary := Y_train]

Y_test <- mo_test[,price_binary]
X_test <- mo_test[,1:20]
X_test[, price_binary := Y_test]

# fitting model
glm.fit <- glm(price_binary ~ ., data = X_train, family = binomial) # binomial for logistic regression

# train predictions
Y_train_hat <- predict(glm.fit, newdata = X_train, type = "response")
Y_train_hat <- data.table(Y_train_hat > 0.5)
Y_train_hat <- transform(Y_train_hat, V1 = as.numeric(V1))[,V1]

train_accuracy <- mean(Y_train == Y_train_hat)

#test predictions
Y_test_hat <- predict(glm.fit, newdata = X_test, type = "response")
Y_test_hat <- data.table(Y_test_hat > 0.5)
Y_test_hat <- transform(Y_test_hat, V1 = as.numeric(V1))[,V1]

test_accuracy <- mean(Y_test == Y_test_hat)



# Evaluation:
cm <- table(Y_test, Y_test_hat) # confusion matrix


diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

scores <- data.frame(precision, recall, f1) 

cv.err <- cv.glm(X_train ,glm.fit, K = 10)

train_accuracy
test_accuracy  # 98% accuracy!
cm 
scores
1- cv.err$delta[1]