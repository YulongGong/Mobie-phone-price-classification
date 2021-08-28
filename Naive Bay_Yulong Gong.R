install.packages(c('tidyverse','caret','caretEnsemble','psych','Amelia','mice','GGally','rpart','randomForest'))

library(tidyverse)
library(ggplot2)
library(caret)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)
library(data.table)

# import the data
data <- fread('/Users/yulong/Desktop/BA810/train.csv')

str(data)
describe(data)

# price high_low
data[,result:=0]

# 0,1,2, low
# 3 high
data[price_range == 3, result:=1]

# drop original
data[,price_range:=NULL]

# build the model 

# split the train & test. 20 as test, 80 as train.
data[,test:=0]
data[sample(nrow(data),1600),test := 1]

Train <- data[test==1]
Test <- data[test==0]

Train[,test:= NULL]
Test[,test:=NULL]

# Check the distribution of the result
prop.table(table(Train$result))

# split the feature. 

x = Train[,-21]
y = factor(Train$result)

# Naive Bay
install.packages('e1071')
library(e1071)

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

# predict
predict<- predict(model, newdata = Test)

pre <- ifelse(predict > 0.5, 1,0)

confusionMatrix(factor(preds), factor(Test$result))
