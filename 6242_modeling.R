install.packages("e1071")
library(e1071)
install.packages("parallelSVM")
library(parallelSVM)
install.packages("caret", dependencies=TRUE)
library(caret)
library(dplyr)
library(kknn)
library(VIM)

#read in data
train1 <- read.csv("train1.csv",header=TRUE,sep=",")
train2 <- read.csv("train2.csv",header=TRUE,sep=",")
train3 <- read.csv("train3.csv",header=TRUE,sep=",")
train4 <- read.csv("train4.csv",header=TRUE,sep=",")
test <- read.csv("test.csv",header=TRUE,sep=",")

#stack training data
train <- rbind(train1,train2,train3,train4)

#read in data
train_categorized1 <- read.csv("train_categorized1.csv",header=TRUE,sep=",")
train_categorized2 <- read.csv("train_categorized2.csv",header=TRUE,sep=",")
train_categorized3 <- read.csv("train_categorized3.csv",header=TRUE,sep=",")
train_categorized4 <- read.csv("train_categorized4.csv",header=TRUE,sep=",")
test_categorized <- read.csv("test_categorized.csv",header=TRUE,sep=",")

#stack training data
train_categorized <- rbind(train_categorized1,train_categorized2,train_categorized3,train_categorized4)

#balance training data
sub1 = train_categorized[train_categorized$CERTIFIED== 0 & train_categorized$STATE == 'STATES_GROUP_1',  c("SOC_NAME","PREVAILING_WAGE","STATE","CERTIFIED","FULL_TIME")]
sub2 = train_categorized[train_categorized$CERTIFIED== 0 & train_categorized$STATE == 'STATES_GROUP_2',  c("SOC_NAME","PREVAILING_WAGE","STATE","CERTIFIED","FULL_TIME")]
sub3 = train_categorized[train_categorized$CERTIFIED== 1 & train_categorized$STATE == 'STATES_GROUP_1',  c("SOC_NAME","PREVAILING_WAGE","STATE","CERTIFIED","FULL_TIME")]
sub4 = train_categorized[train_categorized$CERTIFIED== 1 & train_categorized$STATE == 'STATES_GROUP_2',  c("SOC_NAME","PREVAILING_WAGE","STATE","CERTIFIED","FULL_TIME")]

sub1_new = sub1[sample(nrow(sub1),16710),]
sub2_new = sub2
sub3_new = sub3[sample(nrow(sub3),16710),]
sub4_new = sub4[sample(nrow(sub4),16710),]

train_balanced_categorized = rbind(sub1_new,sub2_new,sub3_new,sub4_new)

train_balanced_categorized$SOC_NAME <- as.factor(train_balanced_categorized$SOC_NAME)
train_balanced_categorized$STATE <- as.factor(train_balanced_categorized$STATE)

test_categorized$SOC_NAME <- as.factor(test_categorized$SOC_NAME)
test_categorized$STATE <- as.factor(test_categorized$STATE)

#get 1/8th of the rows
train_new = train[sample(nrow(train),nrow(train)/8),]
train_categorized_new = train_categorized[sample(nrow(train_categorized),nrow(train_categorized)/8),]
train_balanced_categorized_new = train_balanced_categorized[sample(nrow(train_balanced_categorized),nrow(train_balanced_categorized)/8),]

#logistic regression 
model_1 <- glm(CERTIFIED ~ STATE + SOC_NAME + FULL_TIME + PREVAILING_WAGE, data=train_new, family="binomial")
lr_prob <- predict(model_1, test, type="response")
lr_pred <- ifelse(lr_prob > 0.50, 1, 0)
test_categorized$logistic_regression1 <- lr_pred

#logistic regression (categorized)
model_2 <- glm(CERTIFIED ~ STATE + SOC_NAME + FULL_TIME + PREVAILING_WAGE, data=train_categorized_new, family="binomial")
lr_prob <- predict(model_2, test_categorized, type="response")
lr_pred <- ifelse(lr_prob > 0.50, 1, 0)
test_categorized$logistic_regression2 <- lr_pred

#logistic regression (categorized and balanced)
model_3 <- glm(CERTIFIED ~ STATE + SOC_NAME + FULL_TIME + PREVAILING_WAGE, data=train_balanced_categorized_new, family="binomial")
lr_prob <- predict(model_3, test_categorized, type="response")
lr_pred <- ifelse(lr_prob > 0.50, 1, 0)
test_categorized$logistic_regression3 <- lr_pred
                          
#svm 
model_4 <- parallelSVM(CERTIFIED ~ STATE + SOC_NAME + FULL_TIME + PREVAILING_WAGE, data=train_new, numberCores = 8, samplingSize = 0.2)
svm_prob <- predict(model_4, test, type="response")
svm_pred <- ifelse(svm_prob > 0.50, 1, 0)
test_categorized$svm1 <- svm_prob

#svm (categorized)
model_5 <- parallelSVM(CERTIFIED ~ STATE + SOC_NAME + FULL_TIME + PREVAILING_WAGE, data=train_categorized_new, numberCores = 8, samplingSize = 0.2)
svm_prob <- predict(model_5, test_categorized, type="response")
svm_pred <- ifelse(svm_prob > 0.50, 1, 0)
test_categorized$svm2 <- svm_prob

#svm (categorized and balanced)
model_6 <- parallelSVM(CERTIFIED ~ STATE + SOC_NAME + FULL_TIME + PREVAILING_WAGE, data=train_balanced_categorized_new, numberCores = 8, samplingSize = 0.2)
svm_prob <- predict(model_6, test_categorized, type="response")
svm_pred <- ifelse(svm_prob > 0.50, 1, 0)
test_categorized$svm3 <- svm_prob

#knn
model_7 = kknn(CERTIFIED ~ STATE + SOC_NAME + FULL_TIME + PREVAILING_WAGE,train_new,test,distance = 2, k = 100, scale = TRUE)
knn_prob <- fitted.values(model_7)
knn_pred <- ifelse(knn_prob > 0.50, 1, 0)
test_categorized$knn1 <- knn_pred

#knn (categorized)
model_8 = kknn(CERTIFIED ~ STATE + SOC_NAME + FULL_TIME + PREVAILING_WAGE, train_categorized_new, test_categorized, distance = 2, k = 100, scale = TRUE)
knn_prob <- fitted.values(model_8)
knn_pred <- ifelse(knn_prob > 0.50, 1, 0)
test_categorized$knn2 <- knn_pred

#knn (categorized and balanced)
model_9 = kknn(CERTIFIED ~ STATE + SOC_NAME + FULL_TIME + PREVAILING_WAGE, train_balanced_categorized_new, test_categorized, distance = 2, k = 100, scale = TRUE)
knn_prob <- fitted.values(model_9)
knn_pred <- ifelse(knn_prob > 0.50, 1, 0)
test_categorized$knn3 <- knn_pred

write.csv(test_categorized,"results.csv")

