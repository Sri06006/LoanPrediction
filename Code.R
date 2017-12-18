rm(list=ls(all=TRUE))
setwd("C:\\Users\\R_files\\AnalyticsVidya\\LoanPrediction")
data = read.csv("train.csv", na.strings = "")
test = read.csv("test.csv", na.strings = "")

str(data)
summary(data)
str(test)
Loan_ID = test[,1]
Loan_ID = as.character(Loan_ID)
data = data[,-1]
test = test[,-1]

str(data$Credit_History)
data1 = data
data1$Credit_History = factor(data1$Credit_History)
str(data1$Credit_History)
data$Credit_History = as.factor(data$Credit_History)
str(test)
test$Credit_History = as.factor(test$Credit_History)

head(data,10)
head(data1,10)
sum(is.na(data))

data = centralImputation(data)
test = centralImputation(test)

library(infotheo)
table(data$Loan_Amount_Term)
loan_term = discretize(data$Loan_Amount_Term, disc = "equalwidth", nbins= 10)
table(loan_term)

tapply(data$Loan_Amount_Term, loan_term, min)
tapply(data$Loan_Amount_Term, loan_term, max)
str(data$Education)
str(data)

#Stratified Sampling
library(caret)
samples = createDataPartition(data$Loan_Status, times=1, p=0.7, list=F)
train = data[samples,]
train_test = data[-samples,]


loan_term_test = discretize(test$Loan_Amount_Term, disc = "equalwidth", nbins=10)
tapply(test$Loan_Amount_Term, loan_term_test, min)
tapply(test$Loan_Amount_Term, loan_term_test, max)
###
# simple rpart

library(rpart)
dtModel = rpart(Loan_Status ~., data = train, method='class')
summary(dtModel)


plot(dtModel, main="Classification", margin=0.15, uniform=TRUE)
text(dtModel,use.n=T)


predictions = predict(dtModel, newdata = train, type="class")
confusionMatrix(train$Loan_Status, predictions)
###
#
# rpart with cp = 0.01
# 

dtModel_cp = rpart(Loan_Status ~., data = train, method='class', control = rpart.control(cp=0.01))
plotcp(dtModel_cp)
printcp(dtModel_cp)

predictions = predict(dtModel_cp, data = train, type='class')
confusionMatrix(train$Loan_Status, predictions)


####################
#
# Random Forest
#
###

library(randomForest)
bankdata_rf = randomForest(Loan_Status~., data=train,keep.forest=TRUE,ntree=30)
print(bankdata_rf)

bankdata_rf$predicted
bankdata_rf$importance
varImpPlot(bankdata_rf)
predictions = predict(bankdata_rf, newdata = train, type = "response")
predictions_test = predict(bankdata_rf, newdata = train_test, type="response")

confusionMatrix(train_test$Loan_Status,predictions_test)

###############
#Evaluation on test data
predictions = predict(bankdata_rf, newdata = test, type="response")

Loan.Status = predictions
Loan_Status = as.character(Loan.Status)


test_final = cbind(Loan_ID, Loan_Status)
View(test_final)
write.csv(test_final, "submissions.csv", row.names=F)

