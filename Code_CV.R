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

data$Credit_History = as.factor(data$Credit_History)
str(test)
test$Credit_History = as.factor(test$Credit_History)

head(data,10)

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

loan_term_test = discretize(test$Loan_Amount_Term, disc = "equalwidth", nbins=10)
tapply(test$Loan_Amount_Term, loan_term_test, min)
tapply(test$Loan_Amount_Term, loan_term_test, max)


#K - fold cross validation

k= 10

data$id = sample(1:k, nrow(data), replace=TRUE)


list = 1:k

#initializing predictions and testsetcopy as data frame
prediction <- data.frame()
testsetCopy <- data.frame()

#Creating a progress bar to know the status of CV

library(plyr)
library(dplyr)
library(randomForest)
progress.cv = create_progress_bar("text")
progress.cv$init(k)

#function for k fold
for(i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  
  bankdata_rf = randomForest(Loan_Status~., data=trainingset,ntree=100)
  
  #bankdata_rf$predicted
  #bankdata_rf$importance
  #varImpPlot(bankdata_rf)
  predictions = predict(bankdata_rf, newdata = trainingset, type = "response")
  temp = as.data.frame(predict(bankdata_rf, newdata = testset, type="response"))
  prediction = rbind(prediction,temp)
  
  testsetCopy = rbind(testsetCopy, as.data.frame(testset$Loan_Status))
  
  progress.cv$step()
}


results = cbind(prediction, testsetCopy)



names(results) = c("Prediction", "Actual")

write.csv(results, "submissions.csv", row.names=F)

confusionMatrix(results)
