library(ISLR)

library(e1071)

names(Khan)

table(Khan$ytrain)  # number of classes 


data_train=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain)) ## important factor input for classification here ##
  
svm_gene= svm(y~.,data=data_train,kernel='linear',cost=10)  

summary(svm_gene)

table(svm_gene$fitted,data_train$y)

### test confusion matric ###

data_test=data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))

pred_svm_gene = predict(svm_gene,newdata=data_test)  # important # pay attention whole matrix is given as input instead of just x variables

table(pred_svm_gene,data_test$y)
