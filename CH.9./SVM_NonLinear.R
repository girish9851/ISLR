set.seed(1)

library(e1071)

X = matrix(rnorm(200*2),ncol=2)

X[1:100,]=X[1:100,]+2

X[101:150,]=X[101:150,]-2

Y = c(rep(1,150),rep(2,50))

plot(X,col=Y)

data1=data.frame(X=X,Y=as.factor(Y))

train=sample(200,100)

# lower cost #

svmfit_r = svm(Y~.,data=data1[train,],kernel='radial',gamma=1,cost=1)

summary(svmfit_r)

plot(svmfit_r,data1[train,])

# higher cost #

svmfit_r2 = svm(Y~.,data=data1[train,],kernel='radial',gamma=1,cost=1e5)

summary(svmfit_r2)

plot(svmfit_r2,data1[train,])

# hyperparameter tuning #

tune.out_n = tune(svm,Y~.,data=data1[train,],kernel='radial',ranges=list(gamma=c(.5,1,2,3,4),cost=c(.1,1,10,100,1000)))

# best model on test data #
summary(tune.out_n)

table(true=data1[-train,'Y'],pred=predict(tune.out_n$best.model,newx = data1[-train,]))

# ROC plot

library(ROCR)

rocplot=function(pred,truth,...){
   predob=prediction(pred,truth , label.ordering = c(2, 1))
   perf=performance(predob,'tpr','fpr')
   plot(perf,...) }


svmfit.opt_r=svm(Y~.,data=data1[train,],gamma=.5,cost=1,kernel='radial',decision.values=T)

fitted=attributes(predict(svmfit.opt_r,data1[train,],decision.values = TRUE))$decision.values

par ( mfrow =c(1 ,2) )

rocplot(fitted,data1[train,'Y'],main="training data")

###

svmfit.opt_r1=svm(Y~.,data=data1[train,],gamma=50,cost=1,kernel='radial',decision.values=T)

fitted1=attributes(predict(svmfit.opt_r1,data1[train,],decision.values = TRUE))$decision.values

rocplot(fitted1,data1[train,'Y'],main="training data",add=T,col='red')

### ROC plots on test data 

fittedt1=attributes(predict(svmfit.opt_r,data1[-train,],decision.values = TRUE))$decision.values
fittedt2=attributes(predict(svmfit.opt_r1,data1[-train,],decision.values = TRUE))$decision.values

par ( mfrow =c(1 ,2) )

rocplot(fittedt1,data1[-train,'Y'],main="test data")
rocplot(fittedt2,data1[-train,'Y'],main="test data",add=T,col='red')


#### multiple classes ###

# adding an additional class # 

X = rbind( X , matrix(rnorm(50*2),ncol=2))
Y=c(Y,c(rep(0,50)))
X[Y==0,]=X[Y==0,]+2

data2=data.frame(X=X,Y=Y)

par ( mfrow =c(1 ,1) )

plot(X,col=c(3-Y), main='scatter plot of multiclass simulated data')

svm_fit_multc=svm(Y~.,data=data2,kernel='radial',gamma=1,cost=10)

summary(svm_fit_multc)

par ( mfrow =c(1 ,1) )

plot(svm_fir_multc,data2)

######################### end ############################