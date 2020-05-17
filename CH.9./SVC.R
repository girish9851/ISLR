set.seed(1)


x=matrix(rnorm(20*2),ncol=2)

y=c(rep(-1,10),rep(1,10))

  
x[y==1,]=x[y==1,]+1

plot(x[,1],x[,2],col=3-y)

data1=data.frame(x=x,y=as.factor(y))

library(e1071)
  
svm.fit1=svm(y~.,data=data1,kernel='linear',cost=10,scale=FALSE)

summary(svm.fit1)

plot(svm.fit1, data1)


# hyper parameter tuning


set.seed(1)
tune.out = tune(svm,y~.,data=data1,kernel='linear',ranges = list(cost=c(0.001,.01,.1,1,5,10,100)))

summary(tune.out)

bestmod =tune.out$best.model

summary(bestmod)


