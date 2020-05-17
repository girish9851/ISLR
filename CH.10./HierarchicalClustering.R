set.seed(2)

x=matrix(rnorm(50*2),ncol=2)


x[1:25,1]=x[1:25,1]+3

x[1:25,2]=x[1:25,2]-4

hc.complete = hclust(dist(x),method="complete")
hc.average = hclust(dist(x),method="average")
hc.single = hclust(dist(x),method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="hierarchical clustering with complete linkage",xlab="",ylab="",cex=.9)
plot(hc.average,main="hierarchical clustering with average linkage",xlab="",ylab="",cex=.9)
plot(hc.single,main="hierarchical clustering with single linkage",xlab="",ylab="",cex=.9)

cutree(hc.complete,2)
cutree(hc.average,2)
cutree(hc.single,2)

cutree(hc.single,4)

## scaled variables ##

xsc=scale(x)
plot(hclust(dist(xsc),method="complete"),main="hierarchical clustering with complete linkage and scaled variable",xlab="",ylab="",cex=.9)

## correlation based distance ##

x=matrix(rnorm(30*3),ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd,method="complete"),main="hierarchical clustering with complete linkage and scaled variable with correlation distance",xlab="",ylab="",cex=.9)
