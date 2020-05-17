set.seed(2)

x=matrix(rnorm(50*2),ncol=2)


x[1:25,1]=x[1:25,1]+3

x[1:25,2]=x[1:25,2]-4

km.out=kmeans(x,2,nstart=20)

km.out$cluster

km.out$tot.withinss

#plot(x,col=(km.out$cluster+1),main="k means clustering with two clusters",xlab='',ylab='',pch=20,cex=2)

km.out1=kmeans(x,3,nstart=20)

km.out1$cluster

km.out1$tot.withinss

plot(x,col=(km.out1$cluster+1),main="k means clustering with three clusters",xlab='',ylab='',pch=20,cex=2)


