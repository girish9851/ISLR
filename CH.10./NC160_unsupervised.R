library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

#View(nci.data)

# applying pca first #

pr.out=prcomp((nci.data),scale=TRUE)

vec=nci.labs
cols = rainbow(length(unique (vec )))
cols_vec = cols[as.numeric (as.factor ( vec ))]


# applying k-means clustering now #

# unscaled

nci.data_s=scale(nci.data)
km.out_nci=kmeans(nci.data,4,nstart=20)
km.out_nci_s=kmeans(nci.data_s,4,nstart=20)

vec1=km.out_nci$cluster
cols1 = rainbow(length(unique (vec1 )))
cols_vec1 = cols1[as.numeric (as.factor ( vec1 ))]

vec2=km.out_nci_s$cluster
cols2 = rainbow(length(unique (vec2 )))
cols_vec2 = cols2[as.numeric (as.factor ( vec2 ))]

par(mfrow=c(1,3))

plot(pr.out$x [ ,c(1,2)] , col =cols_vec1, pch =19,
     xlab ="Z1", ylab =" Z2",main="kmeans clustering plotted on PC1 and PC2")

text(pr.out$x [ ,c(1,2)], labels=nci.labs, cex=0.5, font=1)

plot(pr.out$x [ ,c(1,2)] , col =cols_vec2, pch =19,
     xlab ="Z1", ylab =" Z2",main="kmeans clustering scaled plotted on PC1 and PC2")

text(pr.out$x [ ,c(1,2)], labels=nci.labs, cex=0.5, font=1)

plot(pr.out$x [ ,c(1,2)] , col =cols_vec, pch =19,
     xlab ="Z1", ylab =" Z2",main="original data on PC1 and PC2 ")
text(pr.out$x [ ,c(1,2)], labels=nci.labs, cex=0.5, font=1)


# applying hierarchical clustering now 

hc.nci_c=hclust(dist(nci.data_s),method='complete')
hc.nci_s=hclust(dist(nci.data_s),method='single')
hc.nci_a=hclust(dist(nci.data_S),method='average')

par(mfrow=c(1,2))
plot(hc.nci_c,main="hierarchical clustering with complete linkage",xlab="",ylab="",cex=.9)
plot(hc.nci_s,main="hierarchical clustering with average linkage",xlab="",ylab="",cex=.9)
#plot(hc.nci_a,main="hierarchical clustering with single linkage",xlab="",ylab="",cex=.9)

hc_cluster_c4=cutree(hc.nci_c,4)
hc_cluster_s4=cutree(hc.nci_s,4)

vec3=hc_cluster_c4
cols3 = rainbow(length(unique (vec3 )))
cols_vec3 = cols3[as.numeric (as.factor ( vec3 ))]

vec3=hc_cluster_s4
cols3 = rainbow(length(unique (vec3 )))
cols_vec4 = cols3[as.numeric (as.factor ( vec3 ))]

par(mfrow=c(1,2))

plot(pr.out$x [ ,c(1,2)] , col =cols_vec3, pch =19,
     xlab ="Z1", ylab =" Z2",main="hierarchical clustering complete plotted on PC1 and PC2")

text(pr.out$x [ ,c(1,2)], labels=nci.labs, cex=0.5, font=1)

plot(pr.out$x [ ,c(1,2)] , col =cols_vec4, pch =19,
     xlab ="Z1", ylab =" Z2",main="hierarchical clustering single plotted on PC1 and PC2")

text(pr.out$x [ ,c(1,2)], labels=nci.labs, cex=0.5, font=1)

