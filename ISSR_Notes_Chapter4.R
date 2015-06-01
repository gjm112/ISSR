

#This data is created in Chapter 2 code.  
##############
load("/Users/gregorymatthews/Dropbox/presTDM.RData")



##########################################
#Principal Components
##########################################
#Principal Component Analysis 
corr<-cor(as.matrix(presTDM))
evv<-eigen(corr)
pcs <- evv$vectors[,1:2]
##values:a vector containing the p eigenvalues of x, sorted in decreasing order
evals <- evv$values[1:2]
temp<-diag(1/sqrt(evals)) %*% t(pcs)%*%t(as.matrix(presTDM))
PCs<-t(as.matrix(presTDM))%*%t(temp)


plot(PCs[,1],PCs[,2],main="PC1 vs PC2: Pres Inaugural Speeches",xlab='PC1',ylab='PC2')
text(PCs[,1],PCs[,2],substring(presTDM$dimnames$Docs,1,6))
  
##########################################
#K-means Clustering
##########################################
#Term frequency - Inverse document frequency
  m<-weightTfIdf(presTDM)
d<-proxy::dist(as.matrix(t(m)), method = "eJaccard")
cl <- kmeans(d, 3)
table(cl$cluster)

plot(PCs[,1],PCs[,2],main="PC1 vs PC2: k-means clustering",col="white")
points(PCs[,1],PCs[,2],col=cl$cluster,cex=3,lwd=3)
text(PCs[,1],PCs[,2],substring(presTDM$dimnames$Docs,1,6),col="gray50")


##########################################
#Hierarchical Clustering
##########################################
m<-weightTfIdf(presTDM)
d<-proxy::dist(as.matrix(t(m)), method = "eJaccard")
hc <- hclust(d, method="average")
plot(hc)







