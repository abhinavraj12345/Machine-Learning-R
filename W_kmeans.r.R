require("datasets")
data("iris")  #load iris datasets
str(iris)     #view structure of dataset
summary(iris) #view statistical summary of 
head(iris)    #view top rows of dataset
iris.new<-iris[,c(1,2,3,4)]
iris.class<-iris[,"Species"]
head(iris.new)
head(iris.class)
#----------------------------------------------------------------------------------------
normalize<-function(x){
  return( (x-min(x)) / (max(x)-min(x)) )
}
#----------------------------------------------------------------------------------------
iris.new$Sepal.Length<-normalize(iris.new$Sepal.Length)
iris.new$Petal.Length<-normalize(iris.new$Petal.Length)
iris.new$Petal.Width<-normalize(iris.new$Petal.Width)
iris.new$Sepal.Width<-normalize(iris.new$Sepal.Width)
result<-kmeans(iris.new,3)    #apply k-means algorithm with no. of centroids(k)=3
#-------------------------plotting-------------------------------------------------------
par(mfrow=c(2,2),mar=c(5,4,2,2))
plot(iris.new[c(1,2)],col=result$cluster)
plot(iris.new[c(1,2)],col=iris.class)
plot(iris.new[c(3,4)],col=result$cluster)
plot(iris.new[c(3,4)],col=iris.class)
table(result$cluster,iris.class)#Accuracy =133/(133+17)=0.88




