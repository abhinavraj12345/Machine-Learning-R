library(party)
str(iris)
dim(iris)
set.seed(1234)
ind<-sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
trainData<-iris[ind==1,]
dim(trainData)
testData<-iris[ind==2,]
dim(testdata)
myFormula<-Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
iris_ctree<-ctree(myFormula,data=trainData)
train_predict<-predict(iris_ctree)
table(train_predict,trainData$Species)
mean(train_predict != trainData$Species)*100
#------------------------------------------------------------------------------
testPred<-predict(iris_ctree,newdata=testData)
table(testPred,testData$Species)
mean(testPred != testData$Species)*100
#------------------------------------------------------------------------------
predict(iris_ctree,testData)
predict(iris_ctree,testData,type="prob")
plot(iris_ctree,type="simple")
plot(iris_ctree)

