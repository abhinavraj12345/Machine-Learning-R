require(e1071)
data=iris
ind<-sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
train=data[ind==1,]
test=data[ind==2,]
levels(train$Species)
model= naiveBayes(Species~.,data=train)
class(model)
pred=predict(model,test)
confusion_matrix=table(test$Species,pred)
print(confusion_matrix)
accuracy=sum(test$Species==pred)/length(pred)
print(accuracy)
