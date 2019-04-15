library(mlbench)
data("BreastCancer",package = "mlbench")
bc<-BreastCancer[complete.cases(BreastCancer),]
str(bc)
bc=bc[,-1]
for(i in 1:(ncol(bc)-1)) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
bc$Class=ifelse(bc$Class=="malignant",1,0)
bc$Class <- as.factor(bc$Class)
library(caret)
trainDataIndex=createDataPartition(bc$Class,p=0.7,list=F)
trainData=bc[trainDataIndex,]
testData=bc[-trainDataIndex,]
table(trainData$Class)
#-----------------------down sample------------------------------------
down_train=downSample(x=trainData[,-ncol(trainData)],y=trainData$Class)
#-----------------------up sample--------------------------------------
up_train=upSample(x=trainData[,-ncol(trainData)],y=trainData$Class)
#----------------------------------------------------------------------
logitmod<-glm(Class~.,family = "binomial",data=down_train)
summary(logitmod)
pred=predict(logitmod,testData,type="response")
y_pred_num=ifelse(pred>0.5,1,0)
y_pred=as.factor(as.character(y_pred_num))
y_actual=testData$Class
accuracy=mean(y_actual==y_pred)



