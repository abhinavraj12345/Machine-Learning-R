
data=read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\breast-cancer.csv",header = T)
#--------------------data cleaning--------------------------------
cdata=data[,1:32]
fdata= cdata[,-1]#final data
#---------------------random forest--------------------------------
library(randomForest)
cancer.rf=randomForest(formula=diagnosis~.,data=fdata); varImpPlot(cancer.rf);
D=importance(cancer.rf);  sorted_names=labels(sort(D[,1],decreasing = T));
sorted_data=cbind(fdata$diagnosis,fdata[sorted_names]); colnames(sorted_data)[1]="diagnosis";
#-----------------------------------------------------------------------
CV.random.forest<-function(data,fold){ #other function parameters){
  d=floor(nrow(data)/fold)
  k=1:d
  train<-NULL
  test<-NULL
  accuracy=NULL
  for(i in 1:fold){
    test=data[k,]
    train=data[-k,]
    library(randomForest)
    cancer.rf=randomForest(formula=diagnosis~.,data=train)#include function parameters 
    p=predict(cancer.rf,test)
    accuracy[i]=mean(test$diagnosis==p)
    k=d+k
  }
  return(mean(accuracy))
}
#------------------------accuracy for attributes----------------------------------
k=NULL
for (i in 1:length(sorted_names)/2){
  data1=sorted_data[, 1:((2*i)+1)]
  k[i]=CV.random.forest(data1,5)
}
plot(k,type='b')
final.data=sorted_data[,1:(2*(order(k)[length(k)])+1)]
#--------------------best ntree and mtry------------------------------------------
CV.RF.ntree.mtry<-function(data,fold,a,b){ #other function parameters){
  d=floor(nrow(data)/fold)
  k=1:d
  train<-NULL
  test<-NULL
  accuracy=NULL
  for(i in 1:fold){
    test=data[k,]
    train=data[-k,]
    cancer.rf=randomForest(formula=diagnosis~.,data=train,mtry=a,ntree=b)#include function parameters 
    p=predict(cancer.rf,test)
    accuracy[i]=mean(test$diagnosis==p)
    k=d+k
  }#END:for
  return(mean(accuracy))
}
#-------------------grid points----------------------------------
x<-seq(2,30,by=2)
y<-seq(50,500,by=50)
aa=matrix(NA,nrow=length(x),length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    aa[i,j]=CV.RF.ntree.mtry(final.data,5,x[i],y[j])
  }#END:j
}#END:i
accuracy.matrix=t(aa)
#---------------printing accuracy matrix--------------------------
#print(accuracy.matrix)
l=which(accuracy.matrix == max(accuracy.matrix), arr.ind = TRUE)
cat(sprintf("\n Maximum accuracy is %f for mtry = %d and ntree = %d in 
            cross-validation with subset attributes. \n \n",
            max(accuracy.matrix),x[l[1,2]],y[l[1,1]]))






