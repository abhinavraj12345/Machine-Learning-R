#---------------------- reading data -----------------------------
library(randomForest)
data1=read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\breast-cancer.csv",header = T)
#--------------------data cleaning--------------------------------
cdata=data1[,1:32]
fdata= cdata[,-1]#--------------------------------------final data
data<-fdata
fold=5
#-----------------------------------------------------------------
CV.random.forest<-function(data,fold){ #other function parameters){
  d=floor(nrow(data)/fold)
  k=1:d
  train<-NULL
  test<-NULL
  accuracy=NULL
  for(i in 1:fold){
    test=data[k,]
    train=data[-k,]
    cancer.rf=randomForest(formula=diagnosis~.,data=train)#include function parameters 
    p=predict(cancer.rf,test)
    accuracy[i]=mean(test$diagnosis==p)
    k=d+k
  }
  return(mean(mean(accuracy)))
}