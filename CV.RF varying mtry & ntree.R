#---------------------- reading data -----------------------------
library(randomForest)
data1=read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\breast-cancer.csv",header = T)
#--------------------data cleaning--------------------------------
cdata=data1[,1:32]
fdata= cdata[,-1]#--------------------------------------final data
data<-fdata
fold=5
#-----------------cross validation random forest-------------------
CV.random.forest<-function(data,fold,a,b){ #other function parameters){
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
    aa[i,j]=CV.random.forest(data,fold,x[i],y[j])
  }#END:j
}#END:i
accuracy.matrix=t(aa)
#---------------printing accuracy matrix--------------------------
print(accuracy.matrix)
l=which(accuracy.matrix == max(accuracy.matrix), arr.ind = TRUE)
cat(sprintf("\n Maximum accuracy is %f for mtry = %d and ntree = %d in cross-validation. \n \n",
            max(accuracy.matrix),x[l[1,2]],y[l[1,1]]))



