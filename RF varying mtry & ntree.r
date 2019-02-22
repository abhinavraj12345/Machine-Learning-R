data=read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\breast-cancer.csv",header = T)
#--------------------data cleaning--------------------------------
cdata=data[,1:32]
fdata= cdata[,-1]#final data
v=1: floor(nrow(fdata)*0.8) 
train=fdata[v,]
test=fdata[-v,]
#------------------------------------------------------------------

x<-seq(2,30,by=2)
y<-seq(50,500,by=50)
aa=matrix(NA,nrow=length(x),length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    cancer.rf=randomForest(formula=diagnosis~.,data=train,mtry=x[i],ntree=y[j])#include function parameters 
    p=predict(cancer.rf,test)
    aa[i,j]=mean(test$diagnosis==p)
  }#END:j
}#END:i
accuracy.matrix=t(aa)
#---------------printing accuracy matrix--------------------------
#print(accuracy.matrix)
l=which(accuracy.matrix == max(accuracy.matrix), arr.ind = TRUE)
cat(sprintf("\n Maximum accuracy is %f for mtry = %d and ntree = %d without cross-validation. \n \n",
            max(accuracy.matrix),x[l[1,2]],y[l[1,1]]))
